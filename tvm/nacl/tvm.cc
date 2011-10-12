/// @file tvm.cc
/// This example demonstrates loading, running and scripting a very simple NaCl
/// module.  To load the NaCl module, the browser first looks for the
/// CreateModule() factory method (at the end of this file).  It calls
/// CreateModule() once to load the module code from your .nexe.  After the
/// .nexe code is loaded, CreateModule() is not called again.
///
/// Once the .nexe code is loaded, the browser than calls the CreateInstance()
/// method on the object returned by CreateModule().  It calls CreateInstance()
/// each time it encounters an <embed> tag that references your NaCl module.
///
/// The browser can talk to your NaCl module via the postMessage() Javascript
/// function.  When you call postMessage() on your NaCl module from the browser,
/// this becomes a call to the HandleMessage() method of your pp::Instance
/// subclass.  You can send messages back to the browser by calling the
/// PostMessage() method on your pp::Instance.  Note that these two methods
/// (postMessage() in Javascript and PostMessage() in C++) are asynchronous.
/// This means they return immediately - there is no waiting for the message
/// to be handled.  This has implications in your program design, particularly
/// when mutating property values that are exposed to both the browser and the
/// NaCl module.

#include <cstdio>
#include <string>
#include "ppapi/c/ppb_instance.h"
#include "ppapi/c/ppb_messaging.h"
#include "ppapi/cpp/completion_callback.h"
#include "ppapi/cpp/instance.h"
#include "ppapi/cpp/module.h"
#include "ppapi/cpp/var.h"

#include "tvm_nacl.h"

#define STATE_STOPPED 0
#define STATE_RUNNING 1
#define STATE_FINISHED 2

class MessageBuffer;
class MessageBuffer {
	public:
		MessageBuffer *next; 
		std::string *message;
		
		explicit MessageBuffer(const std::string source)
		{
			next = NULL;
			message = new std::string(source);
		}
		explicit MessageBuffer(const char *source)
		{
			next = NULL;
			message = new std::string(source);
		}
		virtual ~MessageBuffer()
		{
			delete message;
		}
};

/// The Instance class.  One of these exists for each instance of your NaCl
/// module on the web page.  The browser will ask the Module object to create
/// a new Instance for each occurence of the <embed> tag that has these
/// attributes:
///     type="application/x-nacl"
///     src="tvm.nmf"
/// To communicate with the browser, you must override HandleMessage() for
/// receiving messages from the borwser, and use PostMessage() to send messages
/// back to the browser.  Note that this interface is entirely asynchronous.
class TVMInstance : public pp::Instance {
	private:
		pp::Core *core;
		tvm_instance_t *tvm;
		volatile int state;
		pthread_t tvm_thread;
		mutable pthread_mutex_t tvm_mutex;
		mutable pthread_mutex_t msg_mutex;
		volatile MessageBuffer *msg_queue_head;
		volatile MessageBuffer *msg_queue_tail;
		pp::CompletionCallbackFactory<TVMInstance> cc_factory;

		void ReleaseAndClean()
		{
			if(tvm) {
				tvm_free_instance(tvm);
				tvm = NULL;
			}
		}

		static void *TVMThread(void *param)
		{
			TVMInstance *instance = static_cast<TVMInstance *>(param);
			tvm_instance_t *tvm = instance->tvm;
			
			fprintf (stderr, "running\n");
			instance->ExternalPostMessage("state:running");

			int ret = tvm_run_instance(tvm);
			if (ret != 0) {
				std::string msg = std::string("error:");
				msg.append(tvm->last_error);
				instance->ExternalPostMessage(msg);
			}

			instance->ExternalPostMessage("state:finished");
			fprintf (stderr, "stopped (%d)\n", ret);

			pthread_mutex_lock(&(instance->tvm_mutex));
			instance->state = STATE_FINISHED;
			pthread_mutex_unlock(&(instance->tvm_mutex));

			return NULL;
		}

		void DispatchMessages(int32_t result)
		{
			bool pending;
			do {
				pending = false;

				pthread_mutex_lock(&msg_mutex);
				if (msg_queue_head != NULL) {
					MessageBuffer *buffer = (MessageBuffer *) msg_queue_head;
					if ((msg_queue_head = buffer->next) == NULL) {
						msg_queue_tail = NULL;
					} else {
						pending = true;
					}
					PostMessage(*buffer->message);
					delete buffer;
				}
				pthread_mutex_unlock(&msg_mutex);
			} while (pending);
			
			if (state == STATE_RUNNING) {
				pp::CompletionCallback cc 
					= cc_factory.NewCallback(&TVMInstance::DispatchMessages);
				core->CallOnMainThread(10, cc, 0);
			}
		}

		void ExternalPostMessage(MessageBuffer *buffer)
		{	
			pthread_mutex_lock(&msg_mutex);
			msg_queue_tail = buffer;
			if (msg_queue_head == NULL) {
				msg_queue_head = buffer;
			} else {
				msg_queue_head->next = buffer;
			}
			pthread_mutex_unlock(&msg_mutex);
		}
		void ExternalPostMessage(const std::string message)
		{
			ExternalPostMessage(new MessageBuffer(message));
		}
		void ExternalPostMessage(const char *message)
		{
			ExternalPostMessage(new MessageBuffer(message));
		}

		void StartTVM()
		{
			pthread_mutex_lock(&tvm_mutex);
			pthread_create(&tvm_thread, NULL, TVMThread, this);
			state = STATE_RUNNING;
			pthread_mutex_unlock(&tvm_mutex);

			DispatchMessages(0);
		}

		void StopTVM()
		{
			pthread_mutex_lock(&tvm_mutex);
			if (state != STATE_STOPPED) {
				tvm->stop = 1;
				pthread_join(tvm_thread, NULL);
				state = STATE_STOPPED;
				ExternalPostMessage("state:stopped");
				DispatchMessages(0);
			}
			pthread_mutex_unlock(&tvm_mutex);
		}
	public:
		/// The constructor creates the plugin-side instance.
		/// @param[in] instance the handle to the browser-side plugin instance.
		explicit TVMInstance(PP_Instance instance, pp::Core *core_) : 
			pp::Instance(instance), cc_factory(this)
		{
			core = core_;
			pthread_mutex_init(&tvm_mutex, NULL);
			pthread_mutex_init(&msg_mutex, NULL);
			tvm = NULL;
			state = STATE_STOPPED;
		}
		virtual ~TVMInstance()
		{
			ReleaseAndClean();
		}

		/// Handler for messages coming in from the browser via postMessage().  The
		/// @a var_message can contain anything: a JSON string; a string that encodes
		/// method names and arguments; etc.  For example, you could use
		/// JSON.stringify in the browser to create a message that contains a method
		/// name and some parameters, something like this:
		///   var json_message = JSON.stringify({ "myMethod" : "3.14159" });
		///   nacl_module.postMessage(json_message);
		/// On receipt of this message in @a var_message, you could parse the JSON to
		/// retrieve the method name, match it to a function call, and then call it
		/// with the parameter.
		/// @param[in] var_message The message posted by the browser.
		virtual void HandleMessage(const pp::Var& var_message) {
			if (!var_message.is_string()) {
				return;
			}

			std::string message = var_message.AsString();
			if (message.find("bytecode:") == 0) {
				if (state != STATE_STOPPED) {
					PostMessage(pp::Var("error:can only load bytecode when stopped"));
					return;
				}
				
				uint8_t *data = (uint8_t *) malloc(message.size());
				size_t data_len = 0;
				int ret;

				ReleaseAndClean();
				
				data_len = tvm_base64_decode(message.c_str() + 9, data);
				tvm = tvm_alloc_instance();
				ret = tvm_load_bytecode(tvm, data, data_len);
				free (data);

				if (ret == 0) {
					PostMessage(pp::Var("state:bytecode loaded"));
				} else {
					ReleaseAndClean();
					PostMessage(pp::Var("error:invalid bytecode"));
				}
			} else if (message.find("start") == 0) {
				if (!tvm) {
					PostMessage(pp::Var("error:no bytecode specified"));
				} else if (state != STATE_STOPPED) {
					PostMessage(pp::Var("error:already running"));
				} else {
					StartTVM();
				}
			} else if (message.find("stop") == 0) {
				if (state != STATE_STOPPED) {
					StopTVM();
				} else {
					PostMessage(pp::Var("error:not running"));
				}
			}
		}
};

/// The Module class.  The browser calls the CreateInstance() method to create
/// an instance of your NaCl module on the web page.  The browser creates a new
/// instance for each <embed> tag with type="application/x-nacl".
class TVMModule : public pp::Module {
	public:
		TVMModule() : pp::Module() {}
		virtual ~TVMModule() {}

		/// Create and return a TvmInstance object.
		/// @param[in] instance The browser-side instance.
		/// @return the plugin-side instance.
		virtual pp::Instance* CreateInstance(PP_Instance instance) {
			return new TVMInstance(instance, core());
		}
};

namespace pp {
/// Factory function called by the browser when the module is first loaded.
/// The browser keeps a singleton of this module.  It calls the
/// CreateInstance() method on the object you return to make instances.  There
/// is one instance per <embed> tag on the page.  This is the main binding
/// point for your NaCl module with the browser.
	Module* CreateModule() {
		return new TVMModule();
	}
}  // namespace pp
