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
		
		volatile uint8_t *kyb_buffer;
		volatile int kyb_buf_size;
		volatile int kyb_buf_start;
		volatile int kyb_buf_end;
		mutable pthread_mutex_t kyb_mutex;

		void ReleaseAndClean()
		{
			if (tvm) {
				tvm_free_instance(tvm);
				tvm = NULL;
			}
			if (kyb_buffer) {
				free ((void *) kyb_buffer);
			}
			kyb_buffer	= NULL;
			kyb_buf_start	= 0;
			kyb_buf_end	= 0;
			kyb_buf_size	= 0;
		}

		static int ReadChar(tvm_instance_t *tvm)
		{
			TVMInstance *instance = static_cast<TVMInstance *>(tvm->handle);

			/* Do a quick test without locking, even if this gives an
			 * undefined results repeated polling should clean things up
			 */
			if (instance->kyb_buf_start == instance->kyb_buf_end) {
				return -1;
			}

			/* Looks like there is something there; take lock and retest */
			int result = -1;
			
			pthread_mutex_lock(&(instance->kyb_mutex));
			/*
			fprintf (stderr, "%p %d %d %d\n",
				instance->kyb_buffer,
				instance->kyb_buf_start, instance->kyb_buf_end,
				instance->kyb_buf_size);
			*/

			if (instance->kyb_buf_start != instance->kyb_buf_end) {
				result = instance->kyb_buffer[instance->kyb_buf_start];
				instance->kyb_buf_start = (instance->kyb_buf_start + 1)
								% instance->kyb_buf_size;
				if (instance->kyb_buf_start == instance->kyb_buf_end) {
					instance->kyb_buf_start = instance->kyb_buf_end = 0;
				}
			}
			
			pthread_mutex_unlock(&(instance->kyb_mutex));
			
			return result;
		}

		void QueueChar(uint8_t b)
		{
			bool enlarge = false;

			pthread_mutex_lock(&kyb_mutex);
			/*
			fprintf (stderr, "%p %d %d %d\n",
				kyb_buffer,
				kyb_buf_start, kyb_buf_end,
				kyb_buf_size);
			*/

			if (kyb_buf_start < kyb_buf_end) {
				enlarge = ((kyb_buf_end - kyb_buf_start) >= kyb_buf_size);
			} else if (kyb_buf_start > kyb_buf_end) {
				enlarge = ((kyb_buf_end + (kyb_buf_size - kyb_buf_start)))
						>= kyb_buf_size;
			} else if (!kyb_buffer) {
				enlarge = true;
			} else {
				/* do nothing */
			}
				
			if (enlarge) {
				int n_buf_size = (kyb_buf_size * 2) + 128;
				uint8_t *n_buffer = (uint8_t *) malloc(n_buf_size);
				
				if (kyb_buf_start < kyb_buf_end) {
					memcpy(n_buffer, 
						((uint8_t *)kyb_buffer) + kyb_buf_start,
						kyb_buf_end - kyb_buf_start);
					kyb_buf_end = kyb_buf_end - kyb_buf_start;
					kyb_buf_start = 0;
				} else if (kyb_buf_start > kyb_buf_end) {
					int p0 = (kyb_buf_size - kyb_buf_start);
					memcpy(n_buffer +  0, 
						((uint8_t *)kyb_buffer) + kyb_buf_start, p0);
					memcpy(n_buffer + p0,
						((uint8_t *)kyb_buffer), kyb_buf_end);
					kyb_buf_end = p0 + kyb_buf_end;
					kyb_buf_start = 0;
				}

				if (kyb_buffer)
					free((void *)kyb_buffer);
				kyb_buffer = n_buffer;
				kyb_buf_size = n_buf_size;
			}

			kyb_buffer[kyb_buf_end] = b;
			kyb_buf_end = (kyb_buf_end + 1) % kyb_buf_size;
			
			pthread_mutex_unlock(&kyb_mutex);
		}

		static void WriteScreen(tvm_instance_t *tvm, const char *data, int length)
		{
			TVMInstance *instance = static_cast<TVMInstance *>(tvm->handle);
			
			if (length > 0) {
				unsigned int len = 32 + (length * 4);
				unsigned int pos = 0;
				char *buf = new char[len];
				int i;

				pos += snprintf (buf + pos, len - pos, "\"stdout\",");
				for (i = 0; i < length; ++i) {
					if (i > 0) {
						buf[pos++] = ',';
					}
					pos += snprintf(buf + pos, len - pos, "%d", data[i]);
				}
				buf[pos++] = '\0';
			
				instance->ExternalPostMessage(buf);
				delete buf;

				fwrite (data, length, 1, stdout);
			}
			fflush (stdout);
		}
		
		static void WriteError(tvm_instance_t *tvm, const char byte)
		{
			TVMInstance *instance = static_cast<TVMInstance *>(tvm->handle);
			char buffer[32];
			
			snprintf (buffer, sizeof(buffer), "\"stderr\",%d", byte);
			instance->ExternalPostMessage(buffer);

			fputc (byte, stderr);
			fflush (stderr);
		}

		static void *TVMThread(void *param)
		{
			TVMInstance *instance = static_cast<TVMInstance *>(param);
			tvm_instance_t *tvm = instance->tvm;
			
			tvm->handle		= (void *) instance;
			tvm->read_char		= &ReadChar;
			tvm->write_screen	= &WriteScreen;
			tvm->write_error	= &WriteError;

			fprintf (stderr, "running\n");
			instance->ExternalPostMessage("\"state\",\"running\"");

			int ret = tvm_run_instance(tvm);
			if (ret != 0) {
				std::string msg = std::string("\"error\",");
				msg.append(tvm->last_error);
				instance->ExternalPostMessage(msg);
			}

			instance->ExternalPostMessage("\"state\",\"finished\"");
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
			if (msg_queue_head == NULL) {
				msg_queue_head = buffer;
			} else {
				msg_queue_tail->next = buffer;
			}
			msg_queue_tail = buffer;
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
				ExternalPostMessage("\"state\",\"stopped\"");
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
			pthread_mutex_init(&kyb_mutex, NULL);
			tvm = NULL;
			state = STATE_STOPPED;
			kyb_buffer = NULL;
			kyb_buf_size = 0;
			kyb_buf_start = 0;
			kyb_buf_end = 0;
		}
		virtual ~TVMInstance()
		{
			StopTVM();
			ReleaseAndClean();
		}

		static inline uint8_t dehex(char c) {
			if ((c >= '0') && (c <= '9')) {
				return (c - '0');
			} else if ((c >= 'A') && (c <= 'F')) {
				return 10 + (c - 'A');
			} else if ((c >= 'a') && (c <= 'f')) {
				return 10 + (c - 'a');
			} else {
				return 0;
			}
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
			if (message.find("stdin:") == 0) {
				if (state != STATE_RUNNING) {
					return;
				}

				const char *c_str = message.c_str();
				uint8_t buf = 0;
				int s = 0;
				int i;
				for (i = 6; c_str[i] != '\0'; ++i) {
					buf <<= 4;
					buf |= dehex(c_str[i]);
					if (s) {
						//fprintf(stderr, "%02x\n", buf);
						QueueChar(buf);
						buf = s = 0;
					} else {
						s++;
					}
				}
			} else if (message.find("bytecode:") == 0) {
				if (state != STATE_STOPPED) {
					PostMessage(pp::Var("\"error\",\"can only load bytecode when stopped\""));
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
					PostMessage(pp::Var("\"state\",\"bytecode loaded\""));
				} else {
					ReleaseAndClean();
					PostMessage(pp::Var("\"error\",\"invalid bytecode\""));
				}
			} else if (message.find("start") == 0) {
				if (!tvm) {
					PostMessage(pp::Var("\"error\",\"no bytecode specified\""));
				} else if (state != STATE_STOPPED) {
					PostMessage(pp::Var("\"error\",\"already running\""));
				} else {
					StartTVM();
				}
			} else if (message.find("stop") == 0) {
				if (state != STATE_STOPPED) {
					StopTVM();
				} else {
					PostMessage(pp::Var("\"error\",\"not running\""));
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
			fprintf (stderr, "new TVMInstance built at %s %s\n",
				__DATE__, __TIME__);
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
