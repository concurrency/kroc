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
#include "ppapi/cpp/instance.h"
#include "ppapi/cpp/module.h"
#include "ppapi/cpp/var.h"

#include "tvm_nacl.h"

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
		tvm_instance_t *tvm;
	public:
		/// The constructor creates the plugin-side instance.
		/// @param[in] instance the handle to the browser-side plugin instance.
		explicit TVMInstance(PP_Instance instance) : pp::Instance(instance)
		{
			tvm = NULL;
		}
		virtual ~TVMInstance()
		{
			if(tvm) {
				free_tvm_instance(tvm);
				tvm = NULL;
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
			if (message.find("bytecode") == 0) {
				uint8_t *data = (uint8_t *) malloc(message.size());
				size_t data_len = 0;

				data_len = tvm_base64_decode(message.c_str() + 9, data);
				fprintf (stderr, "message length = %d, data length = %d\n",
					message.size(), data_len);

				if (tvm) {
					free_tvm_instance(tvm);
				}
				tvm = alloc_tvm_instance();
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
			return new TVMInstance(instance);
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
