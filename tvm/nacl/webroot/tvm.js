TVMModule = null;  // Global application object.
TVMVT100 = null;
Terminal = null;
statusText = 'NO-STATUS';

// When the NaCl module has loaded, hook up an event listener to handle
// messages coming from it via PPB_Messaging.PostMessage() (in C) or
// pp::Instance.PostMessage() (in C++), and indicate success.
function moduleDidLoad() {
	TVMModule = document.getElementById('tvm');
	TVMModule.addEventListener('message', handleMessage, false);
	TVMVT100 = document.getElementById('vt100');
	updateStatus('SUCCESS');
}

// The 'message' event handler.  This handler is fired when the NaCl module
// posts a message to the browser by calling PPB_Messaging.PostMessage()
// (in C) or pp::Instance.PostMessage() (in C++).  This implementation
// simply displays the content of the message in an alert panel.
function handleMessage(message_event) {
	msg = $.parseJSON("[" + message_event.data + "]");
	if ((msg[0] == "stdout") || (msg[0] == "stderr")) {
		for (i = 1; i < msg.length; ++i) {
			Terminal.vt100(String.fromCharCode(msg[i]));
		}
	} else if (msg[0] == "state") {
		elem = document.getElementById('state');
		elem.innerHTML = "State: " + msg[1];
	} else if (msg[0] == "error") {
		alert(msg[1]);
	}
}

// If the page loads before the Native Client module loads, then set the
// status message indicating that the module is still loading.  Otherwise,
// do not change the status message.
function pageDidLoad() {
	if (TVMModule == null) {
		updateStatus('LOADING...');
	} else {
		// It's possible that the Native Client module onload event fired
		// before the page's onload event.  In this case, the status message
		// will reflect 'SUCCESS', but won't be displayed.  This call will
		// display the current message.
		updateStatus();
	}
}

// Set the global status message.  If the element with id 'statusField'
// exists, then set its HTML to the status message as well.
// opt_message The message test.  If this is null or undefined, then
// attempt to set the element with id 'statusField' to the value of
// |statusText|.
function updateStatus(opt_message) {
	if (opt_message)
		statusText = opt_message;
	var statusField = document.getElementById('status_field');
	if (statusField) {
		statusField.innerHTML = statusText;
	}
}

function loadBytecode(code) {
	tvm.postMessage('bytecode:' + code);
}

function fetchBytecode(url) {
	$.getJSON(url, function (data) {
			loadBytecode(data);
			});
}

function startTVM() {
	tvm.postMessage('start');
}
function stopTVM() {
	tvm.postMessage('stop');
}

function initTerminal() {
	setTimeout('Terminal = new TVMTerminal();', 100);
}
