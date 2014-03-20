// --------------------------------------------------------------------------
// Elements
// --------------------------------------------------------------------------

var infoError = $("#chat-info-error");
var infoNotice = $("#chat-info-notice");

var chatHistory = $("#chat-history");
var chatUserList = $("#chat-user-list");

var chatMsgBox = $("#chat-message-box");
var chatSendBtn = $("#chat-send-button");

var chatSetNameText = $("#chat-set-name-text");
var chatSetNameModal = $("#chat-set-name-modal");
var chatSetNameSubmit = $("#chat-set-name-submit");

// --------------------------------------------------------------------------
// Networking
// --------------------------------------------------------------------------

var netSocket = null;

function netConnect() {
	netSocket = new WebSocket("ws://" + window.location.host + "/ws");
	netSocket.onopen = uiOnConnect;
	netSocket.onclose = uiOnDisconnect;
	netSocket.onmessage = uiOnMessage;
}

function netSendMessage(msg) {
	netSocket.send(JSON.stringify(msg));
}

// --------------------------------------------------------------------------
// Backend API
// --------------------------------------------------------------------------

function apiJoin(name) {
	netSendMessage({
		"type": "join",
		"name": name
	});
}

function apiSend(text) {
	netSendMessage({
		"type": "message",
		"text": text
	});
}

function apiGetHistory() {
	netSendMessage({
		"type": "get_history"
	});
}

// --------------------------------------------------------------------------
// User Interface
// --------------------------------------------------------------------------

var uiName = null;
var uiUserList = [];

function uiOnConnect() {
	infoNotice.slideUp(600);

	if(uiName == null) {
		chatSetNameModal.modal();
	} else {
		apiJoin(uiName);
	}
}

function uiOnMessage(msg) {
	msg = JSON.parse(msg.data);

	switch(msg.type) {
		case "error":
			switch(msg.what) {
				case "bad_name":       infoError.text("name too long");       break;
				case "bad_message":    infoError.text("message too long");    break;
				case "name_taken":     infoError.text("name taken");          break;
				case "already_in":     infoError.text("developer fucked up"); break;
			}
			infoError.slideDown(600).delay(2500).slideUp(600);
			break;
		case "message":
			uiRenderMessage(msg.from, msg.text);
			uiScrollHistory();
			break;
		case "user_list":
			uiUserList = msg.list;
			uiEnableControls();
			uiRenderUserList();
			apiGetHistory();
			break;
		case "user_joined":
			listAdd(uiUserList, msg.user);
			uiRenderUserList();
			uiRenderWebMessage(msg.user + " joined the chat.");
			uiScrollHistory();
			break;
		case "user_left":
			listRemove(uiUserList, msg.user);
			uiRenderUserList();
			uiRenderWebMessage(msg.user + " left the chat.");
			uiScrollHistory();
			break;
		case "history":
			chatHistory.empty();
			msg.hist.forEach(function (msg) {
				switch(msg.type) {
					case "message":
						uiRenderMessage(msg.from, msg.text);
						break;
					case "user_joined":
						uiRenderWebMessage(msg.user + " joined the chat.");
						break;
					case "user_left":
						uiRenderWebMessage(msg.user + " left the chat.");
						break;
				}
			});
			uiScrollHistory();
			break;
	}
}

function uiOnDisconnect() {
	uiDisableControls();
	infoNotice.text("reconnecting").slideDown(600);
	netConnect();
}

function uiScrollHistory() {
	chatHistory.scrollTop(chatHistory[0].scrollHeight);
}

function uiRenderUserList() {
	chatUserList.empty();
	uiUserList.forEach(function(user) {
		$("<li></li>").text(user).appendTo(chatUserList);
	});
}

function uiRenderMessage(from, text) {
	var text = urlize(text, {autoescape: true, target: "_blank"});
	var entry = $("<p>: " + text + "</p>").appendTo(chatHistory);
	$("<span class=\"chat-username\"></span>").text(from).prependTo(entry);
}

function uiRenderWebMessage(text) {
	$("<p></p>").text(text).appendTo(chatHistory);
}

function uiEnableControls() {
	chatMsgBox.prop("disabled", false);
	chatSendBtn.prop("disabled", false);
	chatMsgBox.focus();
}

function uiDisableControls() {
	chatMsgBox.prop("disabled", true);
	chatSendBtn.prop("disabled", true);
}

chatSendBtn.click(function(e) {
	e.preventDefault();

	if(chatMsgBox.val() == "") {
		return;
	}

	apiSend(chatMsgBox.val());
	chatMsgBox.val("");
});

chatSetNameSubmit.click(function(e) {
	if(chatSetNameText.val() == "") {
		return;
	}

	uiName = chatSetNameText.val();
	apiJoin(uiName);
});

// --------------------------------------------------------------------------
// Helpers
// --------------------------------------------------------------------------

function listAdd(array, value) {
	array.push(value);
}

function listRemove(array, value) {
	var idx = array.indexOf(value);
	if(idx > -1) {
		array.splice(idx, 1);
	}
}

// --------------------------------------------------------------------------
// Main
// --------------------------------------------------------------------------

netConnect();
