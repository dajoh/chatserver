var socket = new WebSocket("ws://" + window.location.host + "/ws");

var infoConnecting = $("#chat-info-connecting");
var infoDisconnected = $("#chat-info-disconnected");

var chatHistory = $("#chat-history");
var chatUserList = $("#chat-user-list");

var chatMsgBox = $("#chat-message-box");
var chatSendBtn = $("#chat-send-button");

var chatSetName = $("#chat-set-name");
var chatSetNameText = $("#chat-set-name-text");
var chatSetNameSubmit = $("#chat-set-name-submit");

infoConnecting.slideDown(500);

function sendJSON(obj) {
	socket.send(JSON.stringify(obj));
}

function appendMessage(from, text) {
	var entry = $("<p></p>").text(": " + text).appendTo(chatHistory);
	$("<span class=\"chat-name\"></span>").text(from).prependTo(entry);
}

function appendSysMessage(text) {
	$("<p></p>").text(text).appendTo(chatHistory);
}

function addUser(name) {
	$("<li></li>").text(name).appendTo(chatUserList);
}

function removeUser(name) {
	$("#chat-user-list > li").filter(function() {
		return $.text([this]) === name;
	}).remove();
}

socket.onopen = function() {
	infoConnecting.slideUp(500);
	chatSetName.modal();
};

socket.onmessage = function(raw) {
	var msg = JSON.parse(raw.data);

	if(msg["type"] == "message") {
		appendMessage(msg["from"], msg["text"]);
	}
	else if(msg["type"] == "connected") {
		appendSysMessage(msg["user"] + " entered the chat.");
		addUser(msg["user"]);
	}
	else if(msg["type"] == "disconnected") {
		appendSysMessage(msg["user"] + " left the chat.");
		removeUser(msg["user"]);
	}
	else if(msg["type"] == "client_list") {
		msg["list"].forEach(addUser);
	}

	chatHistory.scrollTop(chatHistory[0].scrollHeight);
};

socket.onclose = function() {
	infoConnecting.slideUp(500, function() {
		infoDisconnected.slideDown(500);
		chatMsgBox.prop("disabled", true);
		chatSendBtn.prop("disabled", true);
	});
};

chatSetNameSubmit.click(function() {
	sendJSON({
		"type": "set_name",
		"name": chatSetNameText.val()
	});
	chatMsgBox.prop("disabled", false);
	chatSendBtn.prop("disabled", false);
	chatMsgBox.focus();
});

chatSendBtn.click(function(e) {
	e.preventDefault();

	if(chatMsgBox.val() == "") {
		return;
	}

	sendJSON({
		"type": "send_msg",
		"text": chatMsgBox.val()
	});
	chatMsgBox.val("");
});
