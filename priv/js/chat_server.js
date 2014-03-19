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

socket.onopen = function() {
	infoConnecting.slideUp(500);
	chatSetName.modal();
};

socket.onmessage = function(msg) {
	var split = msg.data.split(/:(.+)?/);
	var entry = $("<p></p>").text(": " + split[1]).appendTo(chatHistory);
	$("<span class=\"chat-name\"></span>").text(split[0]).prependTo(entry);
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
	socket.send(chatSetNameText.val());
	chatMsgBox.prop("disabled", false);
	chatSendBtn.prop("disabled", false);
	chatMsgBox.focus();
});

chatSendBtn.click(function(e) {
	e.preventDefault();

	if(chatMsgBox.val() == "") {
		return;
	}

	socket.send(chatMsgBox.val());
	chatMsgBox.val("");
});
