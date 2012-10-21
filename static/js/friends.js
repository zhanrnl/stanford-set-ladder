$(function() {
    var showErrorBox = function(text) {
	$('#ErrorContainer').html('');
	$('#ErrorContainer').append(
	    $('<div>').addClass('alert alert-error').append(
		$('<button>').addClass('close')
		    .attr({'type': 'button', 'data-dismiss': 'alert'})
		    .html('&times;'),
		text
	    )
	);
    };

    var showSuccessBox = function(text) {
	$('#ErrorContainer').html('');
	$('#ErrorContainer').append(
	    $('<div>').addClass('alert alert-success').append(
		$('<button>').addClass('close')
		    .attr({'type': 'button', 'data-dismiss': 'alert'})
		    .html('&times;'),
		text
	    )
	);
    }

    var GetFriendsViewModel = function() {
	var self = this;
	self.friendName = ko.observable("");
	self.outFriends = ko.observableArray(outFriendsInit);
	self.addFriend = function() {
	    var friendName = self.friendName();
	    $.post('/addfriend', {friendName: encodeURI(friendName)}, function(data) {
		if (data == "cannotaddself") {
		    showErrorBox("You can't add yourself as a friend.");
		} else if (data == "alreadyfriend") {
		    showErrorBox("You have already added " + friendName + " as a friend.");
		} else if (data == "usernamenotfound") {
		    showErrorBox("User " + friendName + " not found.");
		} else if (data == "servererror") {
		    showErrorBox("Server error.");
		} else {
		    showSuccessBox("Friend added!");
		    $.get('/getfriends', function(data) {
			self.outFriends(JSON.parse(data));
		    });
		}
	    });
	    self.friendName('');
	}
    };
    
    var viewmodel = new GetFriendsViewModel();
    ko.applyBindings(viewmodel);

    $("#FriendNameInput").autocomplete({
	source: function(request, response) {
	    $.ajax({
		url: "/usernamesearch/" + encodeURI(request.term),
		success: function(data) {
		    response(JSON.parse(data));
		}
	    });
	},
	select: function(event, ui) {
	    console.log('hello');
	}
    });
});