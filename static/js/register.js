$(function() {
    ko.bindingHandlers.buttonEnable = {
	update: function(elem, valueAccessor) {
	    var val = valueAccessor();
	    if (val) {
		$(elem).removeClass('disabled');
	    } else {
		$(elem).addClass('disabled');
	    }
	}
    };
    var hasPunctuation = function(str) {
	return (/\W/).test(str);
    };
    var GetRegisterViewModel = function() {
	var self = this;
	self.enteredUsername = ko.observable('');
	self.usernameState = ko.observable('');
	ko.computed(function() {
	    var username = self.enteredUsername();
	    if (username.length == 0) {
		self.usernameState('empty');
	    } else if (hasPunctuation(username)) {
		self.usernameState('invalidchars');
	    } else {
		$.get('/usernameavailable/' + username, function(response) {
		    var usernameAvailable = JSON.parse(response).usernameAvailable;
		    if (usernameAvailable) {
			self.usernameState('good');
		    } else {
			self.usernameState('nametaken');
		    }
		});
	    }
	});
	self.usernameError = ko.computed(function() {
	    var state = self.usernameState();
	    return (state == 'invalidchars' || state == 'nametaken');
	});
	self.displayUsernameState = ko.computed(function() {
	    var usernameState = self.usernameState();
	    if (usernameState == 'empty') {
		return '';
	    } else if (usernameState == 'invalidchars') {
		return 'Only letters and digits allowed.';
	    } else if (usernameState == 'nametaken') {
		return 'Username already taken!';
	    } else {
		return 'Username good.';
	    }
	});
	self.password1 = ko.observable('');
	self.password2 = ko.observable('');
	self.passwordsDontMatch = ko.computed(function() {
	    var pass1 = self.password1(), pass2 = self.password2();
	    if (pass1.length == 0) {
		return false;
	    } if (pass2.length > 0 && pass1 !== pass2) {
		return true;
	    } else {
		return false;
	    }
	});
	self.passwordsMatchText = ko.computed(function() {
	    if (self.passwordsDontMatch()) {
		return 'Passwords don\'t match.';
	    } else if (self.password2() === '' || self.password1() === '') {
		return '';
	    } else {
		return 'Passwords good.';
	    }
	});
	self.enteredEmail = ko.observable('');
	self.emailIsInvalid = ko.computed(function() {
	    var email = self.enteredEmail();
	    if (email === '') return false;
	    var emailFilter = /^\s*[\w\-\+_]+(\.[\w\-\+_]+)*\@[\w\-\+_]+\.[\w\-\+_]+(\.[\w\-\+_]+)*\s*$/;
	    return String(email).search(emailFilter) == -1;
	});
	self.displayEmailState = ko.computed(function() {
	    if (self.enteredEmail() === '') {
		return '';
	    } else if (self.emailIsInvalid()) {
		return 'Email is invalid.';
	    } else {
		return 'Email good.';
	    }
	});
	self.submitButtonState = ko.computed(function() {
	    var formValid = true;
	    formValid &= (self.usernameState() == 'good');
	    formValid &= self.password1().length > 0;
	    formValid &= self.password2().length > 0;
	    formValid &= !self.passwordsDontMatch();
	    formValid &= self.enteredEmail().length > 0;
	    formValid &= !self.emailIsInvalid();
	    return formValid;
	});
    };
    
    var viewmodel = new GetRegisterViewModel();
    ko.applyBindings(viewmodel);
});