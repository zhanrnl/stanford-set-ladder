$(function() {
    var validInt = function(str) {
	if (isNaN(str)) return NaN;
	if (parseInt(str) != str) return NaN;
	if (parseInt(str) < 0) return NaN;
	if (parseInt(str) > 27) return NaN;
	return parseInt(str);
    };

    var ReportGameViewModel = function() {
	var self = this;
	self.opponent = ko.observable('');
	self.opponentValid = ko.computed(function() {
	    var opponentText = self.opponent();
	    return opponentText.length > 0;
	});
	self.ownScore = ko.observable('');
	self.ownScoreValid = ko.computed(function() {
	    return validInt(self.ownScore());
	});
	self.ownScoreErrorText = ko.computed(function() {
	    if (self.ownScore().length == 0) return '';
	    var score = self.ownScoreValid();
	    if (isNaN(score)) {
		return 'Scores must be integers 0-27.';
	    }
	    return '';
	});
	self.opponentScore = ko.observable('');
	self.opponentScoreValid = ko.computed(function() {
	    return validInt(self.opponentScore());
	});
	self.opponentScoreErrorText = ko.computed(function() {
	    if (self.opponentScore().length == 0) return '';
	    var score = self.opponentScoreValid();
	    if (isNaN(score)) {
		return 'Scores must be integers 0-27.';
	    } 
	    return '';
	});
	self.numbersValid = ko.computed(function() {
	    var scoreA = self.ownScoreValid();
	    var scoreB = self.opponentScoreValid();
	    if (isNaN(scoreA) || isNaN(scoreB)) return false;
	    if (scoreA + scoreB > 27) return false;
	    if (scoreA + scoreB == 0) return false;
	    return true;
	});
	self.numbersValidText = ko.computed(function() {
	    var scoreA = self.ownScoreValid();
	    var scoreB = self.opponentScoreValid();
	    if (isNaN(scoreA) || isNaN(scoreB)) return '';
	    if (scoreA + scoreB > 27) return 'Can\'t have more than 27 sets between the two players.';
	    if (scoreA + scoreB == 0) return 'Scores can\'t be both 0.';
	    return '';
	});
	self.submitButtonState = ko.computed(function() {
	    if (!self.opponentValid()) return false;
	    if (isNaN(self.ownScoreValid())) return false;
	    if (isNaN(self.opponentScoreValid())) return false;
	    if (!self.numbersValid()) return false;
	    return true;
	});
    };

    var viewmodel = new ReportGameViewModel();
    ko.applyBindings(viewmodel);

    $("#OpponentInput").autocomplete({
	source: function(request, response) {
	    $.ajax({
		url: "/infriendsearch/" + encodeURI(request.term),
		success: function(data) {
		    response(JSON.parse(data));
		}
	    });
	},
	select: function(event, ui) {
	    viewmodel.opponent(ui.item.value);
	},
	minLength: 0
    });
});