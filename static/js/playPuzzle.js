/**
 * jQuery.fn.sortElements
 * --------------
 * @param Function comparator:
 *   Exactly the same behaviour as [1,2,3].sort(comparator)
 *   
 * @param Function getSortable
 *   A function that should return the element that is
 *   to be sorted. The comparator will run on the
 *   current collection, but you may want the actual
 *   resulting sort to occur on a parent or another
 *   associated element.
 *   
 *   E.g. $('td').sortElements(comparator, function(){
 *      return this.parentNode; 
 *   })
 *   
 *   The <td>'s parent (<tr>) will be sorted instead
 *   of the <td> itself.
 */
jQuery.fn.sortElements = (function(){
    var sort = [].sort;
    return function(comparator, getSortable) {
        getSortable = getSortable || function(){return this;};
        var placements = this.map(function(){
            var sortElement = getSortable.call(this),
                parentNode = sortElement.parentNode,
                // Since the element itself will change position, we have
                // to have some way of storing its original position in
                // the DOM. The easiest way is to have a 'flag' node:
                nextSibling = parentNode.insertBefore(
                    document.createTextNode(''),
                    sortElement.nextSibling
                );
            return function() {
                if (parentNode === this) {
                    throw new Error(
                        "You can't sort elements if any one is a descendant of another."
                    );
                }
                // Insert before flag:
                parentNode.insertBefore(this, nextSibling);
                // Remove flag:
                parentNode.removeChild(nextSibling);
            };
        });
        return sort.call(this, comparator).each(function(i){
            placements[i].call(getSortable.call(this));
        });
    };
})();
    
ko.bindingHandlers.fadeVisible = {
    init: function(element, valueAccessor) {
        // Initially set the element to be instantly visible/hidden depending on the value
        var value = valueAccessor();
        $(element).toggle(ko.utils.unwrapObservable(value)); // Use "unwrapObservable" so we can handle values that may or may not be observable
    },
    update: function(element, valueAccessor) {
        // Whenever the value subsequently changes, slowly fade the element in or out
        var value = valueAccessor();
        ko.utils.unwrapObservable(value) ? $(element).fadeIn(100) : $(element).fadeOut(200);
    }
};

var sum = function(list) {
    var o = 0;
    $.map(list, function(e) {
	o += e;
    });
    return o;
};

$.ajaxSetup({ cache: false });
$(function() {
    var numImagesLoaded = 0;
    var cardContainer = $('#CardContainer');
    var images = [];
    var startTime = new Date();
    var updateTime;
    var errorHideTimeout;
    var cards = gcards;
    var mode = gmode;

    // deal with redirecting out when we shouldn't be here
    $.get('/ajax/hasstarted', function(data) {
	if (data == 'true' && mode == 'daily') {
	    window.location.replace("/dailypuzzle");
	}
    });

    var isSet = function(set) {
	var cardNums = $.map(set, function(e){return e[1]});
	var toComponents = function(cardNum) {
	    return [[Math.floor(cardNum / 27) % 3,
		    Math.floor(cardNum / 9) % 3,
		    Math.floor(cardNum / 3) % 3,
		     cardNum % 3]];
	};
	cardComponents = $.map(cardNums, toComponents);
	var numbers = $.map(cardComponents, function(e){return e[0]});
	var shadings = $.map(cardComponents, function(e){return e[1]});
	var colors = $.map(cardComponents, function(e){return e[2]});
	var shapes = $.map(cardComponents, function(e){return e[3]});
	return (sum(numbers) % 3 == 0) && (sum(shadings) % 3 == 0) &&
	    (sum(colors) % 3 == 0) && (sum(shapes) % 3 == 0);
    };

    var makeTimeString = function(time) {
	var makeDoubleDigits = function(str) {
	    while (str.length < 2) str = '0' + str;
	    return str;
	};
	var totalSeconds = Math.floor(time / 1000);
	var seconds = totalSeconds % 60;
	var minutes = Math.floor(totalSeconds / 60);
	var centis = Math.floor((time / 10) % 100);
	return makeDoubleDigits(minutes.toString()) + ':' + 
	    makeDoubleDigits(seconds.toString()) + '.' + 
	    makeDoubleDigits(centis.toString());
    };

    var PracticePuzzleViewmodel = function() {
	var self = this;

	self.toFilename = function(str) {
	    return '/static/svg/' + str + '.svg';
	};
	self.cardSelected = [];
	for (var i = 0; i < 12; i++) {
	    self.cardSelected.push(ko.observable(false));
	}
	self.numSelected = ko.computed(function() {
	    var count = 0;
	    for (var i = 0; i < self.cardSelected.length; i++) {
		if (self.cardSelected[i]()) {
		    count++;
		}
	    }
	    return count;
	});
	self.showCards = ko.observable(false);
	self.windowWidth = ko.observable(0);
	self.windowHeight = ko.observable(0);
	self.cardWidth = ko.computed(function() {
	    var totalHeight = self.windowHeight() - 150;
	    var totalWidth = Math.min(self.windowWidth() - 
				      $('#CardContainer').offset().left, 
				      $('#CardContainer').width()) - 30;
	    var cardWidthA = (totalHeight / 4) * (320 / 200);
	    var cardWidthB = totalWidth / 3;
	    return Math.min(cardWidthA, cardWidthB);
	});
	self.setsFound = ko.observableArray();
	self.setAlreadyFound = function(set) {
	    var alreadyFound = false;
	    $.map(self.setsFound(), function(e) {
		if (JSON.stringify(set) == JSON.stringify(e[0])) {
		    alreadyFound = true;
		}
	    });
	    return alreadyFound;
	};
	self.notSetMistakes = ko.observable(0);
	self.alreadyFoundMistakes = ko.observable(0);
	self.showErrorBox = ko.observable(false);
	self.errorText = ko.observable('');
	self.complete = ko.observable(false);
	self.toggleCard = function(cardI) {
	    var state = self.cardSelected[cardI]();
	    self.cardSelected[cardI](!state);

	    var numSelected = self.numSelected();
	    if (numSelected == 3) {
		var set = $.map(cards, function(elem, i) {
		    if (self.cardSelected[i]()) {
			return [elem];
		    }
		});
		var isASet = isSet(set);
		if (isASet) {
		    if (self.setAlreadyFound(set)) {
			self.errorText('Already found!');
			self.showErrorBox(true);
			clearTimeout(errorHideTimeout);
			self.alreadyFoundMistakes(self.alreadyFoundMistakes() + 1);
			errorHideTimeout = setTimeout(function() {
			    self.showErrorBox(false);
			}, 1000);
		    } else {
			if (self.setsFound().length == 5) {
			    clearInterval(updateTime);
			}
			self.setsFound.push([set, self.timeDisplay(), self.time()]);
			if (self.setsFound().length == 6) {
			    self.complete(true);
			}
		    }
		} else {
		    self.errorText('Not a set!');
		    self.showErrorBox(true);
		    clearTimeout(errorHideTimeout);
		    self.notSetMistakes(self.notSetMistakes() + 1);
		    errorHideTimeout = setTimeout(function() {
			self.showErrorBox(false);
		    }, 1000);
		}
		setTimeout(function() {
		    self.deselectAll();
		}, 60);
	    }
	};
	self.deselectAll = function() {
	    $.map(self.cardSelected, function(e) {
		e(false);
	    });
	};
	self.cardClickedFn = function(cardI) {
	    return function() {
		self.toggleCard(cardI);
	    };
	};
	self.time = ko.observable(0);
	self.timeDisplay = ko.computed(function() {
	    return makeTimeString(self.time());
	});
	self.readyToStart = ko.observable(false);
	self.gameRunning = ko.observable(false);
	ko.computed(function(){
	    var complete = self.complete();
	    if (complete) {
		self.gameRunning(false);
		$('.errorBox').animate({
		    'height': 1,
		    'margin-top': 1
		}, 1000);
		$('.largeTime').animate({
		    'margin-bottom': 1
		}, 1000)
		var averageTime = self.setsFound()[5][2] / 6;
		var averageTimeDisplay = Math.floor(averageTime / 1000) + '.' + 
		    Math.floor(averageTime / 10) % 100;
		var leftCol = $('<div>').addClass('span8').append(
		    $('<p>').append(
			'Well done! You took ',
			$('<strong>').text(self.timeDisplay()),
			' to complete the puzzle.'
		    ),
		    $('<p>').append(
			'You found one set every ',
			$('<strong>').text(averageTimeDisplay),
			' seconds on average.'
		    ),
		    $('<p>').append(
			'You made ',
			$('<strong>').text(self.notSetMistakes()),
			' mistakes where you claimed something was a set that wasn\'t actually, and you made ',
			$('<strong>').text(self.alreadyFoundMistakes()),
			' mistakes where you entered a set you had already found.'
		    )
		);
		if (mode == 'daily') {
		    leftCol.append(
			$('<p>').text('Your time has been recorded on the ladder! You might want to head there now and compare your time with your friends\' scores?'),
			$('<a>').text('View daily puzzle ladder')
			    .attr({
				href: '/puzzleladder'
			    }).addClass('btn btn-success'),
			' ',
			$('<a>').text('Go back to main site')
			    .attr({
				href: '/'
			    }).addClass('btn btn-inverse')
		    );
		} else {
		    leftCol.append(
			$('<a>').text('Play again')
			    .attr({
				href: '/play/practicepuzzle'
			    }).addClass('btn btn-success'),
			' ',
			$('<a>').text('Go back to main site')
			    .attr({
				href: '/practicepuzzle'
			    }).addClass('btn btn-inverse')
		    );
		}
		$('#CardContainer').empty().append(
		    $('<h1>').text('Complete!').addClass('page-header'),
		    $('<div>').addClass('row').append(leftCol)
		);
		if (mode == 'daily') {
		    $.ajax({
			type: 'POST',
			url: '/ajax/puzzlecompleted',
			data: {'time': viewmodel.time()}
		    });
		}
	    }
	});
	self.startGame = function() {
	    if (self.gameRunning()) return;
	    self.gameRunning(true);
	    self.readyToStart(false);
	    windowResizeFunction();
	    setupLeftPanel();
	    ko.applyBindings(viewmodel);
	    viewmodel.showCards(true);
	    startTime = new Date();
	    if (mode == 'daily') {
		$.ajax({
		    type: 'POST',
		    url: '/ajax/puzzlestarted'
		});
	    }
	};
    };

    var viewmodel = new PracticePuzzleViewmodel();
    ko.applyBindings(viewmodel);

    var windowResizeFunction = function() {
	viewmodel.windowWidth($(window).width());
	viewmodel.windowHeight($(window).height());
    };
    $(window).resize(windowResizeFunction);

    var setupLeftPanel = function() {
	var leftPanel = $('#LeftPanel').empty();
	$('<h1>').addClass('largeTime').attr({
	    'data-bind': 'text: timeDisplay' 
	}).appendTo(leftPanel);
	$('<div>').append(
	    $('<span>').attr({
		'data-bind': 'text: errorText, fadeVisible: showErrorBox'
	    })
	).addClass('errorBox').appendTo(leftPanel);
	$('<h3>').text('Sets found:').appendTo(leftPanel);
	$('<div>').attr({
	    'data-bind': 'foreach: setsFound'
	}).append(
	    $('<div>').append(
		$('<img>').attr({
		    'data-bind': 'attr: {src: $root.toFilename($data[0][0][0])}',
		    width: '60'
		}).addClass('tinyCard')
	    ).append(
		$('<img>').attr({
		    'data-bind': 'attr: {src: $root.toFilename($data[0][1][0])}',
		    width: '60'
		}).addClass('tinyCard')
	    ).append(
		$('<img>').attr({
		    'data-bind': 'attr: {src: $root.toFilename($data[0][2][0])}',
		    width: '60'
		}).addClass('tinyCard')
	    ).append(
		$('<span>').attr({
		    'data-bind': 'text: $data[1]'
		}).addClass('timeLabel')
	    )
	).appendTo(leftPanel);
	updateTime = setInterval(function() {
	    var now = new Date();
	    viewmodel.time(now - startTime);
	}, 20);
    };

    var makeLoadFunction = function(num) {
	return (function() {
	    images[num].appendTo(cardContainer);
	    numImagesLoaded++;
	    if (numImagesLoaded % 3 == 0) {
		cardContainer.append($('<div>').addClass('clear'));
	    }
	    if (numImagesLoaded == 12) {
		$('.card').sortElements(function(a, b) {
		    var aText = $(a).children().first().text();
		    var bText = $(b).children().first().text();
		    return aText < bText ? -1 : 1;
		});
		viewmodel.readyToStart(true);
		$('#LeftPanel').empty().append(
		    $('<h3>').text('Get ready!'),
		    $('<p>').text('Click the green button or tap the spacebar to begin and start the timer.'),
		    $('<a>').attr({
			'data-bind': 'click: function(){startGame()}'
		    }).text('Start!').addClass('btn btn-success'),
		    ' ',
		    $('<a>').attr({
			'href': (function() {
			    if (mode == 'practice') return '/practicepuzzle';
			    else if (mode == 'daily') return '/dailypuzzle';
			})()
		    }).text('I don\'t want to do this').addClass('btn btn-inverse')
		);
		ko.applyBindings(viewmodel);
	    }
	});
    };

    $(document).keydown(function(event) {
	var keyCode = event.keyCode;
	var cardNum;
	if (keyCode == 32 && viewmodel.readyToStart()) {
	    viewmodel.startGame();
	} else if (viewmodel.gameRunning()) {
	    if (keyCode >= 65 && keyCode <= 76) {
		cardNum = keyCode - 65;
	    } else if (keyCode >= 97 && keyCode <= 108) {
		cardNum = keyCode - 97;
	    } else {
		return;
	    }
	    viewmodel.toggleCard(cardNum);
	}
    });
    
    $(window).unload(function() {
	if (mode == 'daily' && viewmodel.gameRunning() == true) {
	    $.ajax({
		type: 'POST',
		url: '/ajax/puzzleDNF',
		async: false
	    });
	}
    });

    for (var i = 0; i < 12; i++) {
	var img = $('<img>')
	    .attr({'src': viewmodel.toFilename(cards[i][0]),
		   'data-bind': 'attr: {width: cardWidth}, css: {cardSelected: cardSelected[' + i + ']()}'
		  })
	    .addClass('mediumCorners dropShadow')
	    .load(makeLoadFunction(i))
	    .click(viewmodel.cardClickedFn(i));
	images[i] = $('<div>').append(
	    $('<div>')
		.addClass('cardLabel')
		.text(String.fromCharCode(i + 65))
	).append(img).addClass('card');
    };
});