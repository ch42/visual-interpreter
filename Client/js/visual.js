
var selectableElements = '#currentExprContainer .bvar, #currentExprContainer .fvar, #currentExprContainer .redex';	
var brackets = '#currentExprContainer *[data-brackets]';



$(document).ready(function() {
	
	Interpreter.keywords();
	Editor.setWorkspace(Editor.workspaces.MANIPULATE);
	Canvas.init();
		
	$(document).foundation();
	
	
	/**
	 *	Keyword Drag & Drop
	 */
	$('#sidebar').on('dragstart', '.keyword', function(e) {
		console.log('drag start');
		e.originalEvent.dataTransfer.setData('keyword', $(this).html());
	});
	
	$('#sidebar').on('click', '.keyword', function(e) {
		var keyword = $(this).html();
		$('#textareaEditor').insertAtCaret(' ' + keyword + ' ');
	});
	
	$('#textareaEditor').on('dragover', function(e) {
		e.originalEvent.preventDefault();
	});
	
	$('#textareaEditor').on('drop', function(e) {
		var keyword = e.originalEvent.dataTransfer.getData('keyword');
		$(this).insertAtCaret(' ' + keyword + ' ');
	});
	
		
	$('#buttonToggleSidebar').click(function() {	
		Editor.toggleSidebar();
	});
	
	// User changes workspace
	$('#selectWorkspace').change(function() {
		var newWorkspace = $('#selectWorkspace').val();
		Editor.setWorkspace(newWorkspace);	
	});
	
	$('input[name=highlightMixins]').change(function() {
		Transition.add($(selectableElements), 'all 0.2s');
		Editor.resetMixins();
	});
	
	$('input[name=bracketMixin]').change(function() {
		Transition.add($(brackets), 'all 0.4s');
		Editor.resetMixins();
	});	
	
	$('#history').on('click', '.historyItem', function() {
		$('#history .historyItem').removeClass('selection');
		$(this).addClass('selection');	
		Editor.showExpr($(this).data('expr'));	
		
		switch ($(this).data('transformation')) {
			
			case Interpreter.functions.ALPHA_CONVERSION:
				var varID = $(this).data('termID');
				var oldVarName = $(this).data('otherData');
				Editor.highlightAlphaConversion(varID, oldVarName)
				break;
				
			case Interpreter.functions.BETA_REDUCTION:			
				break;			
		}
		
		Editor.redrawConnections();	
	});
	
	/**
	 * Keyboard-Shortcuts
	*/
		
	// Sidebar visibility
	$(document).bind('keydown', 'alt+s', function() {
		Editor.toggleSidebar();
	});
		
	// Keyword folding
	$(document).bind('keydown', 'alt+f', function() {
		Editor.toggleFoldingMode();
		$('#checkboxFoldingMode').prop('checked', !$('#checkboxFoldingMode').prop('checked'));
	});
	
	// Zoom in
	$(document).bind('keydown', 'f4', function() {
		var val = parseInt($('#sliderZoom').attr('data-slider'));
		
		if(val + 4 <= Editor.maxFontSize) {
			val += 4;
			$('#sliderZoom').foundation('slider', 'set_value', val);
			Editor.setFontSize(val);
		}
	});
	
	// Zoom out
	$(document).bind('keydown', 'f3', function() {
		var val = parseInt($('#sliderZoom').attr('data-slider'));
		
		if(val - 4 >= Editor.minFontSize) {
			val -= 4;
			$('#sliderZoom').foundation('slider', 'set_value', val);
			Editor.setFontSize(val);
		}
	});
	
	// Next workspace
	$(document).bind('keydown', 'left', function() {
		$('#selectWorkspace option:selected').prev().prop('selected', 'selected');
		$('#selectWorkspace').trigger('change');
	});
	
	// Previous workspace
	$(document).bind('keydown', 'right', function() {
		$('#selectWorkspace option:selected').next().prop('selected', 'selected');
		$('#selectWorkspace').trigger('change');
	});
	
	

	/* GUI-EVENTS */
	$('#textareaEditor').keyup(function() {
		Editor.hideErrorMsg();
		
		var input = $('#textareaEditor').val();
		var cursorPosition = $('#textareaEditor').prop('selectionStart');
		$('#textareaEditor').val(input.replace(/\\/g, 'λ'));
		$('#textareaEditor').prop('selectionStart', cursorPosition);
		$('#textareaEditor').prop('selectionEnd', cursorPosition);	
	});
	
	$('#inputAlphaConversion').keyup(function() {
		Editor.hideErrorMsgAlphaConversion();
	});
	
	
	
	// User clicks 'Interact'-Button
	$('#buttonParse').click(function() {
		Interpreter.parser(Editor.getInput());
		Editor.setWorkspace(Editor.workspaces.MANIPULATE);
		Editor.clearHistory();
	});
	

	
	// User uses Zoom-Slider
	$('#sliderZoom[data-slider]').on('change', function(){	
		var val = $('#sliderZoom').attr('data-slider');
		Editor.setFontSize(val);
	});
		
	$('#checkboxFoldingMode').change(function() {
		Editor.toggleFoldingMode();
	});
		
	$(document).click(function(e) {
		e.stopPropagation();
		Editor.unselectAll();
	});	
		
	$('.f-dropdown').click(function(e) {
		e.stopPropagation();
	});	
	
	// User wants to reduce the selected redex
	$('#buttonBetaReduction').click(function() {
		Interpreter.betaReduction(Editor.currentTermID, Editor.currentExprXML);
	});
		
	$('#buttonAlphaConversion').click(function() {
		var varName = $('#inputAlphaConversion').val().trim();
		
		if($.inArray(varName, Editor.keywords) > 0) {
			Editor.showErrorMsgAlphaConversion('Keywords are not allowed.');
		}
		else if(!isNaN(varName)) {
			Editor.showErrorMsgAlphaConversion('Numbers are not allowed.');
		}
		else if(varName.search(/^[a-zA-Z0-9]+$/) == -1){
			Editor.showErrorMsgAlphaConversion('Special characters are not allowed.');
		}
		else {
			Interpreter.alphaConversion(Editor.currentTermID, varName, Editor.currentExprXML);
		}
	});
	
	
	/* EXPRESSION-EVENTS */
	// Folding/Unfolding of subexpressions
	$('#currentExprContainer').on('click', '.keyword > .name', function(e) {
		e.stopPropagation();
		$(this).parent().children('.keyword > .expr').toggle();
		Editor.unselectAll();
		Editor.redrawConnections();
	});
	
	$('#history').on('click', '.keyword > .name', function(e) {
		e.stopPropagation();
		$(this).parent().children('.keyword > .expr').toggle();
		Editor.unselectAll();
	});
	
	// User selects redex
	$('#currentExprContainer').on('click', '.redex', function(e) {
		e.stopPropagation();
		Editor.select($(this), $(this));
	});
	
	// User selects bounded var
	$('#currentExprContainer').on('click', '.bvar', function(e) {
		e.stopPropagation();	
		var selectedVars = $('#currentExprContainer .bvar[data-id="' + $(e.target).data('id') + '"]');
		Editor.select($(this), selectedVars);		
	});
	
	// User selects free var
	$('#currentExprContainer').on('click', '.fvar', function(e) {
		e.stopPropagation();	
		Editor.select($(this), $(this));
	});
	
	// Block dropdows etc.
	$('#history').on('click', function(e) {
		e.stopPropagation();
	});
	
});



var Transition = {
		
	add: function(elements, transition) {
		elements.css({'transition': transition});
	},
	
	remove: function(elements) {
		elements.css({'transition': 'none'});
	}	
}

var Editor = {
		
	minFontSize: 10,
	maxFontSize: 90,
	sidebarVisible: true,
	expandFolds: false,
	
	keywords: [],
	
	workspaces: {
	    MANIPULATE: 	'workspace-manipulate',
	    REVIEW_CHANGES: 'workspace-review-changes'
	},

	mixins: {
		RENDER_EXPR_BASIC: 	'mixin-render-expr-basic',
		STACKS: 			'mixin-stacks',
		HIGHLIGHT_BVARS: 	'mixin-highlight-bvars',
		HIGHLIGHT_FVARS: 	'mixin-highlight-fvars',
		HIGHLIGHT_REDEXES: 	'mixin-highlight-redexes'	
	},
	
	currentWorkspace: null,
	currentMixins: [],
	
	// Holds a reference to the currently selected part of the expression
	currentTermID: null,
	currentExprXML: null,

		
	showExpr: function(exprXML) {	
		this.hideErrorMsg();
		this.currentExprXML = exprXML;
							
		var currentExprHTML = XML.toHTML(this.currentExprXML);
		$('#currentExprContainer').html(currentExprHTML);
		
		if(!this.expandFolds) {
			this.collapseAllFolds();
		}
			
		this.unselectAll();
		console.log($('#currentExprContainer').html());
	},
	
	showErrorMsg: function(errorMsg, hideExprContainer) {
		this.unselectAll();
		$('#errorMsg').html(errorMsg).fadeIn(200);	
		
		if(hideExprContainer == undefined || hideExprContainer == true) {
			$('#currentExprContainer').hide();
		}
		
	},
	
	hideErrorMsg: function() {
		$('#errorMsg').fadeOut(200);
		$('#currentExprContainer').show();
	},
	
	showErrorMsgAlphaConversion: function(errorMsg) {
		$('#errorMsgAlphaConversion').html(errorMsg).fadeIn(200);	
		
	},
	
	hideErrorMsgAlphaConversion: function() {
		$('#errorMsgAlphaConversion').fadeOut(200);
	},
	
	getInput: function() {
		return $('#textareaEditor').val().trim();
	},
	
	setFontSize: function(fontSize) {
		Transition.remove($(selectableElements));
		Transition.remove($(brackets));
		$('#outputContainer div').css( 'font-size', fontSize + 'px');
			
		this.redrawConnections(false); 
		this.unselectAll();	
	},
	
	/**
	 *	Term Selection
	 */
	select: function(eventTarget, elements) {						  
		Transition.remove($(selectableElements));
			
		this.currentTermID = elements.data('id');
		$(selectableElements).removeClass('selection');
		elements.addClass('selection');
	
		Foundation.libs.dropdown.toggle(eventTarget);
		this.hideErrorMsg();
	},
	
	unselectAll: function() {
		// close all dropdowns
		Foundation.libs.dropdown.closeall();
		$('#currentExprContainer div').removeClass('selection');	
		$('#inputAlphaConversion').val('');
		$('#errorMsgAlphaConversion').hide();
	},
		
	/**
	 *	Workspaces and Mixins
	 */
	setWorkspace: function(newWorkspace) {
		Transition.remove($(selectableElements));
		Transition.remove($(brackets));
		
		$('.errorMsgContainer').hide();
		$('*[data-themeable]').removeClass(this.currentWorkspace);
		$('*[data-themeable]').addClass(newWorkspace);
		
		$('#selectWorkspace option[value="' + newWorkspace + '"]').attr('selected', true);
				
		this.currentWorkspace = newWorkspace;
		this.resetMixins();
	},
	
	setMixins: function(newMixins) {
		this.currentMixins.forEach(function(oldMixin) {
			$('*[data-allows-mixins]').removeClass(oldMixin);
		});

		newMixins.forEach(function(newMixin) {
			$('*[data-allows-mixins]').addClass(newMixin);
		});

		this.currentMixins = newMixins;
		this.redrawConnections();	
	},
		
	resetMixins: function() {
		var highlightingMixins = [];
		var bracketMixins = [];
		var otherMixins = [];

		switch(this.currentWorkspace) {		
			case this.workspaces.MANIPULATE:
				highlightingMixins = this.selectedHighlightingMixins();
				bracketMixins = this.selectedBracketMixins();
				otherMixins = [this.mixins.RENDER_EXPR_BASIC];
				break;

			case this.workspaces.REVIEW_CHANGES:
				otherMixins = [this.mixins.RENDER_EXPR_BASIC];
				break;	
		}

		this.setMixins(otherMixins.concat(bracketMixins.concat(highlightingMixins)));	
	},
	
	redrawConnections: function(withAnimation) {	
		Canvas.clear();
		
			
		$('#currentExprContainer .predecessor .app, #currentExprContainer .predecessor .expr')
			.first().children().find('.bvar')
			.first().addClass('line-through');

				
		if(this.currentWorkspace == this.workspaces.REVIEW_CHANGES) {
			if(withAnimation === undefined || withAnimation) {
				setTimeout(function() {
					Transition.remove($('#canvasContainer'));
					$('#canvasContainer').css('opacity', '0');
				
					$('#currentExprContainer .predecessor .keyword > .expr').show();
				
					var source = $('#currentExprContainer [data-annotation="arrowSource"]').first();
					var targets = $('#currentExprContainer [data-annotation="arrowTarget"]');	
				
					Canvas.drawConnections(source, targets);
					
					Transition.add($('#canvasContainer'), 'opacity 0.8s');
					$('#canvasContainer').css('opacity', '1');
				}, 150);
			} else {
				var source = $('#currentExprContainer [data-annotation="arrowSource"]').first();
				var targets = $('#currentExprContainer [data-annotation="arrowTarget"]');	
			
				Canvas.drawConnections(source, targets);			
			}
		}
	},
	
	highlightAlphaConversion: function(varID, oldVarName) {
		$('#currentExprContainer .bvar[data-id="' + varID + '"]').addClass('alphaConversion');
		$('#currentExprContainer .bvar[data-id="' + varID + '"]').attr('data-old-var-name', oldVarName);	
		$('#currentExprContainer .bvar[data-id="' + varID + '"]').closest('.keyword > .expr').show();
	},

	selectedHighlightingMixins: function() {
		var highlightingMixins = [];
	
		if($('#checkboxHighlightRedexes').prop('checked')) {
			highlightingMixins.push(this.mixins.HIGHLIGHT_REDEXES);
		} 
		if($('#checkboxHighlightBoundedVars').prop('checked')) {
			highlightingMixins.push(this.mixins.HIGHLIGHT_BVARS);
		} 		
		if($('#checkboxHighlightFreeVars').prop('checked')) {
			highlightingMixins.push(this.mixins.HIGHLIGHT_FVARS);
		} 
	
		return highlightingMixins;
	},
	
	selectedBracketMixins: function() {
		var mixin = $('input[name=bracketMixin]:checked').val();

		if(mixin !== 'none') {
			return [mixin]
		}	
		return [];	
	},
	
	/**
	 *	Code Folding / Keywords
	 */
	addKeyword: function(keyword) {
		this.keywords.push(keyword);
		$('#keywordList').append('<div class="keyword" draggable="true">' + keyword + '</div>')
	},
	
	toggleFoldingMode: function() {
		this.expandFolds = !this.expandFolds;

		if(this.expandFolds) { this.expandAllFolds(); }
		else { this.collapseAllFolds(); }	
	},
	
	expandAllFolds: function() {
		$('#currentExprContainer .keyword > .expr').show();
	},

	collapseAllFolds: function() {
		$('#currentExprContainer .keyword > .expr').hide();
	},
	
	/**
	 *	Sidebar
	 */
	toggleSidebar: function() {
		if(this.sidebarVisible) {
			$('#buttonToggleSidebar').html('<img src="img/show-sidebar.svg" width="5" alt="Show">');
			$('#sidebar').css('margin-left', '-184px');
			$('#sidebar').css('overflow-y', 'hidden');
			$('#sidebar > .container').css('display', 'none');
			$('#editor').css('margin-left', '26px');
		}
		else {
			$('#buttonToggleSidebar').html('<img src="img/hide-sidebar.svg" width="5" alt="Hide">');
			$('#sidebar').css('margin-left', '0px');
			$('#sidebar').css('overflow-y', 'scroll');
			$('#sidebar > .container').show();
			$('#editor').css('margin-left', '210px');
		}		
		this.sidebarVisible = !this.sidebarVisible;
		this.unselectAll();
		// neccessary to reset slider
		$(document).foundation();	
	},
	
	/**
	 *	History
	 */
	addHistoryItem: function(exprXML, transformation, otherData) {
		
		var exprHTML = XML.toHTML(exprXML);
		var historyItem = $('<div>')
							.addClass('historyItem selection')
							.data('expr', exprXML)
							.data('transformation', transformation)
							.data('termID', this.currentTermID)
							.data('otherData', otherData)
							.append($('<span>')
							.addClass('label')
							.html(transformation))
							.append(exprHTML);
							
		if(transformation === Interpreter.functions.ALPHA_CONVERSION) {
			historyItem.children().find('.bvar[data-id="' + this.currentTermID + '"]').addClass('alphaConversion');
			historyItem.children().find('.bvar[data-id="' + this.currentTermID + '"]').attr('data-old-var-name', otherData);
			historyItem.children().find('.bvar[data-id="' + this.currentTermID + '"]').closest('.keyword > .expr').show();
		}					
							
		$('#history .historyItem').removeClass('selection');
		$('#history').prepend(historyItem);						
	},
	
	
	clearHistory: function() {
		$('#history').empty();
	}
	
	
}

var Interpreter = {
	
	functions: {
		PARSER: 				'Original',
		ALPHA_CONVERSION: 	'α-conversion',
		BETA_REDUCTION: 		'β-reduction'
	},
	
	parser: function(string) {
		$.post('/parser', { 
							string: string 
						 })
						 
			.done(function(analyzedExprXML) {
				Editor.showExpr(analyzedExprXML);
				Editor.addHistoryItem(analyzedExprXML, Interpreter.functions.PARSER);
			})	
			
			.fail(function(xhr) {
				Editor.showErrorMsg(xhr.responseText);
			});		
	},
	
	keywords: function() {
		$.get('/keywords')
			.done(function(keywords) {
	
				$(keywords).find('keyword').each(function() {
					Editor.addKeyword($(this).text());
				});
			})	
			
			.fail(function(){ 
				
			});		
	},
	
	alphaConversion: function(varID, newVarName, analyzedExprXML) {
		
		var oldVarName = $('.bvar[data-id="' +  varID + '"]').first().html();

		$.post('/alpha-conversion', { 
									  varID: varID, 
							          newVarName: newVarName, 
							          analyzedExprXML: XML.serialize(analyzedExprXML) 
						            })
						  
			.done(function(newAnalyzedExprXML) {
				Editor.showExpr(newAnalyzedExprXML);
				Editor.addHistoryItem(newAnalyzedExprXML, Interpreter.functions.ALPHA_CONVERSION, oldVarName);
				Editor.highlightAlphaConversion(varID, oldVarName);
			})
			
			.fail(function(xhr) {
				Editor.showErrorMsg(xhr.responseText, false);
			});	
	},	
	
	betaReduction: function(appID, analyzedExprXML) {

		$.post('/beta-reduction', { 
									appID: appID, 
									analyzedExprXML: XML.serialize(analyzedExprXML) 
						  	  	  })
						   
			.done(function(newAnalyzedExprXML) {
				Editor.showExpr(newAnalyzedExprXML);
				Editor.addHistoryItem(newAnalyzedExprXML, Interpreter.functions.BETA_REDUCTION);
			})
			
			.fail(function(xhr) {
				Editor.showErrorMsg(xhr.responseText, false);
			});	
	}		
}  

// XSLT transformations
var XML = {	
	
	serialize: function(xml) {
		return (new XMLSerializer()).serializeToString(xml);
	},
	
	toHTML: function(xml) {
		var result = '';

		$.ajax({
			url: 'xml/stylesheets/html5.xsl',
			async: false
		}).done(function(xsl) {
				// Code for IE
				if (window.ActiveXObject) {
			  	  result = xml.transformNode(xsl);
			  	}

				// Code for Chrome, Firefox, Opera, etc.
				else if (document.implementation && document.implementation.createDocument) {
					xsltProcessor = new XSLTProcessor();
			  		xsltProcessor.importStylesheet(xsl);
			  		result = xsltProcessor.transformToFragment(xml, document);
			  	}
			})

		return result;	
	}	
}

// Manipulates SVG to draw visualizations
var Canvas = {
	
	freshCanvas: undefined,
	init: function() {
		this.freshCanvas = $('#canvasContainer').html();
	},
	
	clear: function() {
		$('#canvasContainer').html(this.freshCanvas);	
	},
	
	drawArrow: function(d) {
	    var p = document.createElementNS('http://www.w3.org/2000/svg', 'path');
		
	    p.setAttributeNS(null, 'd', d);
		p.setAttributeNS(null, 'class', 'arrow');
		p.setAttributeNS(null, 'fill', 'none');
		p.setAttributeNS(null, 'marker-end', 'url(#markerArrow)');
		
		// jQuery $('#canvas').append(p) doesn't work!
		document.getElementById('canvas').appendChild(p);
	},
	
	drawConnection: function(source, target, i, j) {
		var c = $('#canvasContainer').offset();
		var p1 = source.offset();
		var p2 = target.offset();
		
		// start point
		var x1 = p1.left - c.left + (source.outerWidth()/2);
		var x2 = p2.left - c.left + (target.outerWidth()/2) + 5;
		
		// end point
		var y1 = p1.top - c.top + source.outerHeight();
		var y2 = p2.top - c.top + target.outerHeight() + 5;
		
		// single control point for quadratic bézier curve
		var curviness = 14;
		var corrector = 10 + j*curviness - i*(curviness-4);
		
		var x3 = x1 + (x2 - x1)/2;
		var y3 = y1 + corrector;
		
		
		var d = 'M ' + x1 + ' ' + y1 + ' Q ' + x3 + ' ' + y3 + ' ' + x2 + ' ' + y2;
		this.drawArrow(d);
	},
	
	drawConnections: function(source, targets) {
		var j = targets.length;
		targets.each(function(i) {
			Canvas.drawConnection(source, $(this), i, j);
		});	
	}
}
