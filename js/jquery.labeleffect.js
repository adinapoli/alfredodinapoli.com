/* http://keith-wood.name/labeleffect.html
   Label Effects for jQuery v1.0.0.
   Written by Keith Wood (kbwood{at}iinet.com.au) July 2009.
   Dual licensed under the GPL (http://dev.jquery.com/browser/trunk/jquery/GPL-LICENSE.txt) and 
   MIT (http://dev.jquery.com/browser/trunk/jquery/MIT-LICENSE.txt) licenses. 
   Please attribute the author if you use it. */

(function($) { // Hide scope, no $ conflict

var PROP_NAME = 'labelEffect';

/* Label effects manager. */
function LabelEffects() {
	this._defaults = {
		color: '', // Main text colour
		hiColor: 'silver', // The highlight colour
		hiDir: this.NONE, // The highlight direction
		hiOffset: 0, // The highlight offset
		hiFill: false, // True to fill from main text to highlight
		hiBlend: false, // True to blend from main color to highlight
		shadowColor: 'silver', // The shadow colour
		shadowDir: this.DOWNRIGHT, // The shadow direction
		shadowOffset: 5, // The shadow offset
		shadowFill: false, // True to fill from main text to shadow
		shadowBlend: false, // True to blend from main color to shadow
		effect: '' // A preset effect
	};
	this._effects = {
		echoed: {color: '', hiColor: 'white', hiDir: this.DOWNRIGHT, hiOffset: 2,
			hiFill: false, hiBlend: false, shadowColor: 'gray', shadowDir: this.DOWNRIGHT,
			shadowOffset: 4, shadowFill: false, shadowBlend: false},
		floating: {color: '', hiColor: 'silver', hiDir: this.NONE, hiOffset: 0,
			hiFill: false, hiBlend: false, shadowColor: 'silver', shadowDir: this.DOWNRIGHT,
			shadowOffset: 5, shadowFill: false, shadowBlend: false},
		raised: {color: 'white', hiColor: 'silver', hiDir: this.UPLEFT, hiOffset: 1,
			hiFill: false, hiBlend: false, shadowColor: 'black', shadowDir: this.DOWNRIGHT,
			shadowOffset: 1, shadowFill: false, shadowBlend: false},
		shadow: {color: 'white', hiColor: 'white', hiDir: this.NONE, hiOffset: 0,
			hiFill: false, hiBlend: false, shadowColor: 'black', shadowDir: this.DOWNRIGHT,
			shadowOffset: 1, shadowFill: false, shadowBlend: false},
		sunken: {color: 'white', hiColor: 'silver', hiDir: this.DOWNRIGHT, hiOffset: 1,
			hiFill: false, hiBlend: false, shadowColor: 'black', shadowDir: this.UPLEFT,
			shadowOffset: 1, shadowFill: false, shadowBlend: false}
	};
}

// X- and y-offsets by direction
var OFFSETS = [[-1, -1], [0, -1], [+1, -1], [-1, 0],
	[0, 0], [+1, 0], [-1, +1], [0, +1], [+1, +1]];

$.extend(LabelEffects.prototype, {
	/* Class name added to elements to indicate already configured with effects. */
	markerClassName: 'hasLabelEffect',
	
	/* Direction indicators. */
	UPLEFT: 0,
	UP: 1,
	UPRIGHT: 2,
	LEFT: 3,
	NONE: 4,
	RIGHT: 5,
	DOWNLEFT: 6,
	DOWN: 7,
	DOWNRIGHT: 8,

	/* Override the default settings for all label effects instances.
	   @param  settings  (object) the new settings to use as defaults
	   @return  (LabelEffects) this object */
	setDefaults: function(settings) {
		extendRemove(this._defaults, settings || {});
		return this;
	},

	/* Add a new label effect to the list.
	   @param  id        (string) the ID of the new effect
	   @param  settings  (object) the preset settings
	   @return  (LabelEffects) this object */
	addEffect: function(id, settings) {
		this._effects[id] = settings;
		return this;
	},

	/* Return the list of defined effects.
	   @return  (object[]) indexed by effect id (string), each object contains:
	            settings (object) the effect's settings */
	getEffects: function() {
		return this._effects;
	},

	/* Attach the label effect to a control.
	   @param  target    (element) the control to affect
	   @param  settings  (object) the custom options for this instance */
	_attachEffect: function(target, settings) {
		target = $(target);
		if (target.hasClass(this.markerClassName)) {
			return;
		}
		target.addClass(this.markerClassName);
		var inst = {settings: $.extend({}, this._defaults),
			saveCSS: {position: target.css('position'), color: target.css('color')}};
		$.data(target[0], PROP_NAME, inst);
		this._updateEffect(target, settings);
	},

	/* Reconfigure the settings for a label effect control.
	   @param  target    (element) the control to affect
	   @param  settings  (object) the new options for this instance or
	                     (string) an individual property name
	   @param  value     (any) the individual property value (omit if settings is an object) */
	_changeEffect: function(target, settings, value) {
		target = $(target);
		if (!target.hasClass(this.markerClassName)) {
			return;
		}
		if (typeof settings == 'string') {
			var name = settings;
			settings = {};
			settings[name] = value;
		}
		this._updateEffect(target, settings);
	},

	/* Construct the requested label effect.
	   @param  target    (jQuery) the control to affect
	   @param  settings  (object) the custom options for this instance */
	_updateEffect: function(target, settings) {
		settings = settings || {};
		var inst = $.data(target[0], PROP_NAME);
		settings = extendRemove(extendRemove(inst.settings,
			this._effects[settings.effect] || {}), settings);
		this._removeEffect(target);
		target.css({position: 'relative', color: 'transparent'});
		var template = $('<span></span>').html(target.html());
		this._createEffect(target, template, 'shadow', settings.color,
			settings.shadowColor, settings.shadowDir, settings.shadowOffset,
			settings.shadowFill, settings.shadowBlend);
		this._createEffect(target, template, 'highlight', settings.color,
			settings.hiColor, settings.hiDir, settings.hiOffset,
			settings.hiFill, settings.hiBlend);
		this._createEffect(target, template, 'orig', settings.color,
			settings.color || inst.saveCSS.color, this.UP, 0, false, false);
	},

	/* Create one of the effects.
	   @param  target       (jQuery) the original element
	   @param  template     (jQuery) the template for effect elements
	   @param  effectClass  (string) a class to add to the effect element(s)
	   @param  mainColour   (string) the colour of the main text
	   @param  colour       (string) the colour for this effect
	   @param  dir          (number) the direction for this effect
	   @param  offset       (number) the distance for this effect
	   @param  fill         (boolean) true to fill between text and this effect
	   @param  blend        (boolean) true to blend the colours for this effect */
	_createEffect: function(target, template, effectClass, mainColour,
			colour, dir, offset, fill, blend) {
		if (dir == this.NONE) {
			return;
		}
		var cOffsets = [0, 0, 0];
		var baseRGB = getRGB(blend && offset > 1 ? mainColour : colour);
		if (blend && offset > 1) {
			var endRGB = getRGB(colour);
			cOffsets = [(endRGB[0] - baseRGB[0]) / offset,
				(endRGB[1] - baseRGB[1]) / offset, (endRGB[2] - baseRGB[2]) / offset];
		}
		var blendColour = function(i) {
			return 'rgb(' + Math.round(baseRGB[0] + i * cOffsets[0]) + ',' +
				Math.round(baseRGB[1] + i * cOffsets[1]) + ',' +
				Math.round(baseRGB[2] + i * cOffsets[2]) + ')';
		};
		var scroll = ($.browser.opera ? [document.documentElement.scrollLeft,
			document.documentElement.scrollTop] : [0, 0]);
		for (var i = offset; i >= (fill ? 1 : offset); i--) {
			template.clone().addClass('labelEffect-' + effectClass).
				css({color: blendColour(i), left: i * OFFSETS[dir][0] + scroll[0],
					top: i * OFFSETS[dir][1] + scroll[1]}).
				appendTo(target);
		}
	},

	/* Remove the label effect widget from a control.
	   @param  target  (element) the control to affect */
	_destroyEffect: function(target) {
		target = $(target);
		if (!target.hasClass(this.markerClassName)) {
			return;
		}
		target.removeClass(this.markerClassName);
		this._removeEffect(target);
		var inst = $.data(target[0], PROP_NAME);
		target.css(inst.saveCSS);
		$.removeData(target[0], PROP_NAME);
	},

	/* Remove the label effect additions.
	   @param  target  (jQuery) the control to affect */
	_removeEffect: function(target) {
		var html = target.find('.labelEffect-orig').html();
		target.find('.labelEffect-highlight,.labelEffect-shadow,' +
			'.labelEffect-orig').remove().html(html);
	}
});

/* Parse strings looking for common colour formats.
   @param  colour  (string) colour description to parse
   @return  (number[3|4]) RGB[A] components of this colour */
function getRGB(colour) {
	var result;
	// Check if we're already dealing with an array of colors
	if (colour && colour.constructor == Array && (colour.length == 3 || colour.length == 4)) {
		return colour;
	}
	// Look for rgb(num,num,num)
	if (result = /rgb\(\s*([0-9]{1,3})\s*,\s*([0-9]{1,3})\s*,\s*([0-9]{1,3})\s*\)/.exec(colour)) {
		return [parseInt(result[1], 10), parseInt(result[2], 10), parseInt(result[3], 10)];
	}
	// Look for rgb(num%,num%,num%)
	if (result = /rgb\(\s*([0-9]+(?:\.[0-9]+)?)\%\s*,\s*([0-9]+(?:\.[0-9]+)?)\%\s*,\s*([0-9]+(?:\.[0-9]+)?)\%\s*\)/.exec(colour)) {
		return [parseFloat(result[1]) * 2.55, parseFloat(result[2]) * 2.55,
			parseFloat(result[3]) * 2.55];
	}
	// Look for #a0b1c2
	if (result = /#([a-fA-F0-9]{2})([a-fA-F0-9]{2})([a-fA-F0-9]{2})/.exec(colour)) {
		return [parseInt(result[1], 16), parseInt(result[2], 16), parseInt(result[3], 16)];
	}
	// Look for #abc
	if (result = /#([a-fA-F0-9])([a-fA-F0-9])([a-fA-F0-9])/.exec(colour)) {
		return [parseInt(result[1] + result[1], 16), parseInt(result[2] + result[2], 16),
			parseInt(result[3] + result[3], 16)];
	}
	// Otherwise, we're most likely dealing with a named color
	return COLOURS[$.trim(colour || '').toLowerCase()] || COLOURS['none'];
}

// The HTML/CSS named colours
var COLOURS = {
	'':			[255, 255, 255, 1],
	none:		[255, 255, 255, 1],
	transparent:[255, 255, 255, 0],
	aqua:		[0, 255, 255],
	black:		[0, 0, 0],
	blue:		[0, 0, 255],
	fuchsia:	[255, 0, 255],
	gray:		[128, 128, 128],
	grey:		[128, 128, 128],
	green:		[0, 128, 0],
	lime:		[0, 255, 0],
	maroon:		[128, 0, 0],
	navy:		[0, 0, 128],
	olive:		[128, 128, 0],
	orange:		[255, 165, 0],
	purple:		[128, 0, 128],
	red:		[255, 0, 0],
	silver:		[192, 192, 192],
	teal:		[0, 128, 128],
	white:		[255, 255, 255],
	yellow:		[255, 255, 0]
};

/* Add and/or remove attributes of an object.
   jQuery extend now ignores nulls!
   @param  target  (object) the object to amend
   @param  props   (object) the attributes to copy
   @return  (object) the updated object */
function extendRemove(target, props) {
	$.extend(target, props);
	for (var name in props) {
		if (props[name] == null) {
			target[name] = null;
		}
	}
	return target;
}

/* Attach the label effect functionality to a jQuery selection.
   @param  command  (string) the command to run (optional, default 'attach')
   @param  options  (object) the new settings to use for these label effect instances (optional)
   @return  (jQuery) for chaining further calls */
$.fn.labeleffect = function(options) {
	var otherArgs = Array.prototype.slice.call(arguments, 1);
	return this.each(function() {
		if (typeof options == 'string') {
			$.labeleffect['_' + options + 'Effect'].
				apply($.labeleffect, [this].concat(otherArgs));
		}
		else {
			$.labeleffect._attachEffect(this, options || {});
		}
	});
};

/* Initialise the label effects functionality. */
$.labeleffect = new LabelEffects(); // singleton instance

if ($.browser.opera) {
	$(document).scroll(function() { // Re-apply after scroll
		$('.' + $.labeleffect.markerClassName).each(function() {
			if (!this.nodeName.toLowerCase().match(/^div$|^p$|^h[1-6]$/)) {
				$(this).labeleffect('change', {});
			}
		});
	});
}

})(jQuery);
