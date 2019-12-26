var env = {};

env.dirty = false;
env.history = [];
env.maxHistoryLength = 15;

env.editor = ace.edit("editor");
env.editor.setTheme("ace/theme/textmate");
env.editor.getSession().setMode("ace/mode/html");
env.editor.setHighlightActiveLine(false);
env.editor.setDisplayIndentGuides(false);
env.editor.renderer.setShowPrintMargin(false);
env.editor.session.setFoldStyle("manual");
env.editor.renderer.setVScrollBarAlwaysVisible(true);


//

function clearContent() {
    var presentation = document.getElementById("presentation");
    presentation.src = "about:blank";
}

function runProgram() {
    var presentation = document.getElementById("presentation").contentWindow;
    if (presentation.Pengine) presentation.Pengine.destroy_all(true);
    presentation.document.open();
    presentation.document.write(getProgram());
    presentation.document.close();
}


// Getting and setting program

function getProgram() {
    return env.editor.getValue()
}

function setProgram(src) {
	env.editor.setValue(src, -1);
	env.dirty = false;
	// $('#run-btn').prop('disabled', false);
}

// Printing

function print_editor_content() {
	var iframe = document.createElement("iframe");
	iframe.style.display = "none"
	document.body.appendChild(iframe)
	var windw = iframe.contentWindow;
	windw.document.open();
    windw.document.write('</head><body><pre>');
    windw.document.write(escapeHtml(getProgram()));
    windw.document.write('</pre></body></html>');
    windw.print();
    windw.document.close();
    document.body.removeChild(iframe);
}

var entityMap = {
    "&": "&amp;",
    "<": "&lt;",
    ">": "&gt;",
    '"': '&quot;',
    "'": '&#39;',
    "/": '&#x2F;'
  };

function escapeHtml(string) {
    return String(string).replace(/[&<>"'\/]/g, function (s) {
      return entityMap[s];
    });
  }


// Handling programs

function maybeLoadSrc() {
    var file = window.location.hash.slice(1);
    if (file) {
        loadSrc("/storage/"+ encodeURIComponent(file));
    }
}

function loadSrc(url) {
    $.get(url)
    .done(function(program) {
		setProgram(program);
	})
	.fail(function() {
		alert('Error: ' + url + ' does not exist.');
	})
}

// Event handlers: Editor

env.editor.getSession().on('change', function() {
	if (!env.dirty) {
	    env.dirty = true;
		// $('#run-btn').prop('disabled', true);
	}
});


// Event handlers: Menus

$("#example-menu").on("click", "a", function(evt) {
	evt.preventDefault();
	clearContent();
	window.location.hash = "";
	loadSrc(evt.target.href);
});


// Non-menu controls

$("#run-btn").on("click", runProgram);


$("#slider").on("input", function() {
    var val = this.value;
    $("#editor").css("width", val+"%");
    $("#console").css("width", (100-val)+"%");
    if (val > 69) {
        $("#console").css("display","none");
        $("#editor").css("width", "100%");
    } else {
        $("#console").css("display","block");
    }
    if (val < 31) {
        $("#editor").css("display","none");
        $("#console").css("width", "100%");
    } else {
        $("#editor").css("display","block");
    }
});

$("#show-checkbox").on("change", function() {
	var value = $("#show-checkbox").prop('checked');
	if (localStorage) {
		localStorage['scratchpad-show-about'] = !value;
	}
});



// GUI preferences

function setTheme(theme) {
	env.editor.setTheme("ace/theme/" + theme);
	$("#theme-menu option:selected").prop("selected", false);
	$("#theme-menu").find("option[value='" + theme +"']").prop("selected", true);
}

function setFontFamily(family) {
	$('#editor').css('fontFamily', family);
	$("#font-family-menu option:selected").prop("selected", false);
	$("#font-family-menu").find("option[value='" + family +"']").prop("selected", true);
}

function setFontSize(size) {
	$('#editor').css('fontSize', size + 'px');
	$("#font-size-menu option:selected").prop("selected", false);
	$("#font-size-menu").find("option[value=" + size +"]").prop("selected", true);
}

function setTabSize(n) {
	env.editor.getSession().setTabSize(n);
	$("#tab-size-menu option:selected").prop("selected", false);
	$("#tab-size-menu").find("option[value=" + n +"]").prop("selected", true);
}

function setUseSoftTabs(bool) {
	env.editor.getSession().setUseSoftTabs(bool);
	$("#tab-soft-checkbox").prop('checked', bool);
}

function setLineWrap(bool) {
	env.editor.getSession().setUseWrapMode(bool);
	$("#line-wrap-checkbox").prop('checked', bool);
}

function setLineHighlight(bool) {
	env.editor.setHighlightActiveLine(bool);
	$("#line-highlight-checkbox").prop('checked', bool);
}

function setShowGutter(bool) {
	env.editor.renderer.setShowGutter(bool);
	$("#line-numbering-checkbox").prop('checked', bool);
}


// Event handlers: Preferences

$("#theme-menu").on("change", function() {
	var value = $("#theme-menu option:selected").val();
	setTheme(value);
	if (localStorage) {
		localStorage['scratchpad-theme'] = value;
	}
});

$("#font-family-menu").on("change", function() {
	var value = $("#font-family-menu option:selected").val();
	setFontFamily(value);
	if (localStorage) {
		localStorage['scratchpad-font-family'] = value;
	}
});

$("#font-size-menu").on("change", function() {
	var value = $("#font-size-menu option:selected").val();
	setFontSize(parseInt(value, 10));
	if (localStorage) {
		localStorage['scratchpad-font-size'] = value;
	}
});

$("#tab-size-menu").on("change", function() {
	var value = $("#tab-size-menu option:selected").val();
	setTabSize(parseInt(value, 10));
	if (localStorage) {
		localStorage['scratchpad-tab-size'] = value;
	}
});


function parseBoolean(value) {
	return value === "true" ? true : false;
}

$(document).ready(function() {
    var showAbout = true;
	if (localStorage && localStorage.length > 0) {
		setTheme(localStorage['scratchpad-theme']);
	    setFontFamily(localStorage['scratchpad-font-family']);
	    setFontSize(localStorage['scratchpad-font-size']);
	    setTabSize(parseInt(localStorage['scratchpad-tab-size'], 10));
	    setLineWrap(parseBoolean(localStorage['scratchpad-line-wrap']) || true);
	    setLineHighlight(parseBoolean(localStorage['scratchpad-line-highlight']) || false);
	    setShowGutter(parseBoolean(localStorage['scratchpad-line-numbering']) || true);
	    setUseSoftTabs(parseBoolean(localStorage['scratchpad-tab-soft']) || true);
	    showAbout = parseBoolean(localStorage['scratchpad-show-about']);
	}
    if (showAbout) $('#about').modal();
    maybeLoadSrc();
});

