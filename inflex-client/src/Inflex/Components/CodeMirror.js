// src/Halogen/CodeMirror.js
"use strict";

var mirrors = {};

exports.setOnKeyHandled = function(codemirror){
  return function(f){
    return function(){
      codemirror.on('keyHandled',function(cm, string, event){
        f(string)();
      });
      return {};
    }
  }
}

exports.setOnInputRead = function(codemirror){
  return function(f){
    return function(){
      codemirror.on('inputRead',function(cm, change){
        f();
      });
      return {};
    }
  }
}

exports.setOnFocused = function(codemirror){
  return function(f){
    return function(){
      codemirror.on('focus',function(){ f(); });
      return {};
    }
  }
}

exports.setOnBlurred = function(codemirror){
  return function(f){
    return function(){
      codemirror.on('blur',function(){ f(); });
      return {};
    }
  }
}

exports.setOnCursorActivity = function(codemirror){
  return function(f){
    return function(){
      codemirror.on('cursorActivity',function(){ f(); });
      return {};
    }
  }
}

exports.codeMirror = function(parent){
  return function(config){
    return function(){
      config.viewportMargin = Infinity;
      if (config.highlightSelectionMatches)
        config.highlightSelectionMatches = {showToken: /[A-Za-z][A-Za-z0-9_]*/};

      ////////////////////////////////////////////////////////////////////////////////
      // comp
      var comp = [
        ["here", "hither"],
        ["he","him"],
        ["asynchronous", "nonsynchronous"],
        ["completion", "achievement", "conclusion", "culmination", "expirations"],
        ["hinting", "advive", "broach", "imply"],
        ["function","action"],
        ["provide", "add", "bring", "give"],
        ["synonyms", "equivalents"],
        ["words", "token"],
        ["each", "every"],
      ]
      function synonyms(cm, option) {
        return new Promise(function(accept) {
          setTimeout(function() {
            var cursor = cm.getCursor(), line = cm.getLine(cursor.line)
            var start = cursor.ch, end = cursor.ch
            while (start && /\w/.test(line.charAt(start - 1))) --start
            while (end < line.length && /\w/.test(line.charAt(end))) ++end
            var word = line.slice(start, end).toLowerCase()
            for (var i = 0; i < comp.length; i++) if (comp[i].indexOf(word) != -1)
              return accept({list: comp[i],
                             from: CodeMirror.Pos(cursor.line, start),
                             to: CodeMirror.Pos(cursor.line, end)})
            return accept(null)
          }, 100)
        })
      }
      config.extraKeys = {"Ctrl-Space": "autocomplete"};
      config.hintOptions = {hint: synonyms, updateOnCursorActivity: true};
      ////////////////////////////////////////////////////////////////////////////////


      let cm = CodeMirror(parent, config);
      return cm;
    };
  };
}

exports.getValue = function(codemirror){
  return function(){
    return codemirror.getValue();
  };
}

exports.setValue = function(codemirror){
  return function(string){
    return function(){
      return codemirror.setValue(string);
    };
  };
}

exports.getSelection = function(codemirror){
  return function(){
    if (codemirror.listSelections().length < 1)
      throw "assert failed: codemirror.listSelections.length";
    return codemirror.listSelections()[0];
  };
}

exports.scrollToLine = function(codemirror) {
  return function(i){
    return function(){
      var t = codemirror.charCoords({line: i, ch: 0}, "local").top;
      var middleHeight = codemirror.getScrollerElement().offsetHeight / 2;
      codemirror.scrollTo(null, t - middleHeight - 5);
    };
  };
}

exports.addKeyMap = function(codemirror){
  return function(name){
    return function(object){
      return function(){
        var obj = object.clone();
        obj.name = name
        codemirror.addKeyMap(obj);
        return {};
      }
    }
  }
}

exports.removeKeyMap = function(codemirror){
  return function(name){
    return function(){
      codemirror.removeKeyMap(name);
      return {};
    }
  }
}

exports.keyHandled = undefined;

exports.keyPass = CodeMirror.Pass;
