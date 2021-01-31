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
