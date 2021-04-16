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

exports.setOnEntered = function(codemirror){
  return function(f){
    return function(){
      codemirror.onenter = f;
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

exports.setOnPicked = function(codemirror){
  return function(f){
    return function(){
      codemirror._onPickHook = f;
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
        config.highlightSelectionMatches = {showToken: true};
      let comp = config.namesInScope;
      function isPrefixOf(sub,sup){
        if (sub.length == 0) throw 'invalid substring';
  	return (
          sub.length <= sup.length &&
            sup.substring(0, sub.length) == sub
        )
      }
      function getPrefixMatches(cm, option) {
        return new Promise(function(accept) {
          var cursor = cm.getCursor(), line = cm.getLine(cursor.line)
          var start = cursor.ch, end = cursor.ch
          while (start && /\w/.test(line.charAt(start - 1))) --start
          while (end < line.length && /\w/.test(line.charAt(end))) ++end
          var word = line.slice(start, end).toLowerCase()
          if (word.length == 0) return accept(null);
          let candidates = [];
          for (var i = 0; i < comp.length; i++) {
            // TODO: Later these could be lower cased ahead of time.
            if (isPrefixOf(word, comp[i].matchText.toLowerCase())) {
              candidates.push(comp[i]);
            }
          }
          if (candidates.length > 0) {
            return accept({list: candidates,
                           from: CodeMirror.Pos(cursor.line, start),
                           to: CodeMirror.Pos(cursor.line, end)})
          } else {
            return accept(null)
          }
        })
      }
      config.hintOptions = {
        hint: getPrefixMatches,
        updateOnCursorActivity: true
      };
      config.extraKeys = {
        "Enter": function(inst) {
          cm.onenter();
        }
      };
      ////////////////////////////////////////////////////////////////////////////////
      let cm = CodeMirror(parent, config);
      cm._onPick = function(completion){
        cm._onPickHook(completion.key)();
      }
      cm.on("inputRead", function (cm, event) {
        /*Enter - do not open autocomplete list just after item has been selected in it*/
        if (!cm.state.completionActive) {
          CodeMirror.commands.autocomplete(cm, null, {completeSingle: false});
        }
      });
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

exports.completionActive = function(cm){
  return function(){
    return cm.state.completionActive;
  }
};

exports.markText = function(codemirror){
  return function(start){
    return function(end){
      return function(opts){
        return function(){
          if (opts.replaceText) {
            var el = document.createElement('span');
            el.innerText = opts.replaceText;
            el.className = 'uuid-overlay';
            opts.replacedWith = el
          }
          codemirror.getDoc().markText(start, end, opts);
          return {};
        }
      }
    }
  }
}
