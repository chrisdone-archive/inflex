// src/Halogen/CodeMirror.js
"use strict";

var mirrors = {};

exports.setOnEnter = function(codemirror){
  return function(f){
    return function(){
      codemirror.myonenter = f;
      return {};
    }
  }
}

exports.codeMirror = function(parent){
  return function(config){
    return function(){
      config.viewportMargin = Infinity;
      config.extraKeys = {
        "Enter": function(cm){
          if (cm.myonenter) {
            cm.myonenter();
          }
        }
      };
      let cm = CodeMirror(parent, config);
      return cm;
    };
  };
}

exports.on = function(codemirror){
  return function(event){
    return function(callback){
      return function(){
        codemirror.on(event,function(doc, change){
          if (!change || change.origin != 'setValue')
            callback();
        });
      }
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
