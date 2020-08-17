'use strict';

// module Inflex.Doc

exports.getValue = function(e) {
  return function(){
    return e.value;
  }
}
