'use strict';

// module Cell

exports.clearDragImage = function(ev) {
  return function(){
    var img = new Image();
    ev.dataTransfer.setDragImage(img, 10, 10);
  }
}

exports.setEmptyData = function(ev) {
  return function(){
    var img = new Image();
    ev.dataTransfer.setData('text', 'empty');
    ev.dataTransfer.effectAllowed = "move";
  }
}
