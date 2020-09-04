'use strict';

// module Cell

exports.clearDragImage = function(ev) {
  return function(){
    var element = document.createElement('p');
    document.body.appendChild(element);
    // var img = new Image();
    ev.dataTransfer.setDragImage(element, 0, 0);
  }
}

exports.setEmptyData = function(ev) {
  return function(){
    var img = new Image();
    ev.dataTransfer.setData('text', 'empty');
    ev.dataTransfer.effectAllowed = "move";
  }
}
