exports.setStyle = function (style) {
  return function (elt) {
    return function () {
      elt.style = style;
      return {};
    };
  };
};

exports.getValue = function(e) {
  return function(){
    return e.value;
  }
}

exports.getDeltaY = function(e) {
  return function(){
    return e.deltaY;
  }
}

exports.autosize = function(e) {
  return function(){
    e.oninput = function(){
      this.style = 'width:'+Math.max(10,this.value.length+1)+'ch';
    }
    e.style = 'width:'+Math.max(10,e.value.length+1)+'ch';
    e.focus();
    return {};
  }
};

exports.vegaInPlace = function(e){
  return function(jsonString){
    return function(){
      var spec = JSON.parse(jsonString);
      vegaEmbed(e, spec, {"actions": false});
      return {};
    }
  }
}
