exports.newConnector = function(element){
  return function(){
    if (element._connector) {
      console.debug('already has a connector, ignoring');
      return element._connector;
    } else {
      element._connector = true;
      function trigger(){
        window.requestAnimationFrame(function(){
          var box = element.getBoundingClientRect();
          var basex = box.left;
          var basey = box.top;
          var arrows = element.getElementsByClassName('connector-arrow');
          for (var i = 0; i < arrows.length; i++) {
            var from = document.getElementById(arrows[i].dataset.from_id);
            var to   = document.getElementById(arrows[i].dataset.to_id);

            let fromr = from.getBoundingClientRect();
            let tor   = to.getBoundingClientRect();

            arrows[i].x1.baseVal.value = -basex + fromr.left + fromr.width/2;
            arrows[i].y1.baseVal.value = -basey + fromr.top  + fromr.height/2;

            var targetx = 0, targety = 0;
            if (tor.left > fromr.left + fromr.width) {
              targetx = tor.left;
              targety = tor.top + tor.height/2;
            } else if (tor.left + tor.width < fromr.left) {
              targetx = tor.left + tor.width - 7;
              targety = tor.top + tor.height/2;
            } else if (tor.top < fromr.top) {
              targetx = tor.left + tor.width/2;
              targety = tor.top + tor.height-7;
            } else {
              targetx = tor.left + tor.width/2;
              targety = tor.top;
            }

            arrows[i].x2.baseVal.value = -basex + targetx;
            arrows[i].y2.baseVal.value = -basey + targety;
          }
        })
      }
      setInterval(function(){
        trigger();
      }, 1000);
      return {
        trigger: trigger
      };
    }
  }
}
