exports.newDragger = function(element){
  return function(connector){
    return function(){
      if (element._dragger) {
        // console.debug('already a dragger, ignoring');
        return element._dragger;
      } else {
        var dragger = {
          xOffset: 0,
          yOffset: 0,
          active: false
        };
        element._dragger = dragger;
        element.addEventListener("touchend", dragEnd, false);
        element.addEventListener("touchmove", drag, false);
        element.addEventListener("mouseup", dragEnd, false);
        element.addEventListener("mousemove", drag, false);

        var currentX;
        var currentY;

        function dragEnd(e) {
          if (dragger.dragme) {
            var rect = dragger.dragme.getBoundingClientRect();
            var parent_rec = element.getBoundingClientRect();

            let left = Math.round((rect.left+element.scrollLeft)-parent_rec.left);
            let top = Math.round((rect.top+element.scrollTop)-parent_rec.top);

            dragger.tellme(left)(top)();

            dragger.initialX = undefined;
            dragger.initialY = undefined;
            dragger.xOffset = 0;
            dragger.yOffset = 0;
            dragger.active = false;
            dragger.dragme = undefined;
          }
        }

        function drag(e) {
          if (dragger.active) {
            // console.log('initialX: %o, initialY: %o', dragger.initialX, dragger.initialY);

            e.preventDefault();

            if (e.type === "touchmove") {
              currentX = e.touches[0].clientX - dragger.initialX;
              currentY = e.touches[0].clientY - dragger.initialY;
            } else {
              currentX = e.clientX - dragger.initialX;
              currentY = e.clientY - dragger.initialY;
            }

            dragger.xOffset = currentX;
            dragger.yOffset = currentY;

            setTranslate(currentX, currentY, dragger.dragme);

            connector.trigger();
          }
        }

        function setTranslate(xPos, yPos, el) {
          el.style.transform = "translate3d(" + xPos + "px, " + yPos + "px, 0)";
        }

        // console.log('new dragger: %o', dragger);
        return dragger;
      }
    }
  }
}

exports.attach = function(actuator){
  return function(levels){
    return function(dragger){
      return function(tellme){
        return function(){
          // console.log('dragger: %o', dragger);
          var dragme = actuator;
          for (var i = 0; i < levels; i++) {
            dragme = dragme.parentNode;
          }
          if (dragme._draggable) {
            // console.debug('already draggable, ignoring');
            return {};
          } else {
            dragme._draggable = true;
            actuator.addEventListener("mousedown", dragStart, false);
            actuator.addEventListener("touchstart", dragStart, false);
            function dragStart(e){
              // console.log('mousedown');
              if (e.type === "touchstart") {
                dragger.initialX = e.touches[0].clientX - dragger.xOffset;
                dragger.initialY = e.touches[0].clientY - dragger.yOffset;
              } else {
                dragger.initialX = e.clientX - dragger.xOffset;
                dragger.initialY = e.clientY - dragger.yOffset;
              }
              if (e.target === actuator) {
                // console.log('starting a drag');
                dragger.active = true;
                dragger.dragme = dragme;
                dragger.tellme = tellme;
              }
            }
          }
        }
      }
    }
  }
}
