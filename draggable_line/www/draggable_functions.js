/**
  * Experimental Draggable points plugin
* Revised 2012-02-08
*
  * On Saving this jsbin, remember to update http://jsfiddle.net/highcharts/AyUbx/
  */
  (function(Highcharts) {
    var addEvent = Highcharts.addEvent,
    each = Highcharts.each;
    
    /**
      * Filter by dragMin and dragMax
    */
      
      function filterRange(newY, series) {
        var options = series.options,
        dragMin = options.dragMin,
        dragMax = options.dragMax;
        
        if (newY < dragMin) {
          newY = dragMin;
        } else if (newY > dragMax) {
          newY = dragMax;
        }
        return newY;
      }
    
    function redrawColumns(dragPoint) {
      var thresholder = null,
      thresholded = null,
      data = [],
      thresholdData = [];
      
      $(dragPoint.series.chart.series).each(function(ind, ser) {
        if (ser.options.type === 'column' && !ser.options.isThresholder) { /* default series */
                                                                             thresholded = ser;
                                                                           var len = ser.data.length; 
                                                                           $(ser.data).each(function(ii, point){
                                                                             if(data.length !== len  ){
                                                                               data.push(point.y > dragPoint.y ? dragPoint.y : point.y);
                                                                               
                                                                               thresholdData.push(point.y > dragPoint.y ? point.y - dragPoint.y : 0);
                                                                             } else {
                                                                               thresholdData.push(data[ii] + point.y > dragPoint.y ? data[ii] + point.y - dragPoint.y : 0);
                                                                               data[ii] = data[ii] + point.y > dragPoint.y ? dragPoint.y : data[ii] + point.y;
                                                                               
                                                                               
                                                                             }
                                                                           });
        } else if(ser.options.type === 'column' && ser.options.isThresholder) {
          $(ser.data).each(function(ii, point){
            data.push(point.y);
          });
          thresholder = ser;
        }
      });
      if(thresholder && thresholded) {            thresholder.setData(thresholdData,false);
                                                  thresholded.setData(data,false);
      }
      
    }
    
    Highcharts.Chart.prototype.callbacks.push(function(chart) {
      
      var container = chart.container,
      dragPoint, dragY, dragPlotY;
      
      chart.redraw(); // kill animation (why was this again?)
      addEvent(container, 'mousedown', function(e) {
        var hoverPoint = chart.hoverPoint;
        if (hoverPoint && hoverPoint.series.options.draggable) {
          dragPoint = hoverPoint;
          dragY = e.pageY;
          dragPlotY = dragPoint.plotY + (chart.plotHeight - (dragPoint.yBottom || chart.plotHeight));
        }
      });
      
      addEvent(container, 'mousemove', function(e) {
        if (dragPoint) {
          var deltaY = dragY - e.pageY,
          newPlotY = chart.plotHeight - dragPlotY + deltaY,
          newY = dragPoint.series.yAxis.translate(newPlotY, true),
          series = dragPoint.series;
          
          newY = filterRange(newY, series);
          if (dragPoint.series.options.draggableSeries) {
            series.data[0].update(newY, false);
            series.data[1].update(newY, false);
          } else {
            dragPoint.update(newY, false);
          }
          chart.tooltip.refresh(dragPoint);
          if (series.stackKey) {
            chart.redraw();
          } else {
            series.redraw();
          }
        }
      });
      
      function drop(e) {
        if (dragPoint) {
          var deltaY = dragY - e.pageY,
          newPlotY = chart.plotHeight - dragPlotY + deltaY,
          series = dragPoint.series,
          newY = series.yAxis.translate(newPlotY, true);
          
          newY = filterRange(newY, series);
          
          redrawColumns(dragPoint);
          dragPoint.firePointEvent('drop');
          dragPoint.update(newY);
          dragPoint = dragY = undefined;
        }
      }
      addEvent(document, 'mouseup', drop);
      addEvent(container, 'mouseleave', drop);
    });
    
    /**
      * Extend the column chart tracker by visualizing the tracker object for small points
    */
      /* var colProto = Highcharts.seriesTypes.column.prototype, baseDrawTracker = colProto.drawTracker;
    
    colProto.drawTracker = function() {
      var series = this;
      baseDrawTracker.apply(series);
      
      each(series.points, function(point) {
        point.tracker.attr(point.shapeArgs.height < 3 ? {
          'stroke': 'black',
          'stroke-width': 2,
          'dashstyle': 'shortdot'
        } : {
          'stroke-width': 0
        });
      });
    };
    */
  })(Highcharts);
