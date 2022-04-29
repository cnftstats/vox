// hover_tooltip.js
function(el) {
  var tooltip = d3.select('#' + el.id + ' .svg-container')
  .append("div")
  .attr("class", "my-custom-tooltip");

  var dragLayer = document.getElementsByClassName('nsewdrag')[0]; // cursor pointer over datapoints

  el.on('plotly_hover', function(d) {
    // Parse customdata passed with R
    var custom_data = JSON.parse(d.points[0].customdata);
    console.log(custom_data);

    // Choose a location (on the data scale) to place the image
    var x0 = d.points[0].xaxis.range[0];
    var x1 = d.points[0].xaxis.range[1];
    var y0 = d.points[0].yaxis.range[0];
    var y1 = d.points[0].yaxis.range[1];

    var x_range = x1 - x0;
    var y_range = y1 - y0;

    var x = x1;
    var y = y1;
    
    // Transform the data scale to the pixel scale
    var xPixel = d.points[0].xaxis.l2p(x) + d.points[0].xaxis._offset + 10;
    var yPixel = d.points[0].yaxis.l2p(y) + d.points[0].yaxis._offset;

    // Insert tooltip
    tooltip.html(custom_data.mytooltip)
      .style("position", "absolute")
      .style("left", xPixel + "px")
      .style("top", yPixel + "px");
    
    tooltip.transition()
      .duration(0)
      .style("opacity", 1);
    
    // cursor pointer over datapoints
    dragLayer.style.cursor = 'pointer';
  });

  el.on('plotly_unhover', function(d) {
    // Fade out the image
    tooltip.transition()
      .duration(0)
      .style("opacity", 0);

    // cursor pointer over datapoints
    dragLayer.style.cursor = '';
  });

  el.on('plotly_click', function(d) {
    var market_link = JSON.parse(d.points[0].customdata).market_link;
    if (market_link != "") window.open(market_link); // only open link if there is one
  });
}
