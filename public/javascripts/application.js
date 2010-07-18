$(window).load(function() {
  var canvas = $('#game-canvas')[0]

  var worldWidth = canvas.getAttribute('data-width');
  var worldHeight = canvas.getAttribute('data-height');

  var frame = $(window);

  var maximumCellWidth = frame.width() * 0.8 / worldWidth;
  var maximumCellHeight = frame.height() * 0.7 / worldHeight;

  var cellSize = Math.floor(Math.min(maximumCellWidth, maximumCellHeight));

  canvas.width = worldWidth * cellSize;
  canvas.height = worldHeight * cellSize;

  var context = canvas.getContext('2d');
  context.fillStyle = "#FF0000";

  var repaintWorld = function(world) {
    $(world.cells).each(function(index) {
      var x = this.point[0]*cellSize;
      var y = this.point[1]*cellSize;
      if (this.alive) {
        context.fillRect(x, y, cellSize, cellSize);
      } else {
        context.clearRect(x, y, cellSize, cellSize);
      }
    });

    $.ajax({
      url: 'world/next.json?tick=' + world.tick,
      dataType: 'json',
      success: repaintWorld,
      cache: false,
      error: function(req, status, error) {
        alert("Ajax error Updating World!");
      }
    });
  };

  var updateWorld = function(event) {
    var offset = $(canvas).offset();
    var worldX = Math.floor( (event.pageX - offset.left) / cellSize);
    var worldY = Math.floor( (event.pageY - offset.top) / cellSize);

    $.ajax({
      type: 'POST',
      url: 'world',
      data: JSON.stringify({cells: [ { point: [worldX, worldY], alive: true} ] }),
      cache: false,
      error: function(req, status, error) {
        alert("Ajax error Updating World!");
      }
    });
  };

  $(canvas).mousedown(function(event) {
    updateWorld(event);
    $(this).mousemove(updateWorld);
  });

  $(canvas).mouseup(function(event) {
    $(this).unbind('mousemove', updateWorld);
  });

  setTimeout(function() {
    $.ajax({
      url: 'world/current.json',
      dataType: 'json',
      success: repaintWorld,
      cache: false,
      error: function(req, status, error) {
        alert("Ajax error Loading World!");
      }
    });
  }, 100);
});
