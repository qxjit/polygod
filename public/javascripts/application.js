$(window).load(function() {
  $('#game-canvas').each(setupCanvas);
  $('.toolbar').each(setupToolbar);
});

function setupCanvas() {
  var canvas = this;
  var worldWidth = canvas.getAttribute('data-width');
  var worldHeight = canvas.getAttribute('data-height');

  var maximumCellWidth = $(window).width() * 0.8 / worldWidth;
  var maximumCellHeight = $(window).height() * 0.7 / worldHeight;

  var cellSize = Math.floor(Math.min(maximumCellWidth, maximumCellHeight));

  canvas.width = worldWidth * cellSize;
  canvas.height = worldHeight * cellSize;

  var context = canvas.getContext('2d');

  var repaintWorld = function(world) {
    $(world.cells).each(function(index) {
      var x = this.point[0]*cellSize;
      var y = this.point[1]*cellSize;

      var gradient = context.createLinearGradient(x, y, x + cellSize, y + cellSize);
      gradient.addColorStop(0, '#FFD859');
      gradient.addColorStop(1, '#FF0000');
      context.fillStyle = gradient;

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
      data: JSON.stringify({cells: [ { point: [worldX, worldY], alive: $('input:radio[name=tool]:checked').val() == "resurrect"} ] }),
      cache: false,
      error: function(req, status, error) {
        alert("Ajax error Updating World!");
      }
    });
  };

  $(canvas).mousedown(function(event) {
    updateWorld(event);
    $(canvas).mousemove(updateWorld);
  });

  $(canvas).mouseup(function(event) {
    $(canvas).unbind('mousemove', updateWorld);
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
};

function setupToolbar() {
  $(this).buttonset();
};
