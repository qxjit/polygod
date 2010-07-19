$(window).load(function() {
  $('#game-canvas').gameCanvas();
  $('.toolbar').buttonset();
  $('#pattern-box').patternBox($('#game-canvas')[0].cellSize, [
    {size: [3,3], points: [ [1,0],[2,1],[0,2],[1,2],[2,2] ]},
    {size: [2,3], points: [ [0,0],[0,1],[1,0],[1,1] ]},
  ]);
});

jQuery.fn.cellCanvas = function(width, height, options) {
  this.each(function() {
    if (options && options.cellSize) {
      this.cellSize = options.cellSize;
    } else {
      var maximumCellWidth = $(window).width() * 0.7 / width;
      var maximumCellHeight = $(window).height() * 0.7 / height;

      this.cellSize =  Math.floor(Math.min(maximumCellWidth, maximumCellHeight));
    }

    this.width =  width * this.cellSize;
    this.height = height * this.cellSize;

    var context = this.getContext('2d');
    var gradients = {};

    this.paintCell = function(point, alive) {
      var x = point[0]*this.cellSize;
      var y = point[1]*this.cellSize;

      if (!gradients[x]) {
        gradients[x] = {};
      }

      var gradient;
      if (gradients[x][y]) {
        gradient = gradients[x][y];
      } else {
        gradient = gradients[x][y] = context.createLinearGradient(x, y, x + this.cellSize, y + this.cellSize);
        gradient.addColorStop(0, '#FFD859');
        gradient.addColorStop(1, '#FF0000');
      }

      if (alive) {
        context.fillStyle = gradient;
        context.fillRect(x, y, this.cellSize, this.cellSize);
      } else {
        context.clearRect(x, y, this.cellSize, this.cellSize);
      }
    };

    this.toImage = function() {
      var img = $('<img/>');
      img.attr('src', this.toDataURL());
      return img;
    };
  });
};

jQuery.fn.gameCanvas = function() {
  this.each(function() {
    $(this).cellCanvas(this.getAttribute('data-width'), this.getAttribute('data-height'));
    var canvasElem = this;
    var repaintWorld = function(world) {
      var max = world.cells.length;
      for (var i = 0; i < max; i++) {
        var cell = world.cells[i];
        canvasElem.paintCell(cell.point, cell.alive);
      }

      polygod.getWorld(repaintWorld, world.tick);
    };

    var worldCoordinates = function(x, y) {
      var canvasOffset = $(canvasElem).offset();
      return {
        x: Math.floor( (x - canvasOffset.left) / canvasElem.cellSize),
        y: Math.floor( (y - canvasOffset.top) / canvasElem.cellSize)
      }

    };

    var updateWorld = function(event) {
      var coords = worldCoordinates( event.pageX, event.pageY );
      polygod.postCells([ { point: [coords.x, coords.y], alive: $('input:radio[name=tool]:checked').val() == "resurrect"} ]);
    };

    $(canvasElem).mousedown(function(event) {
      updateWorld(event);
      $(canvasElem).mousemove(updateWorld);
    });

    $(canvasElem).mouseup(function(event) {
      $(canvasElem).unbind('mousemove', updateWorld);
    });

    $(canvasElem).droppable({
      drop: function(event, ui) {
        polygod.postCells(
          ui.draggable.data('cellsForDrop')(worldCoordinates(ui.offset.left, ui.offset.top))
        );
      }
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
};

jQuery.fn.patternBox = function(cellSize, patterns) {
  var list = $('<ul/>');

  $(patterns).each(function() {
    var pattern = this;

    var canvas = $('<canvas/>');
    var canvasElem = canvas[0];
    var cellCanvas = canvas.cellCanvas(pattern.size[0], pattern.size[1], {cellSize: cellSize});
    $(pattern.points).each(function() { canvasElem.paintCell(this, true)});

    $(canvas).draggable({ helper: function() { return canvasElem.toImage(); }});
    $(canvas).data('cellsForDrop', function(coords) {
      return $(pattern.points).map(function() { return { point: [this[0] + coords.x, this[1] + coords.y], alive: true }}).toArray();
    });

    var item = $('<li/>');
    item.append(canvas);
    list.append(item);
  });

  this.append(list);
}

var polygod = {
  postCells: function(cells) {
    $.ajax({
      type: 'POST',
      url: 'world',
      data: JSON.stringify({cells: cells }),
      cache: false,
      error: function(req, status, error) {
        alert("Ajax error Updating World!");
      }
    });
  },

  getWorld: function(successCallback, tick) {
    $.ajax({
      url: 'world/next.json?tick=' + tick,
      dataType: 'json',
      success: successCallback,
      cache: false,
      error: function(req, status, error) {
        alert("Ajax error Updating World!");
      }
    });
  },
};
