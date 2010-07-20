$(window).load(function() {
  $('#game-canvas').gameCanvas();
  $('.toolbar').buttonset();
  $('#pattern-box').patternBox({
    cellSize: $('#game-canvas').data('cellCanvas').cellSize,
    patterns: [
      {size: [3,3], points: [       [1,0],
                                          [2,1],
                              [0,2],[1,2],[2,2] ]},

      {size: [2,2], points: [ [0,0],[1,0],
                              [0,1],[1,1] ]},

      {size: [7,3], points: [       [1,0],
                                                [3,1],
                              [0,2],[1,2],            [4,2],[5,2],[6,2] ]},

      {size: [4,3], points: [       [1,0],
                              [0,1],[1,1],[2,1],
                              [0,2],      [2,2],[3,2] ]}
    ]
  });
});

$.widget("ui.cellCanvas", {
  options: {
    width: null,
    height: null,
    cellSize: null
  },

  _create: function() {
    this.canvasElem = this.element[0];

    if (!this.options.cellSize) {
      this.cellSize = this.element.width() / this.options.width;
      $.log("1 Initializing canvas based on element width %d", this.element.width());
    } else {
      this.cellSize = this.options.cellSize;
    }


    this.width = this.options.width;
    this.height = this.options.height;
    this.canvasElem.width   = this.width * this.cellSize;
    $.log("2 Initializing canvas based on element width %d", this.element.width());
    this.canvasElem.height  = this.height * this.cellSize;
    $.log("3 Initializing canvas based on element width %d", this.element.width());
    this._context = this.canvasElem.getContext('2d');

  },

  _gradients: {},
  paintCell: function(point, alive) {
    var x = point[0]*this.cellSize;
    var y = point[1]*this.cellSize;

    if (!this._gradients[x]) {
      this._gradients[x] = {};
    }

    var gradient;
    if (this._gradients[x][y]) {
      gradient = this._gradients[x][y];
    } else {
      gradient = this._gradients[x][y] = this._context.createLinearGradient(x, y, x + this.cellSize, y + this.cellSize);
      gradient.addColorStop(0, '#FFD859');
      gradient.addColorStop(1, '#FF0000');
    }

    if (alive) {
      this._context.fillStyle = gradient;
      this._context.fillRect(x, y, this.cellSize, this.cellSize);
    } else {
      this._context.clearRect(x, y, this.cellSize, this.cellSize);
    }
  },

  toImage: function() {
    var img = $('<img/>');
    img.attr('src', this.canvasElem.toDataURL());
    return img;
  }
});


$.widget("ui.gameCanvas", $.ui.mouse, {
	widgetEventPrefix: "drag",
	options: {},
  _create: function() {
    this.canvasElem = this.element[0];
    var widget = this;
    this.element.cellCanvas({ width:  parseInt(this.canvasElem.getAttribute('data-width')),
                              height: parseInt(this.canvasElem.getAttribute('data-height')) });
    this.element.droppable({
      drop: function(event, ui) {
        polygod.postCells(
          widget.element.data("cellCanvas"),
          ui.draggable.data('cellsForDrop')(widget._worldCoordinates(ui.offset.left, ui.offset.top))
        );
      }
    });

    var widget = this;

    setTimeout(function() {
      $.ajax({
        url: 'world/current.json',
        dataType: 'json',
        success: function(world) { widget._repaintWorld(world); },
        cache: false,
        error: function(req, status, error) {
          alert("Ajax error Loading World!");
        }
      });
    }, 100);

    this._mouseInit();
  },

  _repaintWorld:function(world) {
    var max = world.cells.length;
    var cellCanvas = this.element.data("cellCanvas");
    for (var i = 0; i < max; i++) {
      var cell = world.cells[i];
      cellCanvas.paintCell(cell.point, cell.alive);
    }

    var widget = this;
    polygod.getWorld(function(world) { widget._repaintWorld(world) }, world.tick);
  },

  _worldCoordinates:  function(pageX, pageY) {
    var canvasOffset = this.element.offset();
    var cellSize = this.element.data("cellCanvas").cellSize;
    var worldX = Math.round( (pageX - canvasOffset.left) / cellSize );
    var worldY = Math.round( (pageY - canvasOffset.top) / cellSize );

    $.log("Converted page coords (%d, %d) to world coords (%d, %d) using offset (%d, %d) and cellSize %0.2f",
          pageX, pageY, worldX, worldY, canvasOffset.left, canvasOffset.top, cellSize);

    return { x: worldX, y: worldY };
  },

  _updateWorld: function(event) {
    var coords = this._worldCoordinates( event.pageX, event.pageY );
    $.log("Updating world at (%d,%d) in repsonse to event at (%d,%d)", coords.x, coords.y, event.pageX, event.pageY);
    polygod.postCells(this.element.data("cellCanvas"),
                      [ { point: [coords.x, coords.y], alive: $('input:radio[name=tool]:checked').val() == "resurrect"} ]);
  },

  _mouseStart: function(event) { this._updateWorld(event); },
  _mouseDrag: function(event) { this._updateWorld(event); }
});

$.widget("ui.patternBox", {
  options: {
    cellSize: null,
    patterns: null
  },

  _create: function() {
    var box = this.element;
    var cellSize = this.options.cellSize;

    $(this.options.patterns).each(function() {
      var pattern = this;
      var canvas = $('<canvas/>');
      canvas.addClass('ui-corner-all');
      canvas.cellCanvas({ width: pattern.size[0], height: pattern.size[1], cellSize: cellSize});

      $(pattern.points).each(function() { canvas.data("cellCanvas").paintCell(this, true)});
      canvas.draggable({ helper: function() { return canvas.data("cellCanvas").toImage(); }});

      canvas.data('cellsForDrop', function(coords) {
        return $(pattern.points).map(function() { return { point: [this[0] + coords.x, this[1] + coords.y], alive: true }}).toArray();
      });

      box.append(canvas);
    });
  }
});

jQuery.fn.all = function(predicate) {
  var answer = true;
  this.each(function() { answer = answer && predicate.apply(this); });
  return answer;
};

var polygod = {
  postCells: function(cellCanvas, cells) {
    if ($(cells).all(function() { return this.point[0] >= 0 && this.point[0] < cellCanvas.width &&
                                         this.point[1] >= 0 && this.point[1] < cellCanvas.height; })) {
      $.log("Posting %d cells to world with dimension (%d, %d)", cells.length, cellCanvas.width, cellCanvas.height)
      $.ajax({
        type: 'POST',
        url: 'world',
        data: JSON.stringify({cells: cells}),
        cache: false,
        error: function(req, status, error) {
          alert("Ajax error Updating World!");
        }
      });
    } else {
      $.log("Not posting %d cells because some where off world, dimenion (%d, %d)", cells.length, cellCanvas.width, cellCanvas.height);
    }
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
