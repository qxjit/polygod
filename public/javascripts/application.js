$(window).load(function() {
  $('#game-canvas').masterCanvas();
  $('.toolbar').buttonset();
  $('#pattern-box').patternBox({
    masterCanvas: $('#game-canvas').data('masterCanvas'),
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

$.widget("ui.abstractCellCanvas", {
  _create: function() {
    this.canvasElem = this.element[0];
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

$.widget("ui.masterCanvas", $.ui.mouse, $.extend({}, $.ui.abstractCellCanvas.prototype, {
  _create: function() {
    $.ui.abstractCellCanvas.prototype._create.apply(this);

    this.width = parseInt(this.canvasElem.getAttribute('data-width'));
    this.height = parseInt(this.canvasElem.getAttribute('data-height'));
    this.adjustDimensionsToMatchWidth();
    this._cells = new polygod.CellSpace(this.width, this.height);

    var widget = this;

    this.element.droppable({
      drop: function(event, ui) {
        polygod.postCells(
          widget,
          ui.draggable.data('cellsForDrop')(widget._worldCoordinates(ui.offset.left, ui.offset.top))
        );
      }
    });

    setTimeout(function() {
      polygod.getWorldLoop(widget.canvasElem.getAttribute('data-startUrl'),
                       function(data, textStatus, xhr) { widget._nextWorld(data, xhr); });
    }, 100);

    var paintLoop = function() {
      setTimeout(function() {
        paintLoop();

        if (!widget._newWorldFromServerSinceLastPaint()) {
          widget._cells = widget._cells.evolve();
        }

        widget._repaintWorld();
      }, 250);
    };
    paintLoop();

    this._mouseInit();
  },

  _newWorldFromServerSinceLastPaint: function() {
    return this._lastResponse && this._lastResponse.tick != this._lastPaintedTick;
  },

  _nextWorld: function(response, xhr) {
    if (!this._lastResponse || (this._lastResponse.tick < response.tick)) {
      this._lastResponse = response;
      var cells = response.cells;
      var newCellSpace = this._cells.clone();

      for (var i = 0; i < cells.length; i++) {
        var cell = cells[i];
        newCellSpace.setCellAt(cell.point[0], cell.point[1], cell.alive);
      }

      this._cells = newCellSpace;

      $('.concurrentUsersCount').html(xhr.getResponseHeader('X-Polygod-ConcurrentUsers'));
    }
  },

  adjustDimensionsToMatchWidth: function() {
    if (this.element.width() != this._widthDimensionsWereBasedOn) {
      var impl = function() {
        var currentWidth = this.element.width();

        this.cellSize = currentWidth / this.width;
        this.canvasElem.width  = this.width * this.cellSize;
        this.canvasElem.height = this.height * this.cellSize;

        this._widthDimensionsWereBasedOn = this.element.width();
      };

      var widthBeforeAdjustment = this.element.width();

      impl.apply(this);

      // readjust again if setting the height has narrowed our width
      //  (perhaps by making a scroll bar appear)
      //
      if (this.element.width() < widthBeforeAdjustment) {
        impl.apply(this);
      }

      $.log("Triggering redimension event");
      this._trigger('redimension');
    }
  },

  _repaintWorld:function() {
    this.adjustDimensionsToMatchWidth();
    if (this._lastResponse) {
      this._lastPaintedTick = this._lastResponse.tick;
    }

    for (var x = 0; x < this.width; x++) {
      for (var y = 0; y < this.height; y++) {
        this.paintCell([x, y], this._cells.getCellAt(x, y));
      }
    }
  },

  _worldCoordinates:  function(pageX, pageY) {
    var canvasOffset = this.element.offset();

    var elementWidth = this.element.width();
    var elementHeight = this.element.height();

    var worldX = Math.round( this.width  * (pageX - canvasOffset.left) / elementWidth  );
    var worldY = Math.round( this.height * (pageY - canvasOffset.top)  / elementHeight );

    $.log("Converted page coords (%d, %d) to world coords (%d, %d) using offset (%d, %d) and element size (%d, %d)",
          pageX, pageY, worldX, worldY, canvasOffset.left, canvasOffset.top, elementWidth, elementHeight);

    return { x: worldX, y: worldY };
  },

  _updateWorld: function(event) {
    var coords = this._worldCoordinates( event.pageX, event.pageY );
    $.log("Updating world at (%d,%d) in repsonse to event at (%d,%d)", coords.x, coords.y, event.pageX, event.pageY);
    polygod.postCells(this, [ { point: [coords.x, coords.y], alive: $('input:radio[name=tool]:checked').val() == "resurrect"} ]);
  },

  _mouseDown: function(event) { this._updateWorld(event); },
  _mouseDrag: function(event) { this._updateWorld(event); }
}));

$.widget("ui.patternCanvas", $.ui.mouse, $.extend({}, $.ui.abstractCellCanvas.prototype, {
  options: {
    pattern: null,
    masterCanvas: null,
  },

  _create: function() {
    $.ui.abstractCellCanvas.prototype._create.apply(this);

    var widget = this;
    this.element.draggable({ helper: function() { return widget.toImage(); }});
    this.element.data('cellsForDrop', function(coords) {
      return $(widget.options.pattern.points).map(function() { return { point: [this[0] + coords.x, this[1] + coords.y], alive: true }}).toArray();
    });

    this._repaint();

    this.options.masterCanvas.element.bind('abstractcellcanvasredimension', function(event) {
      $.log('Repainting pattern in response to redimension');
      widget._repaint();
    });
  },

  _repaint: function() {
    this.cellSize = this.options.masterCanvas.cellSize;
    this.canvasElem.width  = this.cellSize * this.options.pattern.size[0];
    this.canvasElem.height = this.cellSize * this.options.pattern.size[1];

    var cellCanvas = this;

    $(this.options.pattern.points).each(function() { cellCanvas.paintCell(this, true)});
  },
}));

$.widget("ui.patternBox", {
  options: {
    masterCanvas: null,
    patterns: null
  },

  _create: function() {
    var box = this.element;
    var masterCanvas = this.options.masterCanvas;

    $(this.options.patterns).each(function() {
      var pattern = this;
      var canvas = $('<canvas/>');
      canvas.addClass('ui-corner-all');
      canvas.patternCanvas({ pattern: pattern, masterCanvas: masterCanvas });
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

  getWorldLoop: function(url, successCallback) {
    var xhr = polygod.getWorld(url, successCallback);
    var oldORSC = xhr.onreadystatechange;
    var nextRequestSpawned = false;

    xhr.onreadystatechange = function(isTimeout) {
      if (!nextRequestSpawned && xhr.getResponseHeader("Location")) {
        nextRequestSpawned = true;
        polygod.getWorldLoop(xhr.getResponseHeader("Location"), successCallback);
      }
      oldORSC(isTimeout);
    };
    return xhr;
  },

  getWorld: function(url, successCallback) {
    return $.ajax({
      url: url,
      dataType: 'json',
      success: successCallback,
      cache: false,
      error: function(req, status, error) {
        alert("Ajax error Updating World!");
      }
    });
  },

  CellSpace: function(width, height) {
    var cells = new Array(width*height);
    var properties = {
      width: width,
      height: height,

      setCellAt: function(x, y, val) {
        this[y * this.width + x] = val;
      },

      getCellAt: function(x, y) {
        return this[y * this.width + x];
      },

      clone: function() {
        var newCells = this.slice(0);
        $.extend(newCells, properties);
        return newCells;
      },

      evolve: function() {
        var newCells = this.clone();

        for (var x = 0; x < this.width; x++) {
          for (var y = 0; y < this.height; y++) {
            var neighborAddresses = neighboringAddresses(x, y, this.width, this.height);
            var neighbors = [];

            for (var i = 0; i < neighborAddresses.length; i++) {
              var addr = neighborAddresses[i];
              neighbors[i] = this.getCellAt(addr[0], addr[1]);
            }

            newCells.setCellAt(x, y, fate(this.getCellAt(x,y), neighbors));
          }
        }

        return newCells;

        function fate(cell, neighbors) {
          var liveNeighbors = 0;

          for (var i = 0; i < neighbors.length; i++) {
            if (neighbors[i]) {
              liveNeighbors++;
            }
          }

          if (cell) {
            return (liveNeighbors == 2) || (liveNeighbors == 3);
          } else {
            return liveNeighbors == 3;
          }
        };

        function neighboringAddresses(x, y, width, height) {
          var neighbors = [
            [x - 1, y - 1],  [x, y - 1], [x + 1, y - 1],
            [x - 1, y    ],              [x + 1, y    ],
            [x - 1, y + 1],  [x, y + 1], [x + 1, y + 1]
          ];

          for (var i = 0; i < neighbors.length; i++) {
            neighbors[i][0] = (neighbors[i][0] + width) % width;
            neighbors[i][1] = (neighbors[i][1] + height) % height;
          }

          return neighbors;
        };
      },
    };

    $.extend(cells, properties);

    return cells;
  },
};
