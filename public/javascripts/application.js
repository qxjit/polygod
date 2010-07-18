$(window).load(function() {
  var canvas = $('#game-canvas')[0]

  canvas.width = 300;
  canvas.height = 300;

  var context = canvas.getContext('2d');
  context.fillStyle = "#FF0000";

  var updateWorld = function(world) {
    $(world.cells).each(function(index) {
      var x = this.point[0]*10;
      var y = this.point[1]*10;
      if (this.alive) {
        context.fillRect(x, y, 10, 10);
      } else {
        context.clearRect(x, y, 10, 10);
      }
    });


    $.ajax({
      url: 'world/next.json?tick=' + world.tick,
      dataType: 'json',
      success: updateWorld,
      cache: false,
      error: function(req, status, error) {
        alert("Ajax error Updating World!");
      }
    });
  };

  setTimeout(function() {
    $.ajax({
      url: 'world/current.json',
      dataType: 'json',
      success: updateWorld,
      cache: false,
      error: function(req, status, error) {
        alert("Ajax error Loading World!");
      }
    });
  }, 100);
});
