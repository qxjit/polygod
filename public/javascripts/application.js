$(window).load(function() {
  var canvas = $('#game-canvas')[0]

  canvas.width = 300;
  canvas.height = 300;

  var context = canvas.getContext('2d');
  context.fillStyle = "#FF0000";

  $.ajax({
    url: 'world.json',
    dataType: 'json',
    success: function(jsonData) {
      $(jsonData).each(function(index) {
        var x = this.point[0]*10;
        var y = this.point[1]*10;
        if (this.alive) {
          context.fillRect(x, y, 10, 10);
        } else {
          context.clearRect(x, y, 10, 10);
        }
      });
    },
    error: function(req, status, error) {
      alert("Ajax error Loading World!");
    }
  });
});
