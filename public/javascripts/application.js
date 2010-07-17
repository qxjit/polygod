$(window).load(function() {
  var canvas = $('#game-canvas')[0]
  canvas.width = 300;
  canvas.height = 300;
  var context = canvas.getContext('2d');
  context.fillStyle = "#FF0000";
  context.fillRect(0,0,20,20);
});
