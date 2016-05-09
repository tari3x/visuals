$('html').click(function(){
  var scale = 15.0;
  var circle = $('<div style="display: table-cell;'
                 + 'vertical-align: middle;'
                 + 'position: fixed;'
                 +' width: ' + Math.floor(scale*240.0) + 'px'
                 + 'height: ' + Math.floor(scale*240.0) + 'px'
                 + 'border: '+ Math.floor(scale*60.0) + 'px solid rgba(255,255,255,1.0);'
                 + 'border-radius: 50%;"></div>')
  $('body').append(circle)
  var random_margin = Math.floor(scale*120.0) + Math.floor(Math.random() * scale * 48.0)
  circle.animate({'border-width': '0',
                  'width': Math.floor(scale*50.0) + 'px',
                  'height': Math.floor(scale*50.0) + 'px',
                  'margin': random_margin + 'px',
                  'opacity': 0},
                 {duration: 2000,
                  queue: false,
                  complete: function() { circle.remove(); }
                 });
})
