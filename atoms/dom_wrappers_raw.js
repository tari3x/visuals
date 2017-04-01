
function js_read_camera(video)
{
  // do not include
  // navigator.mediaDevices.getUserMedia
  // it works differently
  navigator.getUserMedia =
    navigator.getUserMedia
    || navigator.webkitGetUserMedia
    || navigator.mozGetUserMedia;

  // Small in chrome, large in firefox, none of the below changes take
  // effect. Maybe try adapter.js (I think the instancing example used
  // that). Maybe try the newer navigator.mediaDevices.getUserMedia. Or just
  // find and deconstruct an example that works in both.
  //
  // It does work in wall.html, so it must be something highly retarted. Perhaps
  // the js_of_ocaml version of drawImage is fucked?
  navigator.getUserMedia({
    video: { width: 1920, height: 1080 }
  }, function(stream) {
    try {
      video.src = window.URL.createObjectURL(stream);
    } catch (err) {
      video.src = stream;
    }
  }, function() {
    throw Error('Cannot capture user camera.');
  })
}
