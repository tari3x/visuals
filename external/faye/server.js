var http = require('http'),
    faye = require('faye');

var server = http.createServer(),
    bayeux = new faye.NodeAdapter({mount: '/faye', timeout: 45});

bayeux.attach(server);
server.listen(8000);

/* 
bayeux.on('handshake', function(clientId) {
  console.log('handshake: ' + clientId);
});

bayeux.on('subscribe', function(clientId, channel) {
  console.log('subscribe: ' + clientId + ' ' + channel);
});

bayeux.on('publish', function(clientId, channel, data) {
  console.log('publish: ' + clientId + ' ' + channel + ' ' + data);
});

bayeux.on('publish', function(clientId, channel, data) {
  console.log('publish: ' + clientId + ' ' + channel + ' ' + data.to_string ());
});
*/
