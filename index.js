const static = require('node-static');
const file = new static.Server('./public');

require('http').createServer((req, res) => {
  req
    .addListener('end', () => file.serve(req, res))
    .resume();
}).listen(process.env.PORT || 8080);
