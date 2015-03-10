// test_client.js

var lib = require('./lib{{jscLibName}}.js');
var c = require('./cauterize.js');
var cb = require('./caut_buffer.js');

var buf = new cb.CautBuffer();

process.stdin.on('readable', function () {
  var chunk = process.stdin.read();
  if (null !== chunk) {
    buf.addU8Array(new Uint8Array(chunk));
  }
});

process.stdin.on('end', function () {
  var di = new c.DataInterface(new lib.CautInfo(), buf);

  var t = di.decodeMeta();
  var e = di.encodeMeta(t);

  process.stdout.write(new Buffer(e));
});
