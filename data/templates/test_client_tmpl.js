// test_client.js

var lib = require('./lib{{jscLibName}}.js');
var c = require('./cauterize.js');

var dataInterface = new c.DataInterface(new lib.CautInfo());

process.stdin.on('readable', function () {
  var chunk = process.stdin.read();
  if (null !== chunk) {
    var buf = new Uint8Array(chunk);
    dataInterface.addData(buf);
  }
});

process.stdin.on('end', function () {
  var t = dataInterface.decodeMeta();
  var e = dataInterface.encodeMeta(t);

  console.error(e);
  process.stdout.write(new Buffer(e));
});
