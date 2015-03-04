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
  process.stderr.write("Data Length: " + dataInterface.metaLength().toString() + "\n");
  process.stderr.write("Data Tag: " + dataInterface.metaType().toString() + "\n");
});
