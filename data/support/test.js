/*global Uint8Array*/
'use strict';
var b = require('./builtin.js');

var a = new Uint8Array([0x01,0x02,0x03,0x04
                       ,0x05,0x06,0x07,0x08]);

function eqTest(ctor, v) {
  var buf = new Uint8Array(ctor.width);
  var x = new ctor(v);
  x.pack(buf,0);
  var y = ctor.unpack(buf,0);

  if (x.value === y.value) {
    console.log(ctor.name + " equal");
  } else {
    console.log(ctor.name + " not equal");
  }
}

var ctors = [b.U8,b.U16,b.U32,b.U64
            ,b.S8,b.S16,b.S32,b.S64
            ,b.CU8,b.CU16,b.CU32
            ,b.F32,b.F64
            ,b.Bool];

var ctor, x;
var i; for(i = 0; i < ctors.length; i++) {
  eqTest(ctors[i], 1);
}
