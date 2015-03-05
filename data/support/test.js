var b = require('./builtin.js')

var a = new Uint8Array([0x01,0x02,0x03,0x04
                       ,0x05,0x06,0x07,0x08]);

console.log(b.Bool.mk(a,0));

console.log(b.U8.mk(a,0));
console.log(b.U16.mk(a,0));
console.log(b.U32.mk(a,0));
console.log(b.U64.mk(a,0));

console.log(b.CU8.mk(a,0));
console.log(b.CU16.mk(a,0));
console.log(b.CU32.mk(a,0));

console.log(b.S8.mk(a,0));
console.log(b.S16.mk(a,0));
console.log(b.S32.mk(a,0));
console.log(b.S64.mk(a,0));

console.log(b.F32.mk(a,0));
console.log(b.F64.mk(a,0));
