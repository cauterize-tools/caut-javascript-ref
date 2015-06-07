/*globals ArrayBuffer, Uint8Array */
'use strict';

var assert = require('assert');
var prot = require('./libcaut/prototypes.js');
var cb = require('./libcaut/buffer.js');

var data = new ArrayBuffer(8);
var view = new Uint8Array(data);
var i; for (i = 0; i < data.byteLength; i++) {
  view[i] = i + 10;
}

/**
 * Can we define a U8?
 */
function U8(buffer) { prot.CBuiltIn.call(this, buffer); }
prot.mkU8(U8, [1,2,3], {min: 1, max: 1});

(function () {
  /* Test we can define a U8. */
  var u8 = new U8(data);
  assert.equal(10, u8.toJS());

  var buffer = new cb.CautBuffer();
  u8.pack(buffer);
  assert.equal(1, buffer.allData().byteLength);
  assert.equal(10, buffer.allData()[0]);

  assert.equal(10, U8.unpack(buffer).toJS());

  var x = U8.fromJS(198);
  assert.equal(198, x.toJS());
}());

/**
 * Can we define a U32?
 */
function U32(buffer) { prot.CBuiltIn.call(this, buffer); }
prot.mkU32(U32, [1,2,3], {min: 4, max: 4});

(function () {
  /* Test we can define a U32. */
  var u32 = new U32(data);
  assert.equal(218893066, u32.toJS());

  var buffer = new cb.CautBuffer();
  u32.pack(buffer);
  var ad = buffer.allData();
  assert.equal(4, ad.byteLength);
  assert.equal(10, ad[0]);
  assert.equal(11, ad[1]);
  assert.equal(12, ad[2]);
  assert.equal(13, ad[3]);

  assert.equal(218893066, U32.unpack(buffer).toJS());

  var x = U32.fromJS(5000);
  assert.equal(5000, x.toJS());
}());

/**
 * Can we define a synonym?
 */
function AnU32(value) { prot.CSynonym.call(this, value); }
prot.mkSynonym(AnU32, 'an_u8', U32, [1,2,3], {min:4, max:4});

(function () {
  /* Test we can define a Synonym */
  var anu32 = new AnU32(new U32(data));
  assert.equal(218893066, anu32.toJS());

  var buffer = new cb.CautBuffer();
  anu32.pack(buffer);
  var ad = buffer.allData();
  assert.equal(4, ad.byteLength);
  assert.equal(10, ad[0]);
  assert.equal(11, ad[1]);
  assert.equal(12, ad[2]);
  assert.equal(13, ad[3]);

  assert.equal(218893066, AnU32.unpack(buffer).toJS());
}());

/**
 * Can we define an array?
 */
function ArrU8(elems) { prot.CArray.call(this, elems); }
prot.mkArray(ArrU8, 'arr_u8', U8, 5, [1,2,3], {min: 5, max: 5});

(function () {
  var buffer = new cb.CautBuffer();
  buffer.addU8Array(view);

  var arru8 = ArrU8.unpack(buffer);
  var js = arru8.toJS();
  assert.equal(10, js[0]);
  assert.equal(11, js[1]);
  assert.equal(12, js[2]);
  assert.equal(13, js[3]);
  assert.equal(14, js[4]);

  var pb = new cb.CautBuffer();
  arru8.pack(pb);

  var ad = pb.allData();
  assert.equal(5, ad.length);
  assert.equal(10, ad[0]);
  assert.equal(11, ad[1]);
  assert.equal(12, ad[2]);
  assert.equal(13, ad[3]);
  assert.equal(14, ad[4]);
}());

/**
 * Can we define a vector?
 */
function VecU8(elems) { prot.CVector.call(this, elems); }
prot.mkVector(VecU8, 'vec_u8', U8, 5, 1, [1,2,3], {min: 1, max: 6});

(function () {
  var vecdata = new Uint8Array([3, 0, 1, 2]);

  var buffer = new cb.CautBuffer();
  buffer.addU8Array(vecdata);

  var vecu8 = VecU8.unpack(buffer);
  var js = vecu8.toJS();
  assert.equal(0, js[0]);
  assert.equal(1, js[1]);
  assert.equal(2, js[2]);

  var pb = new cb.CautBuffer();
  vecu8.pack(pb);

  var ad = pb.allData();
  assert.equal(4, ad.length);
  assert.equal(3, ad[0]);
  assert.equal(0, ad[1]);
  assert.equal(1, ad[2]);
  assert.equal(2, ad[3]);
}());

function Rec(fields) { prot.CRecord.call(this, fields); }
var recfields = [ { name: "a", index: 0, ref: U8 },
                  { name: "b", index: 1, ref: U8 },
                  { name: "c", index: 2, ref: U8 } ];
prot.mkRecord(Rec, "rec", recfields, [1,2,3], {min:3, max:3});

(function () {
  var recdata = new Uint8Array([10, 100, 200]);
  var buffer = new cb.CautBuffer();
  buffer.addU8Array(recdata);

  var rec = Rec.unpack(buffer);
  var js = rec.toJS();

  assert.equal(10, js.a);
  assert.equal(100, js.b);
  assert.equal(200, js.c);

  var pb = new cb.CautBuffer();
  rec.pack(pb);
  var ad = pb.allData();
  assert.equal(10,  ad[0]);
  assert.equal(100, ad[1]);
  assert.equal(200, ad[2]);
}());

function Comb(fields) { prot.CCombination.call(this, fields); }
var combfields = [ { name: "a", index: 0, ref: U8 },
                   { name: "b", index: 1, ref: U8 },
                   { name: "c", index: 2, ref: U8 } ];
prot.mkCombination(Comb, "comb", combfields, 1, [1,2,3], {min:1, max:4});

(function () {
  var combdata = new Uint8Array([5, 10, 200]);
  var buffer = new cb.CautBuffer();
  buffer.addU8Array(combdata);

  var com = Comb.unpack(buffer);
  var js = com.toJS();

  assert.equal(10,  js.a);
  assert.equal(undefined, js.b);
  assert.equal(200, js.c);

  delete com.fields.a;

  var pb = new cb.CautBuffer();
  com.pack(pb);
  var ad = pb.allData();
  assert.equal(4,  ad[0]);
  assert.equal(200, ad[1]);
}());

function Uni(field) { prot.CUnion.call(this,field); }
var unifields = [ { name: "a", index: 0, ref: U8 },
                  { name: "b", index: 1, ref: U32 } ];
prot.mkUnion(Uni, "uni", unifields, 1, [1,2,3], {min:2, max:5});

(function () {
  var unidata = new Uint8Array([1,30,0,0,0]);
  var buffer = new cb.CautBuffer();
  buffer.addU8Array(unidata);

  var u = Uni.unpack(buffer);
  var js = u.toJS();

  assert.equal(undefined, js.a);
  assert.equal(30, js.b);

  var pb = new cb.CautBuffer();
  u.pack(pb);
  var ad = pb.allData();
  assert.equal(1,  ad[0]);
  assert.equal(30, ad[1]);
  assert.equal(0, ad[2]);
  assert.equal(0, ad[3]);
  assert.equal(0, ad[4]);
}());
