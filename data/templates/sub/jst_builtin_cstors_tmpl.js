{{#JSTBuiltIn}}
{{#jstBIInstance}}
{{#JSU8}}
function U8(v) {
  this.value = v;
  this.pack = function (cBuf) { return BIS.pack(this, cBuf); };
}
U8.width = 1;
U8.view = Uint8Array;
U8.unpack = function (cBuf) { return BIS.unpack(U8, cBuf); };
{{/JSU8}}
{{#JSU16}}
function U16(v) {
  this.value = v;
  this.pack = function (cBuf) { return BIS.pack(this, cBuf); };
}
U16.width = 2;
U16.view = Uint16Array;
U16.unpack = function (cBuf) { return BIS.unpack(U16, cBuf); };
{{/JSU16}}
{{#JSU32}}
function U32(v) {
  this.value = v;
  this.pack = function (cBuf) { return BIS.pack(this, cBuf); };
}
U32.width = 4;
U32.view = Uint32Array;
U32.unpack = function (cBuf) { return BIS.unpack(U32, cBuf); };
{{/JSU32}}
{{#JSU64}}
function U64(v0, v1) {
  this.v0 = v0; this.v1 = v1;
  this.pack = function (cBuf) { return BIS.pack64(this, cBuf); };
}
U64.width = 8;
U64.view = Uint32Array;
U64.unpack = function (cBuf) { return BIS.unpack64(U64, cBuf); };
{{/JSU64}}
{{#JSS8}}
function S8(v) {
  this.value = v;
  this.pack = function (cBuf) { return BIS.pack(this, cBuf); };
}
S8.width = 1;
S8.view = Int8Array;
S8.unpack = function (cBuf) { return BIS.unpack(S8, cBuf); };
{{/JSS8}}
{{#JSS16}}
function S16(v) {
  this.value = v;
  this.pack = function (cBuf) { return BIS.pack(this, cBuf); };
}
S16.width = 2;
S16.view = Int16Array;
S16.unpack = function (cBuf) { return BIS.unpack(S16, cBuf); };
{{/JSS16}}
{{#JSS32}}
function S32(v) {
  this.value = v;
  this.pack = function (cBuf) { return BIS.pack(this, cBuf); };
}
S32.width = 4;
S32.view = Int32Array;
S32.unpack = function (cBuf) { return BIS.unpack(S32, cBuf); };
{{/JSS32}}
{{#JSS64}}
function S64(v0, v1) {
  this.v0 = v0; this.v1 = v1;
  this.pack = function (cBuf) { return BIS.pack64(this, cBuf); };
}
S64.width = 8;
S64.view = Uint32Array;
S64.unpack = function (cBuf) { return BIS.unpack64(S64, cBuf); };
{{/JSS64}}
{{#JSF32}}
function F32(v) {
  this.value = v;
  this.pack = function (cBuf) { return BIS.pack(this, cBuf); };
}
F32.width = 4;
F32.view = Float32Array;
F32.unpack = function (cBuf) { return BIS.unpack(F32, cBuf); };
{{/JSF32}}
{{#JSF64}}
function F64(v) {
  this.value = v;
  this.pack = function (cBuf) { return BIS.pack(this, cBuf); };
}
F64.width = 8;
F64.view = Float64Array;
F64.unpack = function (cBuf) { return BIS.unpack(F64, cBuf); };
{{/JSF64}}
{{#JSCu8}}
function Cu8(v) {
  this.value = v;
  this.pack = function (cBuf) { return BIS.pack(this, cBuf); };
}
Cu8.width = 1;
Cu8.view = Uint8Array;
Cu8.unpack = function (cBuf) { return BIS.unpack(Cu8, cBuf); };
{{/JSCu8}}
{{#JSCu16}}
function Cu16(v) {
  this.value = v;
  this.pack = function (cBuf) { return BIS.pack(this, cBuf); };
}
Cu16.width = 2;
Cu16.view = Uint16Array;
Cu16.unpack = function (cBuf) { return BIS.unpack(Cu16, cBuf); };
{{/JSCu16}}
{{#JSCu32}}
function Cu32(v) {
  this.value = v;
  this.pack = function (cBuf) { return BIS.pack(this, cBuf); };
}
Cu32.width = 4;
Cu32.view = Uint32Array;
Cu32.unpack = function (cBuf) { return BIS.unpack(Cu32, cBuf); };
{{/JSCu32}}
{{#JSBool}}
function Bool(v) {
  if (v !== 0 && v !== 1) {
    throw new Error("Illegal value for boolean: " + v.toString());
  }

  this.value = v;
  this.pack = function (cBuf) { return BIS.pack(this, cBuf); };
}
Bool.width = 1;
Bool.view = Uint8Array;
Bool.unpack = function (cBuf) {
  var b = BIS.unpack(Bool, cBuf);
  if (b.value !== 0 && b.value !== 1) {
    throw new Error("Illegal value for boolean: " + b.value);
  }
  return new Bool(b.value);
};
{{/JSBool}}
{{/jstBIInstance}}
{{/JSTBuiltIn}}
