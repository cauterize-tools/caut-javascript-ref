{{#JSTBuiltIn}}
{{#jstBIInstance}}
{{#JSU8}}
function U8(v) {
  this.value = v;
  this.pack = function (u8buf, offset) { return BIS.pack(this, u8buf, offset); };
}
U8.width = 1;
U8.view = Uint8Array;
U8.unpack = function (u8buf, offset) { return BIS.unpack(U8, u8buf, offset); };
Object.freeze(U8);
exports.U8 = U8;
{{/JSU8}}
{{#JSU16}}
function U16(v) {
  this.value = v;
  this.pack = function (u8buf, offset) { return BIS.pack(this, u8buf, offset); };
}
U16.width = 2;
U16.view = Uint16Array;
U16.unpack = function (u8buf, offset) { return BIS.unpack(U16, u8buf, offset); };
Object.freeze(U16);
exports.U16 = U16;
{{/JSU16}}
{{#JSU32}}
function U32(v) {
  this.value = v;
  this.pack = function (u8buf, offset) { return BIS.pack(this, u8buf, offset); };
}
U32.width = 4;
U32.view = Uint32Array;
U32.unpack = function (u8buf, offset) { return BIS.unpack(U32, u8buf, offset); };
Object.freeze(U32);
exports.U32 = U32;
{{/JSU32}}
{{#JSU64}}
function U64(v0, v1) {
  this.v0 = v0; this.v1 = v1;
  this.pack = function (u8buf, offset) { return BIS.pack64(this, u8buf, offset); };
}
U64.width = 8;
U64.view = Uint32Array;
U64.unpack = function (u8buf, offset) { return BIS.unpack64(U64, u8buf, offset); };
Object.freeze(U64);
exports.U64 = U64;
{{/JSU64}}
{{#JSS8}}
function S8(v) {
  this.value = v;
  this.pack = function (u8buf, offset) { return BIS.pack(this, u8buf, offset); };
}
S8.width = 1;
S8.view = Int8Array;
S8.unpack = function (u8buf, offset) { return BIS.unpack(S8, u8buf, offset); };
Object.freeze(S8);
exports.S8 = S8;
{{/JSS8}}
{{#JSS16}}
function S16(v) {
  this.value = v;
  this.pack = function (u8buf, offset) { return BIS.pack(this, u8buf, offset); };
}
S16.width = 2;
S16.view = Int16Array;
S16.unpack = function (u8buf, offset) { return BIS.unpack(S16, u8buf, offset); };
Object.freeze(S16);
exports.S16 = S16;
{{/JSS16}}
{{#JSS32}}
function S32(v) {
  this.value = v;
  this.pack = function (u8buf, offset) { return BIS.pack(this, u8buf, offset); };
}
S32.width = 4;
S32.view = Int32Array;
S32.unpack = function (u8buf, offset) { return BIS.unpack(S32, u8buf, offset); };
Object.freeze(S32);
exports.S32 = S32;
{{/JSS32}}
{{#JSS64}}
function S64(v0, v1) {
  this.v0 = v0; this.v1 = v1;
  this.pack = function (u8buf, offset) { return BIS.pack64(this, u8buf, offset); };
}
S64.width = 8;
S64.view = Uint32Array;
S64.unpack = function (u8buf, offset) { return BIS.unpack64(S64, u8buf, offset); };
Object.freeze(S64);
exports.S64 = S64;
{{/JSS64}}
{{#JSF32}}
function F32(v) {
  this.value = v;
  this.pack = function (u8buf, offset) { return BIS.pack(this, u8buf, offset); };
}
F32.width = 4;
F32.view = Float32Array;
F32.unpack = function (u8buf, offset) { return BIS.unpack(F32, u8buf, offset); };
Object.freeze(F32);
exports.F32 = F32;
{{/JSF32}}
{{#JSF64}}
function F64(v) {
  this.value = v;
  this.pack = function (u8buf, offset) { return BIS.pack(this, u8buf, offset); };
}
F64.width = 8;
F64.view = Float64Array;
F64.unpack = function (u8buf, offset) { return BIS.unpack(F64, u8buf, offset); };
Object.freeze(F64);
exports.F64 = F64;
{{/JSF64}}
{{#JSCu8}}
function Cu8(v) {
  this.value = v;
  this.pack = function (u8buf, offset) { return BIS.pack(this, u8buf, offset); };
}
Cu8.width = 1;
Cu8.view = Uint8Array;
Cu8.unpack = function (u8buf, offset) { return BIS.unpack(Cu8, u8buf, offset); };
Object.freeze(Cu8);
exports.Cu8 = Cu8;
{{/JSCu8}}
{{#JSCu16}}
function Cu16(v) {
  this.value = v;
  this.pack = function (u8buf, offset) { return BIS.pack(this, u8buf, offset); };
}
Cu16.width = 2;
Cu16.view = Uint16Array;
Cu16.unpack = function (u8buf, offset) { return BIS.unpack(Cu16, u8buf, offset); };
Object.freeze(Cu16);
exports.Cu16 = Cu16;
{{/JSCu16}}
{{#JSCu32}}
function Cu32(v) {
  this.value = v;
  this.pack = function (u8buf, offset) { return BIS.pack(this, u8buf, offset); };
}
Cu32.width = 4;
Cu32.view = Uint32Array;
Cu32.unpack = function (u8buf, offset) { return BIS.unpack(Cu32, u8buf, offset); };
Object.freeze(Cu32);
exports.Cu32 = Cu32;
{{/JSCu32}}
{{#JSBool}}
function Bool(v) {
  if (v !== 0 && v !== 1) {
    throw new Error("Illegal value for boolean: " + v.toString());
  }

  this.value = v;
  this.pack = function (u8buf, offset) { return BIS.pack(this, u8buf, offset); };
}
Bool.width = 1;
Bool.view = Uint8Array;
Bool.unpack = function (u8buf, offset) {
  var b = unpack(Bool, u8buf, offset);
  if (b.value !== 0 && b.value !== 1) {
    throw new Error("Illegal value for boolean: " + b.value);
  }
  return new Bool(b.value);
};
Object.freeze(Bool);
exports.Bool = Bool;
{{/JSBool}}
{{/jstBIInstance}}

{{/JSTBuiltIn}}
