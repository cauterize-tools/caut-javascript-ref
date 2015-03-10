{{#JSTSynonym}}
function {{jstDetail.jstConstructor}} (v) {
  this.value = v;
  this.pack = function (u8buf) {
    return this.value.pack(u8buf);
  };
}
{{jstDetail.jstConstructor}}.unpack = function (u8buf) {
  var biVal = this.refTypeCtor.unpack(u8buf);
  return new {{jstDetail.jstConstructor}}(biVal);
};
{{jstDetail.jstConstructor}}.refTypeCtor = {{jstSynnedCtor}};
{{/JSTSynonym}}
