{{#JSTSynonym}}
function {{jstDetail.jstConstructor}} (v) {
  this.value = v;
  this.pack = function (u8buf, offset) {
    return this.value.pack(u8buf, offset);
  };
}
{{jstDetail.jstConstructor}}.unpack = function (u8buf, offset) {
  var biVal = {{jstSynnedCtor}}.unpack(u8buf, offset);
  return new {{jstDetail.jstConstructor}}(biVal);
};

{{/JSTSynonym}}
