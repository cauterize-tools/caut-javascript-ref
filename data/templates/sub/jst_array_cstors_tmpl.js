{{#JSTArray}}
function {{jstDetail.jstConstructor}} (v) {
  this.value = v;
  this.pack = function (u8buf, offset) {
    var i; var pos = 0;
    for (i = 0; i < this.constructor.arrayLength; i++) {
      pos += v[i].pack(u8buf, offset + pos);
    }
    return pos;
  };
}
{{jstDetail.jstConstructor}}.unpack = function (u8buf, offset) {
  /* this.constructor.arrayElemCtor.unpack(u8buf, offset); */
  throw new Error("need to make some sort of smart buffer that keeps track of position.");
};
{{jstDetail.jstConstructor}}.arrayLength = {{jstArrayLen}};
{{jstDetail.jstConstructor}}.arrayElemCtor = {{jstArrayRefCtor}};
{{/JSTArray}}
