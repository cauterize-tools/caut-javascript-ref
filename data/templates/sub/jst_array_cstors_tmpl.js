{{#JSTArray}}
function {{jstDetail.jstConstructor}} (v) {
  this.value = v;
  this.pack = function (cBuf) {
    var i; var pos = cBuf.position();
    for (i = 0; i < this.constructor.arrayLength; i++) {
      v[i].pack(cBuf);
    }
    return cBuf.position() - pos;
  };
}
{{jstDetail.jstConstructor}}.unpack = function (u8buf, offset) {
  /* this.constructor.arrayElemCtor.unpack(u8buf, offset); */
  throw new Error("need to make some sort of smart buffer that keeps track of position.");
};
{{jstDetail.jstConstructor}}.arrayLength = {{jstArrayLen}};
{{jstDetail.jstConstructor}}.arrayElemCtor = {{jstArrayRefCtor}};
{{/JSTArray}}
