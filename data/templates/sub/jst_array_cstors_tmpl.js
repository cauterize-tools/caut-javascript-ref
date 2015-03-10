{{#JSTArray}}
function {{jstDetail.jstConstructor}} (v) {
  this.elems = v;
  this.pack = function (cBuf) {
    var i; var len = cBuf.length();
    for (i = 0; i < {{jstDetail.jstConstructor}}.arrayLength; i++) {
      this.elems[i].pack(cBuf);
    }
    return cBuf.length() - len;
  };
}
{{jstDetail.jstConstructor}}.unpack = function (cBuf) {
  var i;

  var elems = [];
  for (i = 0; i < {{jstDetail.jstConstructor}}.arrayLength; i++) {
    elems.push({{jstDetail.jstConstructor}}.arrayElemCtor.unpack(cBuf));
  }

  return new {{jstDetail.jstConstructor}}(elems);
};
{{jstDetail.jstConstructor}}.arrayLength = {{jstArrayLen}};
{{jstDetail.jstConstructor}}.arrayElemCtor = {{jstArrayRefCtor}};
{{/JSTArray}}
