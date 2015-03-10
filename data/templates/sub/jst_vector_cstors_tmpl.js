{{#JSTVector}}
function {{jstDetail.jstConstructor}} (v) {
  if (v.length > {{jstDetail.jstConstructor}}.vectorMaxLen) {
    throw new Error("Invalid vector length: " + v.length);
  } else {
    this.elems = v;
  }

  this.pack = function (cBuf) {
    var len = cBuf.length();

    C.intToBytes(this.elems.length, cBuf, {{jstDetail.jstConstructor}}.vectorLengthWidth);

    var i;
    for (i = 0; i < this.elems.length; i++) {
      this.elems[i].pack(cBuf);
    }

    return cBuf.length() - len;
  };
}
{{jstDetail.jstConstructor}}.unpack = function (cBuf) {
  var len = C.bytesToInt(cBuf, {{jstDetail.jstConstructor}}.vectorLengthWidth);

  if (len > {{jstDetail.jstConstructor}}.vectorMaxLength) {
    throw new Error("Encoded length is too large: " + len);
  }

  var elems = [];
  for (i = 0; i < len; i++) {
    elems.push({{jstDetail.jstConstructor}}.vectorElemCtor.unpack(cBuf));
  }

  return new {{jstDetail.jstConstructor}}(elems);
};
{{jstDetail.jstConstructor}}.vectorMaxLength = {{jstVectorMaxLen}};
{{jstDetail.jstConstructor}}.vectorLengthWidth = {{jstVectorLenWidth}};
{{jstDetail.jstConstructor}}.vectorElemCtor = {{jstVectorRefCtor}};
{{/JSTVector}}
