{{#JSTUnion}}
function {{jstDetail.jstConstructor}} (f) {
  // TODO: Make sure we don't have any extra fields.
  this.field = f;

  this.pack = function (cBuf) {
    var len = cBuf.length();

    // Figure out what our tag is and pack it.
    var fi, fieldRef;
    for (fi in {{jstDetail.jstConstructor}}.fields) {
      f = {{jstDetail.jstConstructor}}.fields[fi];
      if (undefined !== this.field[f['name']]) {
        fieldRef = f;
        break;
      }
    }
    C.intToBytes(fieldRef['index'], cBuf, {{jstDetail.jstConstructor}}.unionTagWidth);

    // Pack the field.
    if (undefined !== fieldRef['ref']) {
      this.field[fieldRef['name']].pack(cBuf);
    }

    return cBuf.length() - len;
  };
}
{{jstDetail.jstConstructor}}.unpack = function (cBuf) {
  var fi, tag = C.bytesToInt(cBuf, {{jstDetail.jstConstructor}}.unionTagWidth);
  var f, unpackedField = null, fieldObj = {};

  for (fi in {{jstDetail.jstConstructor}}.fields) {
    f = {{jstDetail.jstConstructor}}.fields[fi];

    if (tag === f['index']) {
      if (undefined !== f['ref']) {
        unpackedField = f['ref'].unpack(cBuf);
      }

      fieldObj[f['name']] = unpackedField;

      return new {{jstDetail.jstConstructor}}(fieldObj);
    }
  }

  throw new Error("Invalid tag for union: " + tag);
};
{{jstDetail.jstConstructor}}.unionTagWidth = {{jstUnionTagWidth}};
{{jstDetail.jstConstructor}}.fields = [
{{#jstUnionFields}}
{{#JSTDataField}}
    { name: "{{jstdfName}}", ref: {{jstdfRefCtor}}, index: {{jstdfIndex}} },
{{/JSTDataField}}
{{#JSTEmptyField}}
    { name: "{{jstefName}}", index: {{jstefIndex}} },
{{/JSTEmptyField}}
{{/jstUnionFields}}
  ];
{{/JSTUnion}}
