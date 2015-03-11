{{#JSTRecord}}
function {{jstDetail.jstConstructor}} (fs) {
  var fi, v, k;

  // Make sure we have all the fields we expect.
  for (fi in {{jstDetail.jstConstructor}}.fields) {
    f = {{jstDetail.jstConstructor}}.fields[fi];
    if (fs[f.name] === undefined) {
      throw new Error("Field missing! " + f.name);
    }
  }

  // TODO: Make sure we don't have any extra fields.

  this.fields = fs;

  this.pack = function (cBuf) {
    var len = cBuf.length();
    var f, fi;

    for (fi in {{jstDetail.jstConstructor}}.fields) {
      f = {{jstDetail.jstConstructor}}.fields[fi];
      this.fields[f['name']].pack(cBuf);
    }

    return cBuf.length() - len;
  };
}
{{jstDetail.jstConstructor}}.unpack = function (cBuf) {
    var f;
    var fs = {};

    for (fi in {{jstDetail.jstConstructor}}.fields) {
      f = {{jstDetail.jstConstructor}}.fields[fi];
      if (undefined !== f['ref']) {
        fs[f.name] = f['ref'].unpack(cBuf);
      }
    }

    return new {{jstDetail.jstConstructor}}(fs);
};
{{jstDetail.jstConstructor}}.fields = [
{{#jstRecordFields}}
{{#JSTDataField}}
    { name: "{{jstdfName}}", ref: {{jstdfRefCtor}}, index: {{jstdfIndex}} },
{{/JSTDataField}}
{{#JSTEmptyField}}
    { name: "{{jstefName}}", index: {{jstefIndex}} },
{{/JSTEmptyField}}
{{/jstRecordFields}}
  ];
{{/JSTRecord}}
