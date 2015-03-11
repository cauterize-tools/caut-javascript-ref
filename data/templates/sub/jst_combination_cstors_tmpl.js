{{#JSTCombination}}
function {{jstDetail.jstConstructor}} (fs) {
  // TODO: Make sure we don't have any extra fields.
  this.fields = fs;

  this.pack = function (cBuf) {
    var len = cBuf.length();

    // Build and pack the flags.
    var fi, flags = 0;
    for (fi in {{jstDetail.jstConstructor}}.fields) {
      f = {{jstDetail.jstConstructor}}.fields[fi];
      if (fs.hasOwnProperty(f['name'])) {
        flags |= (1 << f['index']);
      }
    }
    C.intToBytes(flags, cBuf, {{jstDetail.jstConstructor}}.combinationFlagsWidth);

    // Pack the fields that are present.
    for (fi in {{jstDetail.jstConstructor}}.fields) {
      f = {{jstDetail.jstConstructor}}.fields[fi];

      // If it's not an empty field...
      if (undefined !== f['ref']) {
        // And the field is present in the instance...
        if (undefined !== this.fields[f['name']]) {
          // Pack the field data.
          this.fields[f['name']].pack(cBuf);
        }
      }
    }

    return cBuf.length() - len;
  };
}
{{jstDetail.jstConstructor}}.unpack = function (cBuf) {
    var flags = C.bytesToInt(cBuf, {{jstDetail.jstConstructor}}.combinationFlagsWidth);
    var fs = {};
    var f;

    for (fi in {{jstDetail.jstConstructor}}.fields) {
      f = {{jstDetail.jstConstructor}}.fields[fi];

      if (0 !== ((1 << f['index']) & flags)) {
        // Ensure we set the slot in the object.
        fs[f['name']] = null;

        // if there's also associated data, unpack that
        if (undefined !== f['ref']) {
          fs[f.name] = f['ref'].unpack(cBuf);
        }
      }
    }

    return new {{jstDetail.jstConstructor}}(fs);
};
{{jstDetail.jstConstructor}}.combinationFlagsWidth = {{jstCombinationFlagsWidth}};
{{jstDetail.jstConstructor}}.fields = [
{{#jstCombinationFields}}
{{#JSTDataField}}
    { name: "{{jstdfName}}", ref: {{jstdfRefCtor}}, index: {{jstdfIndex}} },
{{/JSTDataField}}
{{#JSTEmptyField}}
    { name: "{{jstefName}}", index: {{jstefIndex}} },
{{/JSTEmptyField}}
{{/jstCombinationFields}}
  ];
{{/JSTCombination}}
