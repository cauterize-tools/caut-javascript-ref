{{#JSTRecord}}
function {{jstDetail.jstConstructor}}(elems) { prot.CRecord.call(this, elems); }
(function () {
  var typeHash = [{{#jstDetail.jstHash}}{{.}},{{/jstDetail.jstHash}}];
  var typeSize = { min: {{jstDetail.jstSize.jstMinSize}}, max: {{jstDetail.jstSize.jstMaxSize}} };
  var fields = [
{{#jstRecordFields}}
{{#JSTDataField}}
      { name: "{{jstdfName}}", index: {{jstdfIndex}}, ref: {{jstdfRefCtor}} },
{{/JSTDataField}}
{{#JSTEmptyField}}
      { name: "{{jstefName}}", index: {{jstefIndex}} },
{{/JSTEmptyField}}
{{/jstRecordFields}}
    ];
  prot.mkRecord({{jstDetail.jstConstructor}}, '{{jstName}}', fields, typeHash, typeSize);
}());
{{/JSTRecord}}
