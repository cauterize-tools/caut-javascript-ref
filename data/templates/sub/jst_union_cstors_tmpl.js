{{#JSTUnion}}
function {{jstDetail.jstConstructor}}(elems) { prot.CUnion.call(this, elems); }
(function () {
  var typeHash = [{{#jstDetail.jstHash}}{{.}},{{/jstDetail.jstHash}}];
  var typeSize = { min: {{jstDetail.jstSize.jstMinSize}}, max: {{jstDetail.jstSize.jstMaxSize}} };
  var fields = [
{{#jstUnionFields}}
{{#JSTDataField}}
      { name: "{{jstdfName}}", index: {{jstdfIndex}}, ref: {{jstdfRefCtor}} },
{{/JSTDataField}}
{{#JSTEmptyField}}
      { name: "{{jstefName}}", index: {{jstefIndex}} },
{{/JSTEmptyField}}
{{/jstUnionFields}}
    ];
  prot.mkUnion({{jstDetail.jstConstructor}}, '{{jstName}}', fields, {{jstUnionTagWidth}}, typeHash, typeSize);
}());
{{/JSTUnion}}
