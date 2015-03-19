{{#JSTCombination}}
function {{jstDetail.jstConstructor}}(elems) { prot.CCombination.call(this, elems); }
(function () {
  var typeHash = [{{#jstDetail.jstHash}}{{.}},{{/jstDetail.jstHash}}];
  var typeSize = { min: {{jstDetail.jstSize.jstMinSize}}, max: {{jstDetail.jstSize.jstMaxSize}} };
  var fields = [
  {{#jstCombinationFields}}
  {{#JSTDataField}}
      { name: "{{jstdfName}}", index: {{jstdfIndex}}, ref: {{jstdfRefCtor}} },
  {{/JSTDataField}}
  {{#JSTEmptyField}}
      { name: "{{jstefName}}", index: {{jstefIndex}} },
  {{/JSTEmptyField}}
  {{/jstCombinationFields}}
    ];
  prot.mkCombination({{jstDetail.jstConstructor}}, '{{jstName}}', fields, typeHash, typeSize);
}());
{{/JSTCombination}}
