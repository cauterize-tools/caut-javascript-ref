{{#JSTSynonym}}
function {{jstDetail.jstConstructor}}(elems) { prot.CSynonym.call(this, elems); }
(function () {
  var typeHash = [{{#jstDetail.jstHash}}{{.}},{{/jstDetail.jstHash}}];
  var typeSize = { min: {{jstDetail.jstSize.jstMinSize}}, max: {{jstDetail.jstSize.jstMaxSize}} };
  prot.mkCombination({{jstDetail.jstConstructor}}, '{{jstName}}', {{jstSynnedCtor}}, typeHash, typeSize);
}());
{{/JSTSynonym}}
