'use strict';

/* Section: Type Dictionary
 *
 * An object to manage a dictionary of types.
 *
 * Private objects
 *
 *    - TypeDict
 */

/* Constructs a new type dictionary from an array of type objects generated
 * by caut-javascript-ref. Type objects must have the following form:
 *
 *    { name: "[type name]", ctor: [type constructor] }
 */
function TypeDict(types) {
  this.types = types;
}

/* Returns the first type where the provided prefix is equal to a prefix of
 * the hash of the same length.
 *
 * If no type is found with that prefix, and exception is raised.  */
TypeDict.prototype.typeWithHashPrefixE = function (prefix) {
  function prefixMatches(tag) {
    var i;

    for (i = 0; i < prefix.length; i++) {
      if (prefix[i] !== tag[i]) { return false; }
    }

    return true;
  }

  var tyName;
  for (tyName in this.types) {
    if (this.types.hasOwnProperty(tyName)) {
      if (prefixMatches(this.types[tyName].hash)) {
        return { name: tyName, ctor: this.types[tyName] };
      }
    }
  }

  throw new Error("Could not find type with prefix: " + prefix.toString());
};

