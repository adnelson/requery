// Extensions to the bare bindings to the built-in Map type in JavaScript.
open UtilsPrelude;
module A = ArrayUtils;
module D = DictUtils;
include JsMap;

// Key/value pairs of a map as an array.
let entriesArray: t('k, 'a) => array(('k, 'a)) = m => m->entries->Js.Array.from;

// Keys of a map as an array.
let keysArray: t('k, 'a) => array('k) = m => m->keys->Js.Array.from;

// Values of a map as an array.
let valuesArray: t('k, 'a) => array('a) = m => m->values->Js.Array.from;

// Map a function over the values in a map.
let map: (t('k, 'a), 'a => 'b) => t('k, 'b) =
  (m, f) => m->entries->A.ArrayLike.map(((k, v)) => (k, f(v)))->fromEntries;

// Map a function over the key/value pairs in a dict.
let mapWithKey: (t('k, 'a), (string, 'a) => 'b) => t('k, 'b) =
  (m, f) => m->entries->A.ArrayLike.map(((k, v)) => (k, f(k, v)))->fromEntries;

let keep: (t('k, 'a), 'a => bool) => t('k, 'a) =
  (m, f) => m->entries->A.ArrayLike.filter(((_, v)) => f(v))->fromEntries;

let keepWithKey: (t('k, 'a), ('k, 'a) => bool) => t('k, 'a) =
  (m, f) => m->entries->A.ArrayLike.filter(((k, v)) => f(k, v))->fromEntries;

let keepMap: (t('k, 'a), 'a => option('b)) => t('k, 'b) =
  (m, f) =>
    m
    ->entries
    ->A.ArrayLike.onArray(arr =>
        arr->Belt.Array.keepMap(((k, v)) => f(v)->Belt.Option.map(v' => (k, v')))
      )
    ->fromEntries;

let keepMapWithKey: (t('k, 'a), ('k, 'a) => option('b)) => t('k, 'b) =
  (m, f) =>
    m
    ->entries
    ->A.ArrayLike.onArray(arr =>
        arr->Belt.Array.keepMap(((k, v)) => f(k, v)->Belt.Option.map(v' => (k, v')))
      )
    ->fromEntries;

// Set a key in a dictionary, producing a new dictionary.
let setPure: (t('k, 'a), string, 'a) => t('k, 'a) =
  (m, k, v) => fromEntries(m->entries->A.ArrayLike.concatArray([|(k, v)|]));

// Create a map with a single key and value
let singleton: (string, 'a) => t('k, 'a) =
  (k, v) => fromEntries(A.(singleton((k, v))->ArrayLike.fromArray));

// Get or throw an exception if the key is not found.
let getOrRaise: (t('k, 'v), 'k, 'k => exn) => 'v =
  (m, k, toExn) =>
    switch (get(m, k)) {
    | None => raise(k->toExn)
    | Some(v) => v
    };

// Look up the key in the dictionary; if it's not in it, add the
// given default to the map and then return it.
let getOrSetDefaultMut: (t('k, 'v), 'k, 'v) => 'v =
  (m, k, default) =>
    switch (m->get(k)) {
    | None =>
      m->setMut(k, default)->ignore;
      default;
    | Some(v) => v
    };

// Group items in an array by a common result to a function. The key
// type must be hashable by javascript -- the type system can't guarantee this.
let groupBy: (array('a), 'a => 'k) => t('k, array('a)) =
  (arr, f) => {
    let result = empty();
    arr->A.forEach(item => {
      let key = item->f;
      let group = result->getOrSetDefaultMut(key, [||]);
      group->A.pushMut(item);
    });
    result;
  };

// Group an array of tuples. Each first element is a key in the resulting
// map, and each second element appears in an array with other values
// sharing the same key.
let groupTuples: array(('k, 'v)) => t('k, array('v)) = (arr) => {
    let result = empty();
    arr->A.forEach(((key, item)) => {
      let group = result->getOrSetDefaultMut(key, [||]);
      group->A.pushMut(item);
    });
    result;
};

// Convert string maps to/from their equivalents in Belt
module String = {
  let fromBeltMap: SMap.t('a) => t(string, 'a) = map => fromArray(SMap.toArray(map));
  let toBeltMap: t(string, 'a) => SMap.t('a) = m => SMap.fromArray(toArray(m));
  let fromDict: D.t('a) => t(string, 'a) = map => fromArray(D.entries(map));
  let toDict: t(string, 'a) => D.t('a) = m => D.fromArray(toArray(m));
};

// Convert int maps to/from equivalents in Belt
module Int = {
  let fromBeltMap: IMap.t('a) => t(int, 'a) = map => fromArray(IMap.toArray(map));
  let toBeltMap: t(int, 'a) => IMap.t('a) = m => IMap.fromArray(toArray(m));
};
