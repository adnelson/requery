// Mutable map underpinned by the Map type from the javascript stdlib.
// Note: although the type indicates that it works for an arbitrary key, the
// only usable key types are sortable primitives like int, string, etc. It's
// up to the user of this library to ensure that the key type makes sense.
// For example, polymorphic variants and opaque ints/strings can work fine too.
open UtilsPrelude;
module A = ArrayUtils;
module D = DictUtils;

// Map type, abstract
type map('k, 'a);

// Alias
type t('k, 'a) = map('k, 'a);

// Key/value pairs of a map.
type entries('k, 'a) = A.ArrayLike.t(('k, 'a));

[@bs.new] external fromEntries: entries('k, 'a) => t('k, 'a) = "Map";
[@bs.new] external fromArray: array(('k, 'a)) => t('k, 'a) = "Map";

[@bs.send] [@bs.return nullable] external get: (t('k, 'a), 'k) => option('a) = "get";
[@bs.send] external has: (t('k, 'a), 'k) => bool = "has";
[@bs.send] external set: (t('k, 'a), 'k, 'v) => unit = "set";
[@bs.send] external delete: (t('k, 'a), 'k) => bool = "delete";

[@bs.send] external entries: t('k, 'a) => entries('k, 'a) = "entries";
let entriesArray: t('k, 'a) => array(('k, 'a)) = m => m->entries->Js.Array.from;

[@bs.send] external keys: t('k, 'a) => Js.Array.array_like('k) = "keys";
let keyArray: t('k, 'a) => array('k) = m => m->keys->Js.Array.from;

[@bs.send] external values: t('k, 'a) => Js.Array.array_like('a) = "values";
let valueArray: t('k, 'a) => array('a) = m => m->values->Js.Array.from;

// Map a function over the values in a map.
let map: (t('k, 'a), 'a => 'b) => t('k, 'b) =
  (m, f) => {
    let entries = entries(m);
    fromEntries(entries->A.ArrayLike.map(((k, v)) => (k, f(v))));
  };

// Map a function over the key/value pairs in a dict.
let mapWithKey: (t('k, 'a), (string, 'a) => 'b) => t('k, 'b) =
  (m, f) => {
    let entries = entries(m);
    fromEntries(entries->A.ArrayLike.map(((k, v)) => (k, f(k, v))));
  };

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

let singleton: (string, 'a) => t('k, 'a) =
  (k, v) => fromEntries(A.(singleton((k, v))->ArrayLike.fromArray));

let getExn = (d, k) =>
  switch (get(d, k)) {
  | None => throw("No such key '" ++ k ++ "' in ")
  | Some(v) => v
  };

module String = {
  [@bs.new] external empty: unit => t(string, 'a) = "Map";
  [@bs.new] external fromArray: array((string, 'a)) => t(string, 'a) = "Map";
  [@bs.send] external toArray: t(string, 'a) => array((string, 'a)) = "entries";

  let fromBeltMap: SMap.t('a) => t(string, 'a) = map => fromArray(SMap.toArray(map));
  let toBeltMap: t(string, 'a) => SMap.t('a) = m => SMap.fromArray(toArray(m));
  let fromDict: D.t('a) => t(string, 'a) = map => fromArray(D.entries(map));
  let toDict: t(string, 'a) => D.t('a) = m => D.fromArray(toArray(m));

  // Construct from an array of keys, applying a function to each key.
  let fromArrayWith = (ks: array(string), f: string => 'a): t(string, 'a) =>
    fromArray(A.map(ks, k => (k, f(k))));
};

module Int = {
  [@bs.new] external empty: unit => t(int, 'a) = "Map";
  [@bs.new] external fromArray: array((int, 'a)) => t(int, 'a) = "Map";
  [@bs.send] external toArray: t(int, 'a) => array((int, 'a)) = "entries";

  let fromBeltMap: IMap.t('a) => t(int, 'a) = map => fromArray(IMap.toArray(map));
  let toBeltMap: t(int, 'a) => IMap.t('a) = m => IMap.fromArray(toArray(m));

  // Construct from an array of keys, applying a function to each key.
  let fromArrayWith = (ks: array(int), f: int => 'a): t(int, 'a) =>
    fromArray(A.map(ks, k => (k, f(k))));
};
