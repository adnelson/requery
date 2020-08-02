// Mutable map underpinned by the Map type from the javascript stdlib.
// Only works for int and string keys.
open UtilsPrelude;
module A = ArrayUtils;
module D = DictUtils;

// Map type, abstract
type map('k, 'a);

// Alias
type t('k, 'a) = map('k, 'a);

// Entries type, abstract but it's arrays of tuples under the hood
type entries('k, 'a);

[@bs.send] external entries: t('k, 'a) => entries('k, 'a) = "entries";

let entriesAsArray:
  (entries('k, 'v), array(('k, 'v)) => array(('k, 'v2))) => entries('k, 'v2) =
  (entries, f) => entries->Obj.magic->f->Obj.magic;

[@bs.send]
external mapEntries: (entries('k, 'a), (('k, 'a)) => ('k, 'b)) => entries('k, 'b) = "map";

[@bs.send]
external filterEntries: (entries('k, 'a), (('k, 'a)) => bool) => entries('k, 'b) = "filter";

[@bs.send]
external concatEntries: (entries('k, 'a), entries('k, 'a)) => entries('k, 'a) = "concat";

// TODO remove magic
let singletonEntries: ('k, 'a) => entries('k, 'a) = (k, v) => Obj.magic([|(k, v)|]);

[@bs.send] external entries: t('k, 'a) => entries('k, 'a) = "entries";
[@bs.new] external fromEntries: entries('k, 'a) => t('k, 'a) = "Map";

[@bs.send] [@bs.return nullable] external get: (t('k, 'a), 'k) => option('a) = "get";
[@bs.send] external has: (t('k, 'a), 'k) => bool = "has";
[@bs.send] external set: (t('k, 'a), 'k, 'v) => unit = "set";
[@bs.send] external delete: (t('k, 'a), 'k) => bool = "delete";
[@bs.send] external keys: t('k, 'a) => Js.Array.array_like('k) = "keys";

let keyArray: t('k, 'a) => array('k) = m => m->keys->Js.Array.from;

// Construct a map with string keys
let fromArrayStringKeys: 'k 'v. (array(('k, 'v)), 'k => string) => t('k, 'a) =
  (arr, keyToString) => {
    let entries = Obj.magic(arr->A.mapFst(keyToString));
    fromEntries(entries);
  };

// Construct a map with int keys
let fromArrayIntKeys: 'k 'v. (array(('k, 'v)), 'k => int) => t('k, 'a) =
  (arr, keyToInt) => {
    let entries = Obj.magic(arr->A.mapFst(keyToInt));
    fromEntries(entries);
  };

// Map a function over the values in a map.
let map: (t('k, 'a), 'a => 'b) => t('k, 'b) =
  (m, f) => {
    let entries = entries(m);
    fromEntries(mapEntries(entries, ((k, v)) => (k, f(v))));
  };

// Map a function over the key/value pairs in a dict.
let mapWithKeys: (t('k, 'a), (string, 'a) => 'b) => t('k, 'b) =
  (m, f) => {
    let entries = entries(m);
    fromEntries(mapEntries(entries, ((k, v)) => (k, f(k, v))));
  };

let keep: (t('k, 'a), 'a => bool) => t('k, 'a) =
  (m, f) => m->entries->filterEntries(((k, v)) => f(v))->fromEntries;

let keepMap: (t('k, 'a), 'a => option('b)) => t('k, 'b) =
  (m, f) =>
    m
    ->entries
    ->entriesAsArray(arr =>
        arr->Belt.Array.keepMap(((k, v)) => f(v)->Belt.Option.map(v' => (k, v')))
      )
    ->fromEntries;

// Set a key in a dictionary, producing a new dictionary.
// TODO remove obj.magic
let setPure: (t('k, 'a), string, 'a) => t('k, 'a) =
  (m, k, v) => {
    fromEntries(concatEntries(entries(m), Obj.magic([|(k, v)|])));
  };

let singleton: (string, 'a) => t('k, 'a) = (k, v) => fromEntries(singletonEntries(k, v));

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
