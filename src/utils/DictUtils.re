open UtilsPrelude;

include Js.Dict;
module A = ArrayUtils;

// Map a function over the values in a dict.
let map: (t('a), 'a => 'b) => t('b) =
  (dict, f) => {
    let entries = entries(dict);
    fromArray(A.map(entries, ((k, v)) => (k, f(v))));
  };

// Map a function over the key/value pairs in a dict.
let mapWithKeys: (t('a), (string, 'a) => 'b) => t('b) =
  (dict, f) => {
    let entries = entries(dict);
    fromArray(A.map(entries, ((k, v)) => (k, f(k, v))));
  };

let fromMap: SMap.t('a) => t('a) = map => fromArray(SMap.toArray(map));
let toMap: t('a) => SMap.t('a) = dict => SMap.fromArray(entries(dict));

// Set a key in a dictionary, producing a new dictionary.
let setPure: (t('a), string, 'a) => t('a) =
  (dict, k, v) => {
    fromArray(A.concat(entries(dict), [|(k, v)|]));
  };

let singleton: (string, 'a) => t('a) = (k, v) => fromArray([|(k, v)|]);

let getExn = (d, k) =>
  switch (Js.Dict.get(d, k)) {
  | None => throw("No such key '" ++ k ++ "' in ")
  | Some(v) => v
  };

// Construct from an array of keys, applying a function to each key.
let fromKeys = (ks: array(string), f: string => 'a): Js.Dict.t('a) =>
  Js.Dict.fromArray(A.map(ks, k => (k, f(k))));

// I really shouldn't have to be implementing this myself but ohhhh wellll
let has = (dict, key) => Belt.Option.isSome(get(dict, key));
