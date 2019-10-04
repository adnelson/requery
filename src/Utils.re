module SMap = Belt.Map.String;
module A = Belt.Array;

// Convert a string to an int, handling failure with an option type.
let parseInt = str =>
  switch (int_of_string(str)) {
  | i => Some(i)
  | exception _ => None
  };

// Convert a string to an float, handling failure with an option type.
let parseFloat = str =>
  switch (float_of_string(str)) {
  | i => Some(i)
  | exception _ => None
  };

type decoder('a) = Json.Decode.decoder('a);
type encoder('a) = Json.Encode.encoder('a);

module Log = {
  [@bs.val] external error: 'a => unit = "console.error";
  [@bs.val] external error2: ('a, 'b) => unit = "console.error";
  [@bs.val] external error3: ('a, 'b, 'c) => unit = "console.error";
  [@bs.val] external error4: ('a, 'b, 'c, 'd) => unit = "console.error";
};

let throw: string => 'a = [%raw message => {| throw new Error(message); |}];

module Promise = {
  include Js.Promise;

  let transform: ('a => 'b, t('a)) => t('b) = (f, prom) => prom |> then_(x => resolve(f(x)));
  let rLog: 'a => Js.Promise.t(unit) = x => Js.Promise.resolve(Js.log(x));
  let rLog2: ('a, 'b) => Js.Promise.t(unit) = (a, b) => Js.Promise.resolve(Js.log2(a, b));
  let rLogReturn: ('a => 'b, 'a) => Js.Promise.t('a) =
    (toLog, x) => {
      Js.log(toLog(x));
      Js.Promise.resolve(x);
    };
  let pair: (t('a), t('b)) => t(('a, 'b)) = (p1, p2) => Js.Promise.all2((p1, p2));
  exception Error(error);
  let finally: (unit => t(unit), t('a)) => t('a) =
    (action, prom) =>
      prom
      |> then_(result => {
           ignore(action());
           resolve(result);
         })
      |> catch(err => {
           ignore(action());
           reject(Error(err));
         });
};

module Dict = {
  include Js.Dict;

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
    | None => throw("No such key '" ++ k ++ "'")
    | Some(v) => v
    };

  // Construct from an array of keys, applying a function to each key.
  let fromKeys = (ks: array(string), f: string => 'a): Js.Dict.t('a) =>
    Js.Dict.fromArray(A.map(ks, k => (k, f(k))));
};

module Option = {
  include Belt.Option;

  // like getWithDefault, but obtains its default thru a lazily-evaluated function.
  let getWithDefaultLazy: (option('a), unit => 'a) => 'a =
    (opt, make) =>
      switch (opt) {
      | Some(x) => x
      | None => make()
      };

  let mapString: (option('a), 'a => string) => string =
    (opt, f) =>
      switch (opt) {
      | None => ""
      | Some(x) => f(x)
      };
};

// Extra functions on arrays
module Array = {
  include Belt.Array;
  let max: array(float) => float = arr => reduce(arr, neg_infinity, max);
  let min: array(float) => float = arr => reduce(arr, infinity, min);
  let contains: (array('a), 'a) => bool = (arr, elem) => some(arr, e => e == elem);
  let joinWith: (array(string), string) => string = (arr, sep) => Js.Array.joinWith(sep, arr);
  let mapJoin: (array('a), ~prefix: string=?, ~suffix: string=?, string, 'a => string) => string =
    (arr, ~prefix="", ~suffix="", sep, f) => prefix ++ joinWith(map(arr, f), sep) ++ suffix;
  let mapJoinWith: (array('a), string, 'a => string) => string =
    (arr, sep, f) => joinWith(map(arr, f), sep);
  let mapJoinCommas: (array('a), 'a => string) => string =
    (arr, f) => mapJoinWith(arr, ", ", f);
  let mapJoinIfNonEmpty:
    (array('a), ~onEmpty: string=?, ~prefix: string=?, ~suffix: string=?, string, 'a => string) =>
    string =
    (arr, ~onEmpty="", ~prefix="", ~suffix="", sep, f) =>
      switch (arr) {
      | [||] => onEmpty
      | _ => mapJoin(arr, ~prefix, ~suffix, sep, f)
      };
  //  let mapJoinCommasIfNonEmpty: (array('a), ~prefix: string=?, 'a => string) => string =
  //    (arr, ~prefix="", f) => mapJoinWithIfNonEmpty(arr, ~prefix, ", ", f);
  //
  // like map, but argument order flipped
  let flipMap: ('a => 'b, array('a)) => array('b) = (f, a) => map(a, f);

  // like forEach, but reverse argument order
  let flipForEach: ('a => 'b, array('a)) => unit = (f, a) => forEach(a, f);

  // Find the first item in the array which matches a predicate, or return None.
  let find: (array('a), 'a => bool) => option('a) =
    (arr, test) =>
      switch (keep(arr, test)) {
      | [||] => None
      | matches => Some(matches[0])
      };

  // Find the first item in the array which matches a predicate, or raise an error.
  let findExn: (array('a), 'a => bool) => 'a =
    (arr, test) =>
      switch (find(arr, test)) {
      | None => throw("No matching element in array")
      | Some(m) => m
      };

  // Mutates arr, adding each element of arr' to it.
  let extend = (arr: array('a), arr': array('a)): unit => {
    A.forEach(
      arr',
      elem => {
        let _ = Js.Array.push(elem, arr);
        ();
      },
    );
  };

  // Flatten an array of arrays.
  let flat = (arr: array(array('a))): array('a) => {
    let res: array('a) = [||];
    A.forEach(arr, innerArr => extend(res, innerArr));
    res;
  };

  // map and then flatten
  let flatMap = (arr: array('a), f: 'a => array('b)): array('b) => flat(map(arr, f));

  let sum = (arr: array(float)): float => reduce(arr, 0.0, (+.));

  // Cross-product two arrays, applying a function to each pair.
  let cross = (arr1: array('a), arr2: array('b), f: ('a, 'b) => 'c): array('c) => {
    flatMap(arr1, a => map(arr2, b => f(a, b)));
  };

  // Same as cross but operating on three arrays.
  let cross3 =
      (arr1: array('a), arr2: array('b), arr3: array('c), f: ('a, 'b, 'c) => 'd): array('d) => {
    flatMap(arr1, a => flatMap(arr2, b => A.map(arr3, c => f(a, b, c))));
  };

  // Get the values of all of the `Some()` variants in an array of options.
  let keepSome = (arr: array(option('a))): array('a) => keepMap(arr, x => x);

  // Create a singleton array
  let singleton = x => [|x|];
  // Same as `map`, but with the arguments order reversed.
  //  let map' = (f: 'a => 'b, arr: array('a)): array('b) => map(arr, f);

  // Return a new array with the given index set to the given value.
  let setPure = (arr, i, x) => {
    let arr' = copy(arr);
    let _ = set(arr', i, x);
    arr';
  };
};

// Extra functions on lists
module List = {
  include Belt.List;
  let amap: (list('a), 'a => 'b) => array('b) = (l, f) => toArray(map(l, f));
};

module Json = {
  module Decode = {
    include Json.Decode;
    external json: Js.Json.t => Js.Json.t = "%identity";
    let strMap: decoder('a) => decoder(SMap.t('a)) =
      (inner, obj) => obj |> dict(inner) |> Js.Dict.entries |> SMap.fromArray;

    // Run two decoders on the same input
    let tup2: (decoder('a), decoder('b)) => decoder(('a, 'b)) =
      (first, second, obj) => (obj |> first, obj |> second);

    let tup3: (decoder('a), decoder('b), decoder('c)) => decoder(('a, 'b, 'c)) =
      (f1, f2, f3, obj) => (obj |> f1, obj |> f2, obj |> f3);

    let strMapWithKey: (string => decoder('a)) => decoder(SMap.t('a)) =
      (inner, obj) => {
        let entries = obj |> dict(x => x) |> Js.Dict.entries;
        SMap.fromArray(A.map(entries, ((k, v)) => (k, inner(k, v))));
      };

    // Can parse either a JSON float, or a float-like string.
    let floatString: decoder(float) = oneOf([float, obj => obj |> string |> float_of_string]);

    // Can parse either a JSON int, or a int-like string.
    let intString: decoder(int) = oneOf([int, obj => obj |> string |> int_of_string]);

    let numberOrString: decoder(string) =
      oneOf([
        string,
        obj => obj |> int |> string_of_int,
        obj => obj |> float |> Js.Float.toString,
      ]);
  };

  module Encode = {
    include Json.Encode;
    external json: Js.Json.t => Js.Json.t = "%identity";
    let strMap: encoder('t) => encoder(SMap.t('t)) =
      (enc, map) => dict(enc, Dict.fromMap(map));
    let object1: (string, encoder('a)) => encoder('a) =
      (key, encodeInner, inner) => object_([(key, encodeInner(inner))]);
  };

  let pretty: Js.Json.t => string = [%bs.raw {|json => JSON.stringify(json, null, 2)|}];
  let pretty_ = pretty; // alias to avoid name clash below
  let rLog = (~pretty=false, enc: Encode.encoder('a), obj: 'a) =>
    Promise.rLog((pretty ? pretty_ : Json.stringify)(enc(obj)));
  let rLogReturn = (~pretty=false, enc: Encode.encoder('a)) =>
    Promise.rLogReturn(obj => (pretty ? pretty_ : Json.stringify)(enc(obj)));
  let rLogJson = rLog(Encode.json);
};
