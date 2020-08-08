open UtilsPrelude;
include Js.Json;
module A = ArrayUtils;
module D = DictUtils;
module P = PromiseUtils;

type fromJson('a) = Js.Json.t => 'a;
type toJson('a) = 'a => Js.Json.t;

let parseJsonAsResult: string => result(t, string) =
  s =>
    try(Ok(parseExn(s))) {
    | _ => Error(s)
    };

let parseJsonAsOption: string => option(t) =
  s =>
    switch (s->parseJsonAsResult) {
    | Error(_) => None
    | Ok(json) => Some(json)
    };

[@bs.val]
external jsonStringify: (t, Js.Nullable.t(unit), option(int)) => string = "JSON.stringify";

let stringify = (~indent=?, json) => json->jsonStringify(Js.Nullable.null, indent);
let stringifyWith = (~indent=?, toJson, obj) =>
  obj->toJson->jsonStringify(Js.Nullable.null, indent);

let logAsJson = (~indent=?, enc: toJson('a), obj: 'a) =>
  Js.Console.log(stringify(~indent?, enc(obj)));

let logJson = (~indent=?, json) => json |> logAsJson(~indent?, j => j);

let logAsJson2 = (~indent=?, encA: toJson('a), encB: toJson('b), a: 'a, b: 'b) =>
  Js.Console.log2(stringify(~indent?, encA(a)), stringify(~indent?, encB(b)));

let logAsJson3 =
    (~indent=?, encA: toJson('a), encB: toJson('b), encC: toJson('c), a: 'a, b: 'b, c: 'c) =>
  Js.Console.log3(
    stringify(~indent?, encA(a)),
    stringify(~indent?, encB(b)),
    stringify(~indent?, encC(c)),
  );

let logAsJson4 =
    (
      ~indent=?,
      encA: toJson('a),
      encB: toJson('b),
      encC: toJson('c),
      encD: toJson('d),
      a: 'a,
      b: 'b,
      c: 'c,
      d: 'd,
    ) =>
  Js.Console.log4(
    stringify(~indent?, encA(a)),
    stringify(~indent?, encB(b)),
    stringify(~indent?, encC(c)),
    stringify(~indent?, encD(d)),
  );

// Traverse a JSON structure with a function
let rec reduce: 'a. (Js.Json.t, 'a, ('a, Js.Json.t) => 'a) => 'a =
  (json, result, f) => {
    let newResult = f(result, json);
    switch (json->classify) {
    | JSONFalse
    | JSONTrue
    | JSONNull
    | JSONString(_)
    | JSONNumber(_) => newResult
    | JSONArray(arr) => arr->A.reduce(newResult, (r, j) => j->reduce(r, f))
    | JSONObject(obj) => obj->D.reduce(newResult, (r, j) => j->reduce(r, f))
    };
  };

module Decode = {
  include Json.Decode;
  type fromJson('a) = decoder('a);
  external json: Js.Json.t => Js.Json.t = "%identity";

  // Parse a string containing JSON
  let jsonString: fromJson(t) =
    j =>
      switch (j->string->parseJsonAsResult) {
      | Ok(json) => json
      | Error(e) => raise(DecodeError(e))
      };

  let embedded: 'a. fromJson('a) => fromJson('a) = (dec, j) => j |> jsonString |> dec;

  let strMap: fromJson('a) => fromJson(SMap.t('a)) =
    (inner, obj) => obj |> dict(inner) |> D.entries |> SMap.fromArray;

  // Run two decoders on the same input
  let tup2: (fromJson('a), fromJson('b)) => fromJson(('a, 'b)) =
    (first, second, obj) => (obj |> first, obj |> second);

  let tup3: (fromJson('a), fromJson('b), fromJson('c)) => fromJson(('a, 'b, 'c)) =
    (f1, f2, f3, obj) => (obj |> f1, obj |> f2, obj |> f3);

  let tup4:
    (fromJson('a), fromJson('b), fromJson('c), fromJson('d)) => fromJson(('a, 'b, 'c, 'd)) =
    (f1, f2, f3, f4, obj) => (obj |> f1, obj |> f2, obj |> f3, obj |> f4);

  let tup5:
    (fromJson('a), fromJson('b), fromJson('c), fromJson('d), fromJson('e)) =>
    fromJson(('a, 'b, 'c, 'd, 'e)) =
    (f1, f2, f3, f4, f5, obj) => (obj |> f1, obj |> f2, obj |> f3, obj |> f4, obj |> f5);

  // Passes the key as a first argument to the decoder, allowing you to
  // customize decoder behavior based on key
  let dictWithKey: (string => fromJson('a)) => fromJson(D.t('a)) =
    (inner, obj) => dict(json, obj)->D.mapWithKey(inner);

  // Passes the key as a first argument to the decoder, allowing you to
  // customize decoder behavior based on key
  let strMapWithKey: (string => fromJson('a)) => fromJson(SMap.t('a)) =
    (inner, obj) => {
      let entries = obj |> dict(x => x) |> D.entries;
      SMap.fromArray(ArrayUtils.map(entries, ((k, v)) => (k, inner(k, v))));
    };

  // Can parse either a JSON float, or a float-like string.
  let floatString: fromJson(float) = oneOf([float, obj => obj |> string |> float_of_string]);

  // Can parse either a JSON int, or a int-like string.
  let intString: fromJson(int) = oneOf([int, obj => obj |> string |> int_of_string]);

  let numberOrString: fromJson(string) =
    oneOf([string, obj => obj |> int |> string_of_int, obj => obj |> float |> Js.Float.toString]);

  // Given an optional value, fail with a DecodeError if it's None.
  let getSome: (decoder('a), option('a)) => 'a =
    (dec, opt) =>
      switch (opt) {
      | Some(inner) => inner |> dec
      | None => raise(DecodeError("Option contained `None` when `Some` es expected"))
      };
};

module Encode = {
  include Json.Encode;
  type toJson('a) = encoder('a);
  external json: Js.Json.t => Js.Json.t = "%identity";
  let strMap: toJson('t) => toJson(SMap.t('t)) = (enc, map) => dict(enc, D.fromMap(map));
  let object1: (string, toJson('a)) => toJson('a) =
    (key, encodeInner, inner) => object_([(key, encodeInner(inner))]);
};
