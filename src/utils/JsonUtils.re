open UtilsPrelude;
include Js.Json;
module A = ArrayUtils;
module D = DictUtils;
module P = PromiseUtils;

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
};

module Encode = {
  include Json.Encode;
  type toJson('a) = encoder('a);
  external json: Js.Json.t => Js.Json.t = "%identity";
  let strMap: toJson('t) => toJson(SMap.t('t)) = (enc, map) => dict(enc, D.fromMap(map));
  let object1: (string, toJson('a)) => toJson('a) =
    (key, encodeInner, inner) => object_([(key, encodeInner(inner))]);
};

let pretty: Js.Json.t => string = [%bs.raw {|json => JSON.stringify(json, null, 2)|}];
let pretty_ = pretty; // alias to avoid name clash below
let rLog = (~pretty=false, enc: Encode.toJson('a), obj: 'a) =>
  P.rLog((pretty ? pretty_ : Json.stringify)(enc(obj)));
let rLogReturn = (~pretty=false, enc: Encode.toJson('a)) =>
  P.rLogReturn(obj => (pretty ? pretty_ : Json.stringify)(enc(obj)));
let rLogJson = rLog(Encode.json);
let rLog2 = (~pretty=false, encA: Encode.toJson('a), encB: Encode.toJson('b), a: 'a, b: 'b) => {
  let toStr = pretty ? pretty_ : Json.stringify;
  P.rLog2(toStr(encA(a)), toStr(encB(b)));
};

type fromJson('a) = Decode.fromJson('a);
type toJson('a) = Encode.toJson('a);
