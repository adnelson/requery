open UtilsPrelude;
module A = ArrayUtils;
module D = DictUtils;
module P = PromiseUtils;

module Decode = {
  include Json.Decode;
  type fromJson('a) = decoder('a);
  external json: Js.Json.t => Js.Json.t = "%identity";
  let strMap: decoder('a) => decoder(SMap.t('a)) =
    (inner, obj) => obj |> dict(inner) |> D.entries |> SMap.fromArray;

  // Run two decoders on the same input
  let tup2: (decoder('a), decoder('b)) => decoder(('a, 'b)) =
    (first, second, obj) => (obj |> first, obj |> second);

  let tup3: (decoder('a), decoder('b), decoder('c)) => decoder(('a, 'b, 'c)) =
    (f1, f2, f3, obj) => (obj |> f1, obj |> f2, obj |> f3);

  let tup4: (decoder('a), decoder('b), decoder('c), decoder('d)) => decoder(('a, 'b, 'c, 'd)) =
    (f1, f2, f3, f4, obj) => (obj |> f1, obj |> f2, obj |> f3, obj |> f4);

  let tup5:
    (decoder('a), decoder('b), decoder('c), decoder('d), decoder('e)) =>
    decoder(('a, 'b, 'c, 'd, 'e)) =
    (f1, f2, f3, f4, f5, obj) => (obj |> f1, obj |> f2, obj |> f3, obj |> f4, obj |> f5);

  let strMapWithKey: (string => decoder('a)) => decoder(SMap.t('a)) =
    (inner, obj) => {
      let entries = obj |> dict(x => x) |> D.entries;
      SMap.fromArray(ArrayUtils.map(entries, ((k, v)) => (k, inner(k, v))));
    };

  // Can parse either a JSON float, or a float-like string.
  let floatString: decoder(float) = oneOf([float, obj => obj |> string |> float_of_string]);

  // Can parse either a JSON int, or a int-like string.
  let intString: decoder(int) = oneOf([int, obj => obj |> string |> int_of_string]);

  let numberOrString: decoder(string) =
    oneOf([string, obj => obj |> int |> string_of_int, obj => obj |> float |> Js.Float.toString]);
};

module Encode = {
  include Json.Encode;
  external json: Js.Json.t => Js.Json.t = "%identity";
  let strMap: encoder('t) => encoder(SMap.t('t)) = (enc, map) => dict(enc, D.fromMap(map));
  let object1: (string, encoder('a)) => encoder('a) =
    (key, encodeInner, inner) => object_([(key, encodeInner(inner))]);
};

let pretty: Js.Json.t => string = [%bs.raw {|json => JSON.stringify(json, null, 2)|}];
let pretty_ = pretty; // alias to avoid name clash below
let rLog = (~pretty=false, enc: Encode.encoder('a), obj: 'a) =>
  P.rLog((pretty ? pretty_ : Json.stringify)(enc(obj)));
let rLogReturn = (~pretty=false, enc: Encode.encoder('a)) =>
  P.rLogReturn(obj => (pretty ? pretty_ : Json.stringify)(enc(obj)));
let rLogJson = rLog(Encode.json);
let rLog2 = (~pretty=false, encA: Encode.encoder('a), encB: Encode.encoder('b), a: 'a, b: 'b) => {
  let toStr = pretty ? pretty_ : Json.stringify;
  P.rLog2(toStr(encA(a)), toStr(encB(b)));
};

type fromJson('a) = Json.Decode.decoder('a);
type toJson('a) = Json.Encode.encoder('a);
