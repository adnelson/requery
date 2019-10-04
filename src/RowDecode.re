module A = Utils.Array;
module J = Utils.Json.Decode;
type decoder('t) = array(Js.Json.t) => 't;

type error =
  | RowDecodeError(int, Js.Json.t, string)
  | EmptyRows;

let encodeError =
  Utils.Json.Encode.(
    fun
    | RowDecodeError(num, rowJson, message) =>
      object_([
        ("number", num |> int),
        ("rowJson", rowJson |> json),
        ("message", message |> string),
      ])
    | EmptyRows => "EmptyRows" |> string
  );

let errorToString: error => string =
  fun
  | RowDecodeError(n, _, str) =>
    "Error occurred when parsing row " ++ string_of_int(n) ++ ": " ++ str
  | EmptyRows => "No rows to parse";

exception Error(error);

let decodeRow = (i, decode, row) =>
  try (decode(row)) {
  | J.DecodeError(err) => raise(Error(RowDecodeError(i, row, err)))
  };

let decodeRows = (decode, next, rows) => {
  next(A.mapWithIndex(rows, (i, row) => row |> decodeRow(i, decode)));
};

let optColumn: (string, J.decoder('t)) => decoder(option('t)) =
  (col, dec) =>
    fun
    | [||] => None
    | rows => Some(rows[0] |> J.field(col, dec));

let column: (string, J.decoder('t)) => decoder('t) =
  (col, dec) =>
    fun
    | [||] => raise(Error(EmptyRows))
    | rows => rows[0] |> decodeRow(0, J.field(col, dec));

// Given a way to get a (key, value) pair from a row, produce a
// dictionary with those keys/values.
let dict =
    (
      ~keyField: string,
      ~keyDecode: J.decoder(string)=J.string,
      ~valueField: string,
      ~valueDecode: J.decoder('a),
    )
    : decoder(Js.Dict.t('a)) =>
  decodeRows(
    J.(tup2(field(keyField, keyDecode), field(valueField, valueDecode))),
    Js.Dict.fromArray,
  );

let nestedDict =
    (
      ~outerKeyField: string,
      ~outerKeyDecode: J.decoder(string)=J.string,
      ~innerKeyField: string,
      ~innerKeyDecode: J.decoder(string)=J.string,
      ~valueField: string,
      ~valueDecode: J.decoder('a),
    )
    : decoder(Js.Dict.t(Js.Dict.t('a))) =>
  decodeRows(
    J.tup3(
      J.field(innerKeyField, innerKeyDecode),
      J.field(outerKeyField, outerKeyDecode),
      J.field(valueField, valueDecode),
    ),
    decoded => {
      let result = Js.Dict.empty();
      A.forEach(decoded, ((inner, outer, value)) =>
        switch (Js.Dict.get(result, outer)) {
        | None => Js.Dict.set(result, outer, Js.Dict.fromArray([|(inner, value)|]))
        | Some(values) => Js.Dict.set(values, inner, value)
        }
      );
      result;
    },
  );
