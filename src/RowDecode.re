module A = Utils.Array;
module J = Utils.Json.Decode;
type decoder('t) = array(Js.Json.t) => 't;

type error =
  | RowDecodeError(int, Js.Json.t, string)
  | EmptyRows;

let errorToStringShort: error => string =
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

let dict =
    (
      ~keyField: string,
      ~keyDecode: J.decoder(string)=J.string,
      ~valueField: string,
      ~valueDecode: J.decoder('a),
    )
    : decoder(Js.Dict.t('a)) =>
  decodeRows(
    J.(pair(field(keyField, keyDecode), field(valueField, valueDecode))),
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
    obj =>
      (
        obj |> J.field(innerKeyField, innerKeyDecode),
        obj |> J.field(outerKeyField, outerKeyDecode),
        obj |> J.field(valueField, valueDecode),
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
