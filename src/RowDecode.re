module A = Utils.Array;
module J = Utils.Json.Decode;

type error =
  | RowDecodeError(int, Js.Json.t, string)
  | EmptyRows;
exception Error(error);

module Row = {
  type t('a) = {
    index: int,
    contents: 'a,
  };
  let make = (index: int, contents: 'a) => {index, contents};
  let map = (row, f) => {...row, contents: f(row.contents)};
  let mapGet = (row, f) => f(row.contents);

  let decodeJson: (t(Js.Json.t), J.decoder('a)) => 'a =
    (row, decode) =>
      try (mapGet(row, decode)) {
      | J.DecodeError(err) => raise(Error(RowDecodeError(row.index, row.contents, err)))
      };
};

type row = Row.t(Js.Json.t);

type decodeRows('t) = array(row) => 't;

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

let toRows: array('a) => array(Row.t('a)) = rs => A.mapWithIndex(rs, Row.make);

let getFirst: array(Row.t('a)) => Row.t('a) =
  fun
  | [||] => raise(Error(EmptyRows))
  | rows => rows[0];

let decodeFirst: J.decoder('t) => decodeRows('t) =
  (decode, rows) => Row.decodeJson(rows |> getFirst, decode);

let optColumn: (string, J.decoder('t)) => decodeRows(option('t)) =
  (col, dec) =>
    fun
    | [||] => None
    | rows => Some(Row.decodeJson(rows[0], J.field(col, dec)));

let column: (string, J.decoder('t)) => decodeRows('t) =
  (col, dec) => decodeFirst(J.field(col, dec));

// Given a row where one of the fields is an ID, a decoder for
// the ID and another decoder to get the rest of the object, decodes
// the row into the object.
let withId =
    (~idField: string, ~idDecode: J.decoder('id), decode: J.decoder('t)): J.decoder(('id, 't)) =>
  J.(tup2(field(idField, idDecode), decode));

let decodeEach: (J.decoder('a), array(Row.t(Js.Json.t))) => array('a) =
  (d, arr) => A.map(arr, row => Row.decodeJson(row, d));

// Given a way to get a (key, value) pair from a row, produce a
// dictionary with those keys/values.
let dict =
    (
      ~keyField: string,
      ~keyDecode: J.decoder(string)=J.string,
      ~valueField: string,
      ~valueDecode: J.decoder('a),
    )
    : decodeRows(Js.Dict.t('a)) =>
  jsonRows => {
    jsonRows
    |> decodeEach(J.(tup2(field(keyField, keyDecode), field(valueField, valueDecode))))
    |> Js.Dict.fromArray;
  };

let nestedDict =
    (
      ~outerKeyField: string,
      ~outerKeyDecode: J.decoder(string)=J.string,
      ~innerKeyField: string,
      ~innerKeyDecode: J.decoder(string)=J.string,
      ~valueField: string,
      ~valueDecode: J.decoder('a),
    )
    : decodeRows(Js.Dict.t(Js.Dict.t('a))) =>
  jsonRows =>
    jsonRows
    |> decodeEach(
         J.tup3(
           J.field(innerKeyField, innerKeyDecode),
           J.field(outerKeyField, outerKeyDecode),
           J.field(valueField, valueDecode),
         ),
       )
    |> (
      decoded => {
        let result = Js.Dict.empty();
        A.forEach(decoded, ((inner, outer, value)) =>
          switch (Js.Dict.get(result, outer)) {
          | None => Js.Dict.set(result, outer, Js.Dict.fromArray([|(inner, value)|]))
          | Some(values) => Js.Dict.set(values, inner, value)
          }
        );
        result;
      }
    );
