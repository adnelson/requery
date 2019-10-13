include Utils.Json.Decode;
module A = Utils.Array;

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

  let decodeJson: (t(Js.Json.t), decoder('a)) => 'a =
    (row, decode) =>
      try (mapGet(row, decode)) {
      | DecodeError(err) => raise(Error(RowDecodeError(row.index, row.contents, err)))
      };
};

// Something which can translate rows of Json objects.
type rowsDecoder('t) = array(Row.t(Js.Json.t)) => 't;

// A do-nothing decoder, used for queries which don't return information.
let unit: rowsDecoder(unit) = _ => ();

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

let decodeFirst: decoder('t) => rowsDecoder('t) =
  (decode, rows) => Row.decodeJson(rows |> getFirst, decode);

let decodeOne: decoder('t) => rowsDecoder('t) = decodeFirst;

let optColumn: (string, decoder('t)) => rowsDecoder(option('t)) =
  (col, dec) =>
    fun
    | [||] => None
    | rows => Some(Row.decodeJson(rows[0], field(col, dec)));

let column: (string, decoder('t)) => rowsDecoder('t) =
  (col, dec) => decodeFirst(field(col, dec));

let columns2: (string, decoder('a), string, decoder('b)) => decoder(('a, 'b)) =
  (columnA, decodeA, columnB, decodeB, j) => (
    j |> field(columnA, decodeA),
    j |> field(columnB, decodeB),
  );

let columns3:
  (string, decoder('a), string, decoder('b), string, decoder('c)) => decoder(('a, 'b, 'c)) =
  (columnA, decodeA, columnB, decodeB, columnC, decodeC, j) => (
    j |> field(columnA, decodeA),
    j |> field(columnB, decodeB),
    j |> field(columnC, decodeC),
  );

// Given a row where one of the fields is an ID, a decoder for
// the ID and another decoder to get the rest of the object, decodes
// the row into the object.
let withId =
    (~idField: string, ~idDecode: decoder('id), decode: decoder('t)): decoder(('id, 't)) =>
  tup2(field(idField, idDecode), decode);

// Map a JSON decoder over the rows, collecting the result for each row.
let decodeEach: (decoder('a), array(Row.t(Js.Json.t))) => array('a) =
  (d, arr) => A.map(arr, row => Row.decodeJson(row, d));

// Decode a row into a 3-tuple.
let tuple3Row:
  (string, decoder('a), string, decoder('b), string, decoder('c)) => decoder(('a, 'b, 'c)) =
  (columnA, decodeA, columnB, decodeB, columnC, decodeC, j) => (
    j |> field(columnA, decodeA),
    j |> field(columnB, decodeB),
    j |> field(columnC, decodeC),
  );

/*
 let decodeResult: (rowsDecoder('a), array(Row.t(Js.Json.t))) => Result.t('a) =
   (decode, rows) =>
     try (Result.Success(decode(rows))) {
     | RowDecode.Error(e) => Result.Error(RowDecodeError(e))
     };

 let decodeResultPromise:
   (rowsDecoder('a), array(Row.t(Js.Json.t))) => Js.Promise.t(Result.t('a)) =
   (decode, rows) => rows |> decodeResult(decode) |> resolve;
 */
// Given a way to get a (key, value) pair from a row, produce a
// dictionary with those keys/values.
let dict =
    (
      ~keyField: string,
      ~keyDecode: decoder(string)=string,
      ~valueField: string,
      ~valueDecode: decoder('a),
    )
    : rowsDecoder(Js.Dict.t('a)) =>
  jsonRows => {
    jsonRows
    |> decodeEach(tup2(field(keyField, keyDecode), field(valueField, valueDecode)))
    |> Js.Dict.fromArray;
  };

let nestedDict =
    (
      ~outerKeyField: string,
      ~outerKeyDecode: decoder(string)=string,
      ~innerKeyField: string,
      ~innerKeyDecode: decoder(string)=string,
      ~valueField: string,
      ~valueDecode: decoder('a),
    )
    : rowsDecoder(Js.Dict.t(Js.Dict.t('a))) =>
  jsonRows =>
    jsonRows
    |> decodeEach(
         tup3(
           field(innerKeyField, innerKeyDecode),
           field(outerKeyField, outerKeyDecode),
           field(valueField, valueDecode),
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
