include JsonUtils.Decode;
module A = ArrayUtils;
module D = DictUtils;
module O = OptionUtils;
module S = StringUtils;
module ColumnName = Sql.ColumnName;
type dict('a) = D.t('a);

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

  let decodeJson: (fromJson('a), t(Js.Json.t)) => 'a =
    (decode, row) =>
      try(mapGet(row, decode)) {
      | DecodeError(err) => raise(Error(RowDecodeError(row.index, row.contents, err)))
      };
};

// Something which can translate rows of Json objects.
type fromRows('t) = array(Row.t(Js.Json.t)) => 't;

// A do-nothing decoder, used for queries which don't return information.
let unit: fromRows(unit) = _ => ();

let errorToJson =
  JsonUtils.Encode.(
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

// Apply two rows decoders to the same rows to parse multiple things.
let two: (fromRows('a), fromRows('b)) => fromRows(('a, 'b)) =
  (rd1, rd2, rows) => {
    let res1 = rows |> rd1;
    let res2 = rows |> rd2;
    (res1, res2);
  };

// Apply three rows decoders to the same rows to parse multiple things.
let three: (fromRows('a), fromRows('b), fromRows('c)) => fromRows(('a, 'b, 'c)) =
  (rd1, rd2, rd3, rows) => {
    let res1 = rows |> rd1;
    let res2 = rows |> rd2;
    let res3 = rows |> rd3;
    (res1, res2, res3);
  };

let getFirst: array(Row.t('a)) => Row.t('a) =
  fun
  | [||] => raise(Error(EmptyRows))
  | rows => rows[0];

// Decode the first row with the given JSON decoder.
let decodeOne: fromJson('t) => fromRows('t) =
  (decode, rows) => rows |> getFirst |> Row.decodeJson(decode);

// Map a JSON decoder over the rows, collecting the result for each row.
let decodeEach: (fromJson('a), array(Row.t(Js.Json.t))) => array('a) =
  (d, arr) => A.map(arr, Row.decodeJson(d));

// A decoder which just returns the json rows.
let jsonRows: fromRows(array(Js.Json.t)) = decodeEach(j => j);

// Decode each row and reduce the result to some value.
let decodeReduce: (fromJson('a), 'b, ('a, 'b) => 'b) => fromRows('b) =
  (dec, start, f, rows) => A.reduce(decodeEach(dec, rows), start, f);

let optColumn: (ColumnName.t, fromJson('t)) => fromRows(option('t)) =
  (col, dec, rows) =>
    O.map(A.head(rows), Row.decodeJson(field(col->ColumnName.toString, dec)));

// Get one column, with the given name and with the given decoder.
// Uses `Json.Decode.field` under the hood
let column1: (ColumnName.t, fromJson('t)) => fromJson('t) =
  (col, fj) => field(col->ColumnName.toString, fj);

let columns2: (ColumnName.t, fromJson('a), ColumnName.t, fromJson('b)) => fromJson(('a, 'b)) =
  (columnA, decodeA, columnB, decodeB, j) => (
    j |> column1(columnA, decodeA),
    j |> column1(columnB, decodeB),
  );

let columns3:
  (ColumnName.t, fromJson('a), ColumnName.t, fromJson('b), ColumnName.t, fromJson('c)) =>
  fromJson(('a, 'b, 'c)) =
  (columnA, decodeA, columnB, decodeB, columnC, decodeC, j) => (
    j |> column1(columnA, decodeA),
    j |> column1(columnB, decodeB),
    j |> column1(columnC, decodeC),
  );

// Given a row where one of the fields is an ID, a decoder for
// the ID and another decoder to get the rest of the object, decodes
// the row into the object.
let withId =
    (~idField: string, ~idDecode: fromJson('id), decode: fromJson('t)): fromJson(('id, 't)) =>
  tup2(field(idField, idDecode), decode);

// Decode a row into a 3-tuple.
let tuple3Row:
  (string, fromJson('a), string, fromJson('b), string, fromJson('c)) => fromJson(('a, 'b, 'c)) =
  (columnA, decodeA, columnB, decodeB, columnC, decodeC, j) => (
    j |> field(columnA, decodeA),
    j |> field(columnB, decodeB),
    j |> field(columnC, decodeC),
  );

// Given a way to get a (key, value) pair from a row, produce a
// dictionary with those keys/values.
let dict =
    (
      ~keyField: string,
      ~keyDecode: fromJson(string)=string,
      ~valueField: string,
      ~valueDecode: fromJson('a),
      (),
    )
    : fromRows(D.t('a)) =>
  jsonRows => {
    jsonRows
    |> decodeEach(tup2(field(keyField, keyDecode), field(valueField, valueDecode)))
    |> D.fromArray;
  };

let dict2d =
    (
      ~outerKeyField: string,
      ~outerKeyDecode: fromJson(string)=string,
      ~innerKeyField: string,
      ~innerKeyDecode: fromJson(string)=string,
      ~valueField: string,
      ~valueDecode: fromJson('a),
      (),
    )
    : fromRows(D.t(D.t('a))) =>
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
        let result = D.empty();
        A.forEach(decoded, ((inner, outer, value)) =>
          switch (D.get(result, outer)) {
          | None => D.set(result, outer, D.fromArray([|(inner, value)|]))
          | Some(values) => D.set(values, inner, value)
          }
        );
        result;
      }
    );

let dict3d =
    (
      ~keyField1: string,
      ~keyDecode1: fromJson(string)=string,
      ~keyField2: string,
      ~keyDecode2: fromJson(string)=string,
      ~keyField3: string,
      ~keyDecode3: fromJson(string)=string,
      ~valueField: string,
      ~valueDecode: fromJson('a),
      (),
    )
    : fromRows(dict(dict(dict('a)))) =>
  jsonRows =>
    jsonRows
    |> decodeEach(
         tup4(
           field(keyField1, keyDecode1),
           field(keyField2, keyDecode2),
           field(keyField3, keyDecode3),
           field(valueField, valueDecode),
         ),
       )
    |> (
      decoded => {
        let result: dict(dict(dict('a))) = D.empty();
        A.forEach(decoded, ((x, y, z, value)) =>
          switch (D.get(result, x)) {
          | None => D.set(result, x, D.singleton(y, D.singleton(z, value)))
          | Some(xValues) =>
            switch (D.get(xValues, y)) {
            | None => D.set(xValues, y, D.singleton(z, value))
            | Some(yValues) => D.set(yValues, z, value)
            }
          }
        );
        result;
      }
    );

// TODO there might be a way to DRY this up
let dict4d =
    (
      ~keyField1: string,
      ~keyDecode1: fromJson(string)=string,
      ~keyField2: string,
      ~keyDecode2: fromJson(string)=string,
      ~keyField3: string,
      ~keyDecode3: fromJson(string)=string,
      ~keyField4: string,
      ~keyDecode4: fromJson(string)=string,
      ~valueField: string,
      ~valueDecode: fromJson('a),
      (),
    )
    : fromRows(dict(dict(dict(dict('a))))) =>
  jsonRows =>
    jsonRows
    |> decodeEach(
         tup5(
           field(keyField1, keyDecode1),
           field(keyField2, keyDecode2),
           field(keyField3, keyDecode3),
           field(keyField4, keyDecode4),
           field(valueField, valueDecode),
         ),
       )
    |> (
      decoded => {
        let result: dict(dict(dict(dict('a)))) = D.empty();
        A.forEach(decoded, ((k1, k2, k3, k4, value)) =>
          switch (D.get(result, k1)) {
          | None => D.set(result, k1, D.singleton(k2, D.singleton(k3, D.singleton(k4, value))))
          | Some(values1) =>
            switch (D.get(values1, k2)) {
            | None => D.set(values1, k2, D.singleton(k3, D.singleton(k4, value)))
            | Some(values2) =>
              switch (D.get(values2, k3)) {
              | None => D.set(values2, k3, D.singleton(k4, value))
              | Some(values3) => D.set(values3, k4, value)
              }
            }
          }
        );
        result;
      }
    );

// Given a way to get a (key, value) pair from a row, produce a dictionary
// with those keys/values and an array of keys in the order encountered.
let dictWithOrder =
    (
      ~keyField: string,
      ~keyDecode: fromJson(string)=string,
      ~valueField: string,
      ~valueDecode: fromJson('a),
      (),
    )
    : fromRows((D.t('a), array(string))) =>
  jsonRows => {
    jsonRows
    |> decodeEach(tup2(field(keyField, keyDecode), field(valueField, valueDecode)))
    |> (entries => (D.fromArray(entries), S.dedupeArray(A.map(entries, fst))));
  };

// Aggregate all rows by a particular field, and apply the inner
// decoder to each resulting row array, returning a dictionary.
// This can be nested, although it's not particularly efficient
// since each nested call will need to iterate over all rows.
let dictOf =
    (~keyField: string, ~keyDecode: fromJson(string)=string, inner: fromRows('a))
    : fromRows(D.t('a)) =>
  rows => {
    let agg = D.empty();
    A.forEach(
      rows,
      row => {
        let key = row |> Row.decodeJson(field(keyField, keyDecode));
        switch (D.get(agg, key)) {
        | None => D.set(agg, key, [|row|]) |> ignore
        | Some(rows') => A.pushMut(rows', row)
        };
      },
    );
    D.map(agg, inner);
  };

// Similar to dictOf, but returns ordered key/value pairs. The restriction is that
// the key must be able to be converted to a string (to achieve the aggregation)
let tuples =
    (keyField: string, keyDecode: fromJson('k), keyToString: 'k => string, inner: fromRows('v))
    : fromRows(array(('k, 'v))) =>
  rows => {
    let agg = D.empty();
    let keys = [||];
    A.forEach(
      rows,
      row => {
        let key = row |> Row.decodeJson(field(keyField, keyDecode));
        let keyString = key |> keyToString;
        switch (D.get(agg, keyString)) {
        | None =>
          D.set(agg, keyString, [|row|]) |> ignore;
          A.pushMut(keys, key);
        | Some(rows') => A.pushMut(rows', row)
        };
      },
    );
    // Get the values from the dictionary and apply the inner decoder
    A.map(keys, k => (k, D.getExn(agg, k |> keyToString) |> inner));
  };
