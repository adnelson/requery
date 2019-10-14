// Tools for encoding into rows
include QueryBuilder;
module O = Belt.Option;

let convertRow = (toC, toE, (k, v)) => (toC(k), toE(v));
let convertColumn = (toC, toE, (k, vs)) => (toC(k), (L.amap(vs), toE));
let stringRowWith = (toExpr, row) => L.map(row, convertRow(column, toExpr));
let stringRow = stringRowWith(Utils.id);
let rowFromFields = (fields, obj) =>
  stringRow(L.map(fields, ((field, fn)) => (field, fn(obj))));

// Make a row from a 2-tuple.
let columns2: (string, toExpr('a), string, toExpr('b)) => toRow(('a, 'b)) =
  (columnA, encodeA, columnB, encodeB, (a, b)) =>
    stringRow([(columnA, a |> encodeA), (columnB, b |> encodeB)]);

// Make a row from a 3-tuple.
let columns3:
  (string, toExpr('a), string, toExpr('b), string, toExpr('c)) => toRow(('a, 'b, 'c)) =
  (columnA, encodeA, columnB, encodeB, columnC, encodeC, (a, b, c)) =>
    stringRow([(columnA, a |> encodeA), (columnB, b |> encodeB), (columnC, c |> encodeC)]);

let nullable = (toExpr, opt) => O.mapWithDefault(opt, null, toExpr);
