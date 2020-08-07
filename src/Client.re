module O = OptionUtils;
module R = ResultUtils;
module P = PromiseUtils;
let (resolve, then_, reject) = Js.Promise.(resolve, then_, reject);

module QueryResult = {
  // Various errors that can occur. TODO add a connection error type
  type error =
    | RowDecodeError(RowDecode.error);

  let errorToExn: error => exn =
    fun
    | RowDecodeError(e) => RowDecode.Error(e);

  let errorToJson: JsonUtils.toJson(error) =
    JsonUtils.Encode.(
      fun
      | RowDecodeError(e) => e |> object1("RowDecodeError", RowDecode.errorToJson)
    );

  exception Error(error);

  type t('a) = R.t('a, error);

  let ok: 'a => t('a) = res => R.Ok(res);
  let error: error => t('a) = err => R.Error(err);

  module Enc = JsonUtils.Encode;

  let encode: JsonUtils.toJson('a) => JsonUtils.toJson(t('a)) =
    Enc.(
      encodeSuccess =>
        fun
        | Error(e) => e |> object1("Error", errorToJson)
        | R.Ok(x) => x |> object1("Success", encodeSuccess)
    );

  let unwrap: t('a) => 'a = r => r |> R.mapError(errorToExn) |> R.unwrap;
  let unwrapPromise: t('a) => Js.Promise.t('a) =
    r => r |> R.mapError(errorToExn) |> R.unwrapPromise;
};

type rows = array(RowDecode.Row.t(Js.Json.t));

// The most general client, parameterized by:
// 'handle: DB abstraction (e.g. a Sqlite3 connection),
// 'result: the type of objects the database returns on success. These
//    will be converted to the `rows` type above, via the `resultToRows` function.
// 'query: The specific SQL query type being used, rendered into a raw
//    SQL via the `queryToSql` function.
type t('handle, 'result, 'query) = {
  // Handle to the object that allows us to communicate with the database.
  handle: 'handle,
  // Render a query into SQL.
  queryToSql: 'query => string,
  // Run a raw query that returns rows.
  queryRaw: ('handle, string) => Js.Promise.t('result),
  // Run a raw query that doesn't return information.
  execRaw: ('handle, string) => Js.Promise.t('result),
  // Translate a result into an array of rows.
  resultToRows: 'result => rows,
  // Function to run on the query before it's executed.
  onQuery: option((t('handle, 'result, 'query), 'query) => unit),
  // Function to run after the result is received.
  onResult: option((t('handle, 'result, 'query), option('query), 'result) => unit),
};

// Can be passed to a `onQuery` argument, given some string conversion. Invokes
// `f` on each query before it's executed.
let logQueryWith: (string => unit, t('h, 'r, 'q), 'q) => unit =
  (f, {queryToSql}, q) => f(queryToSql(q) ++ ";");

// Logs a query to stdout via Js.log.
// TODO add a ~color parameter to this?
let logQuery: (t('h, 'r, 'q), 'q) => unit = (c, q) => logQueryWith(Js.log, c, q);

// Create a client.
let make = (~handle, ~queryToSql, ~resultToRows, ~queryRaw, ~execRaw, ~onQuery=?, ~onResult=?, ()) => {
  handle,
  queryToSql,
  queryRaw,
  resultToRows,
  execRaw,
  onQuery,
  onResult,
};

let renderQuery = ({queryToSql}, query) => queryToSql(query);
let handle = ({handle}) => handle;

let query: (t('h, 'r, 'q), 'q) => Js.Promise.t(rows) =
  ({onQuery, onResult, handle, queryToSql, queryRaw, resultToRows} as client, query) => {
    let _ = O.map(onQuery, f => f(client, query));
    queryRaw(handle, queryToSql(query))
    |> then_(result => {
         let _ = O.map(onResult, f => f(client, Some(query), result));
         result |> resultToRows |> resolve;
       });
  };

let exec: (t('h, 'r, 'q), 'q) => Js.Promise.t(rows) =
  ({onQuery, onResult, handle, queryToSql, execRaw, resultToRows} as client, query) => {
    let _ = O.map(onQuery, f => f(client, query));
    execRaw(handle, queryToSql(query))
    |> then_(result => {
         let _ = O.map(onResult, f => f(client, Some(query), result));
         result |> resultToRows |> resolve;
       });
  };

let decodeResult: (RowDecode.fromRows('a), rows) => QueryResult.t('a) =
  (fromRows, rows) =>
    try(Belt.Result.Ok(fromRows(rows))) {
    | RowDecode.Error(e) => Belt.Result.Error(RowDecodeError(e))
    };

////////////////////////////////////////////////////////////////////////////////
///// Selects

let select =
    (cli: t(_), fromRows: RowDecode.fromRows('a), select): Js.Promise.t(QueryResult.t('a)) =>
  query(cli, Sql.Select(select))->P.map(decodeResult(fromRows));

let selectUnwrap = (cli, fromRows, select_) =>
  select(cli, fromRows, select_) |> then_(QueryResult.unwrapPromise);

////////////////////////////////////////////////////////////////////////////////
///// Inserts

let insert:
  'h 'r 'q.
  (
    t('h, 'r, Sql.query('returning, 'onConflict, _, _)),
    Sql.Insert.t('returning, 'onConflict)
  ) =>
  Js.Promise.t(_)
 =
  (cli, insert) => exec(cli, Sql.Insert(insert));

let insertReturn = (cli, fromRows, insert) =>
  query(cli, Sql.Insert(insert))->P.map(decodeResult(fromRows));

let insertReturnUnwrap = (cli, fromRows, insert) =>
  insertReturn(cli, fromRows, insert) |> then_(QueryResult.unwrapPromise);

////////////////////////////////////////////////////////////////////////////////
///// Creation

let createTable = (cli, ct) => exec(cli, Sql.CreateTable(ct));

let createView = (cli, cv) => exec(cli, Sql.CreateView(cv));

////////////////////////////////////////////////////////////////////////////////
///// Raw SQL

// Execute a query and throw away the result
let execRaw: 'h 'r 'q. (t('h, 'r, 'q), string) => P.t(unit) =
  ({handle, execRaw, onResult} as client, sql) =>
    execRaw(handle, sql)
    ->P.map(res => {
        onResult->O.forEach(f => f(client, None, res));
        ();
      });

// Execute a query with raw SQL and convert the result to rows
let queryRawRows: 'h 'r 'q. (t('h, 'r, 'q), string) => P.t(rows) =
  ({handle, queryRaw, onResult, resultToRows} as client, rawSql) =>
    queryRaw(handle, rawSql)
    ->P.map(res => {
        onResult->O.forEach(f => f(client, None, res));
        res->resultToRows;
      });

// Execute a query with raw SQL and parse the resulting rows.
let queryRawDecode:
  'h 'r 'q 'a.
  (t('h, 'r, 'q), RowDecode.fromRows('a), string) => P.t(QueryResult.t('a))
 =
  (client, fromRows, rawSql) => client->queryRawRows(rawSql)->P.map(decodeResult(fromRows));

// Execute a file containing SQL.
let execFile: 'h 'r 'q. (t('h, 'r, 'q), ~path: string) => P.t(unit) =
  (client, ~path) => P.(readFile(path)->flatMap(contents => client->execRaw(contents)));
