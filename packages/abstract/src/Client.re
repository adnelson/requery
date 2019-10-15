module O = Utils.Option;
let (resolve, then_, reject) = Js.Promise.(resolve, then_, reject);
//module QB = QueryBuilder;

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
  onResult: option((t('handle, 'result, 'query), 'query, 'result) => unit),
};

// Can be passed to a `onQuery` argument, logs each query before it's made.
let logQuery = ({queryToSql}, q) => Js.log(queryToSql(q) ++ ";");

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
         let _ = O.map(onResult, f => f(client, query, result));
         result |> resultToRows |> resolve;
       });
  };

let exec: (t('h, 'r, 'q), 'q) => Js.Promise.t(rows) =
  ({onQuery, onResult, handle, queryToSql, execRaw, resultToRows} as client, query) => {
    let _ = O.map(onQuery, f => f(client, query));
    execRaw(handle, queryToSql(query))
    |> then_(result => {
         let _ = O.map(onResult, f => f(client, query, result));
         result |> resultToRows |> resolve;
       });
  };

let insert = (cli, insert) => exec(cli, Sql.Insert(insert));
let decodeResult:
  (RowDecode.rowsDecoder('a), array(RowDecode.Row.t(Js.Json.t))) => Result.t('a) =
  (decode, rows) =>
    try (Result.Success(decode(rows))) {
    | RowDecode.Error(e) => Result.Error(RowDecodeError(e))
    };
let decodeResultPromise:
  (RowDecode.rowsDecoder('a), array(RowDecode.Row.t(Js.Json.t))) => Js.Promise.t(Result.t('a)) =
  (decode, rows) => rows |> decodeResult(decode) |> resolve;

let drp = decodeResult;
let insertReturn = (cli, decode, insert) =>
  query(cli, Sql.Insert(insert)) |> then_(decodeResultPromise(decode));
let insertReturnUnwrap = (cli, decode, insert) =>
  insertReturn(cli, decode, insert) |> then_(Result.unwrapPromise);
let select = (cli, decode, select) =>
  query(cli, Sql.Select(select)) |> then_(decodeResultPromise(decode));
let selectUnwrap = (cli, decode, select_) =>
  select(cli, decode, select_) |> then_(Result.unwrapPromise);

let createTable = (cli, ct) => exec(cli, Sql.CreateTable(ct));
let createView = (cli, cv) => exec(cli, Sql.CreateView(cv));

let execRaw = ({handle, execRaw}, sql) => execRaw(handle, sql);
