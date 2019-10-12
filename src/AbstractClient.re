module O = Utils.Option;
let (resolve, then_, reject) = Js.Promise.(resolve, then_, reject);
//module QB = QueryBuilder;

module DBClient = {
  type rows = array(RowDecode.Row.t(Js.Json.t));
  type t('handle, 'result, 'empty) = {
    // Handle to underlying client object.
    handle: 'handle,
    // Render a query into SQL.
    queryToSql: Sql.query => string,
    // Run a raw query that returns rows.
    queryRaw: ('handle, string) => Js.Promise.t('result),
    // Run a raw query that doesn't return information.
    execRaw: ('handle, string) => Js.Promise.t('empty),
    // Translate a result into an array of rows.
    resultToRows: 'result => rows,
    // Function to run on the query right before it's executed.
    onQuery: option(('handle, Sql.query) => unit),
    // Function to run after the result is received.
    onResult: option(('handle, Sql.query, 'result) => unit),
  };

  // Create a client.
  let make =
      (~handle, ~queryToSql, ~resultToRows, ~queryRaw, ~execRaw, ~onQuery=?, ~onResult=?, ()) => {
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

  let query: (t('h, 'r, 'e), Sql.query) => Js.Promise.t(rows) =
    ({onQuery, onResult, handle, queryToSql, queryRaw, resultToRows}, query) => {
      let _ = O.map(onQuery, f => f(handle, query));
      queryRaw(handle, queryToSql(query))
      |> then_(result => {
           let _ = O.map(onResult, f => f(handle, query, result));
           result |> resultToRows |> resolve;
         });
    };

  let exec: (t('h, 'r, 'e), Sql.query) => Js.Promise.t('e) =
    ({onQuery, handle, queryToSql, execRaw}, query) => {
      let _ = O.map(onQuery, f => f(handle, query));
      execRaw(handle, queryToSql(query));
    };

  let insert = (cli, insert) => exec(cli, Sql.Insert(insert));
  let decodeResult:
    (RowDecode.rowsDecoder('a), array(RowDecode.Row.t(Js.Json.t))) => Result.t('a) =
    (decode, rows) =>
      try (Result.Success(decode(rows))) {
      | RowDecode.Error(e) => Result.Error(RowDecodeError(e))
      };
  let decodeResultPromise:
    (RowDecode.rowsDecoder('a), array(RowDecode.Row.t(Js.Json.t))) =>
    Js.Promise.t(Result.t('a)) =
    (decode, rows) => rows |> decodeResult(decode) |> resolve;

  let drp = decodeResult;
  let insertReturn = (cli, decode, insert) =>
    query(cli, Sql.Insert(insert)) |> then_(decodeResultPromise(decode));
  let select = (cli, decode, select) =>
    query(cli, Sql.Select(select)) |> then_(decodeResultPromise(decode));
  let execRaw = ({handle, execRaw}, sql) => execRaw(handle, sql);
};
