open RequeryAbstract;
module A = Utils.Array;
module QB = QueryBuilder;
module RE = RowEncode;
module Rules = RenderQuery.DefaultRules;
module Render = RenderQuery.WithRenderingRules(Rules);

module S = {
  type conn = Sqlite.Connection.t;
  type stmt = Sqlite.Statement.t;
  let (make, prepare) = Sqlite.Connection.(make, prepare);
  let (get, all, run) = Sqlite.Statement.(get, all, run);
};

// Arguments when using the filesystem
type fileargs = {
  path: string,
  readonly: bool,
  fileMustExist: bool,
};

// Use either the filesystem or an in-memory database
type args =
  | Memory
  | MemoryNamed(string)
  | File(fileargs);

let connect: args => S.conn =
  Sqlite.Connection.(
    fun
    | Memory => make(~path="memory", ~memory=true, ())
    | MemoryNamed(name) => make(~path=name, ~memory=true, ())
    | File({path, readonly, fileMustExist}) =>
      make(~path, ~memory=false, ~fileMustExist, ~readonly, ())
  );

// The sqlite library returns back a JSON array.
type client = Client.t(Sqlite.Connection.t, array(Js.Json.t), Sql.query);

let makeClient = (~onQuery=?, ~onResult=?, args) =>
  Client.make(
    ~onQuery?,
    ~onResult?,
    ~handle=connect(args),
    ~queryToSql=Render.render,
    ~queryRaw=
      (conn, raw) => {
        let stmt = S.prepare(conn, raw);
        Js.Promise.resolve(S.all(stmt, [||]));
      },
    ~execRaw=
      (conn, raw) => {
        let stmt = S.prepare(conn, raw);
        S.run(stmt, [||]) |> A.singleton |> Js.Promise.resolve;
      },
    //
    ~resultToRows=RowDecode.toRows,
    (),
  );
