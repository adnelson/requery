open RequeryAbstract;
module A = Utils.Array;
module QB = QueryBuilder;
module RE = RowEncode;
module Rules = RenderQuery.DefaultRules;
module Render = RenderQuery.WithRenderingRules(Rules);
module AClient = AbstractClient;

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

type client = AClient.t(Sqlite.Connection.t, AClient.rows);

let makeClient = (~onQuery=?, ~onResult=?, args) =>
  AClient.make(
    ~onQuery?,
    ~onResult?,
    ~handle=connect(args),
    ~queryToSql=Render.render,
    ~queryRaw=
      (conn, raw) => {
        let stmt = S.prepare(conn, raw);
        S.all(stmt, [||]) |> RowDecode.toRows |> Js.Promise.resolve;
      },
    ~execRaw=
      (conn, raw) => {
        let stmt = S.prepare(conn, raw);
        S.run(stmt, [||]) |> A.singleton |> RowDecode.toRows |> Js.Promise.resolve;
      },
    ~resultToRows=Utils.id,
    (),
  );
