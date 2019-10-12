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

module AClient = AbstractClient.DBClient;
let makeClient = args =>
  AClient.make(
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

/*
 let insert = (conn, insert) => {
   let sql = Render.insert(insert);
   Js.log(sql);
   let stmt = S.prepare(conn, sql);
   let _ = S.run(stmt, [||]);
   ();
 };

 let select = (conn, select) => {
   Js.log(Render.select(select));
   let stmt = S.prepare(conn, Render.select(select));
   S.all(stmt, [||]);
 };
 let execRaw = (conn, raw) => {
   let stmt = S.prepare(conn, raw);
   S.run(stmt, [||]);
 };

 let queryRaw = (conn, raw) => {
   let stmt = S.prepare(conn, raw);
   S.all(stmt, [||]);
 }

 */
/*
 module DB: AbstractDB.DBType = {
   type result = Js.Json.t;
   type pool = P.Pool.t;
   type client = P.Client.t;
   let makePool = ({AbstractDB.host, database, port}) =>
     P.Pool.make(~host, ~database, ~port, ());
   let makeClient = P.Pool.Promise.connect;
   let releaseClient = P.Pool.Pool_Client.release;
   let releasePool = P.Pool.Promise.end_;
   let query = (client, q) =>
     P.Client.Promise.query'(P.Query.make(~text=Render.render(q), ()), client);
   let resultToRows = (result: result) => RowDecode.toRows(result##rows);
 };
 */
