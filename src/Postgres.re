module Pg = BsPostgres;
module Rules = RenderQuery.DefaultRules;
module Render = RenderQuery.WithRenderingRules(Rules);
module AClient = AbstractClient.DBClient;
let (then_, then2, resolve, catch, rLog, finally, all2, rLog2) =
  Utils.Promise.(then_, then2, resolve, catch, rLog, finally, all2, rLog2);

// The client for postgres
type result = Pg.Result.t(Js.Json.t);
type client = AClient.t(Pg.Client.t, result, result);
module Pool = {
  type pool = Pg.Pool.t;
  type result = Pg.Result.t(Js.Json.t);
  let makePool = ({AbstractDB.host, database, port}) =>
    Pg.Pool.make(~host, ~database, ~port, ());
  let runRaw = (client, text) => Pg.Client.Promise.query'(Pg.Query.make(~text, ()), client);

  let makeClient: pool => Js.Promise.t(client) =
    pool =>
      Pg.Pool.Promise.connect(pool)
      |> then_(client =>
           AClient.make(
             ~handle=client,
             ~queryToSql=Render.render,
             ~queryRaw=runRaw,
             ~execRaw=runRaw,
             ~resultToRows=(result: result) => RowDecode.toRows(result##rows),
             (),
           )
           |> resolve
         );

  let releaseClient = Pg.Pool.Pool_Client.release;
  let releasePool = Pg.Pool.Promise.end_;
  let runClient: (pool, client => Js.Promise.t('a)) => Js.Promise.t('a) =
    (pool, action) =>
      makeClient(pool)
      |> then_(client => action(client) |> finally(() => releaseClient(AClient.handle(client))));
};
