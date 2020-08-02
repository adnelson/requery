open Utils.Abbreviations;
module Pg = BsPostgres;
module Rules = RenderQuery.DefaultRules;
module Render = RenderQuery.WithRenderingRules(Rules);
let (then_, then2, resolve, catch, rLog, finally, all2, rLog2) =
  PromiseUtils.(then_, then2, resolve, catch, rLog, finally, all2, rLog2);

// Connection config. TODO many more options to support
module Config = {
  type t = {
    host: string,
    database: string,
    port: int,
    user: option(string),
    password: option(string),
  };

  let toJson: JE.encoder(t) =
    ({host, database, port, user}) =>
      JE.(
        object_([
          ("host", host |> string),
          ("database", database |> string),
          ("port", port |> int),
          ("user", user |> nullable(string)),
          // err on the side of caution, not rendering password
        ])
      );

  let make = (~host, ~database, ~port, ~user=?, ~password=?, ()) => {
    host,
    database,
    port,
    user,
    password,
  };
};

// Postgres doesn't know about nested tables :)
type query = PostgresSyntax.pgQuery;

// Postgres results are wrapped in this type
type result = BsPostgres.Result.t(Js.Json.t);

// Type alias for the abstract client paramaterized for postgres
type client = Client.t(BsPostgres.Client.t, result, query);

let runRaw = (client, text) =>
  BsPostgres.Client.Promise.query'(BsPostgres.Query.make(~text, ()), client);

// Convert a node-postgres handle to a requery client
let fromPgClient = (~onQuery=?, ~onResult=?, client: BsPostgres.Client.t) =>
  Client.make(
    ~execRaw=runRaw,
    ~handle=client,
    ~onQuery?,
    ~onResult?,
    ~queryRaw=runRaw,
    ~queryToSql=PostgresRender.pgRender,
    ~resultToRows=(result: result) => RowDecode.toRows(result##rows),
    (),
  );

// Pooled connections
module Pool = {
  let makePool = ({Config.host, database, port, user, password}): BsPostgres.Pool.t =>
    BsPostgres.Pool.make(~host, ~database, ~port, ~user?, ~password?, ());

  let makeClient = (~onQuery=?, ~onResult=?, pool: BsPostgres.Pool.t) =>
    BsPostgres.Pool.Promise.connect(pool)->P.map(h => fromPgClient(~onQuery?, ~onResult?, h));

  let releaseClient = BsPostgres.Pool.Pool_Client.release;
  let releasePool = BsPostgres.Pool.Promise.end_;

  // Abstracts setup/teardown of a postgres connection pool.
  let runPool: (Config.t, BsPostgres.Pool.t => Js.Promise.t('a)) => Js.Promise.t('a) =
    (config, action) => {
      let pool = makePool(config);
      action(pool)->finally(() => releasePool(pool)->ignore);
    };

  // Create a client from a pool and then run an action with it. The
  // client is automatically released afterwards.
  // If you don't want to manage the setup/teardown of the pool, you can
  // use `runPoolClient`.
  let runClientInPool =
      (
        ~onQuery: option((client, query) => unit)=?,
        ~onResult: option((client, query, result) => unit)=?,
        pool: BsPostgres.Pool.t,
        action: client => Js.Promise.t('a),
      ) =>
    BsPostgres.Pool.Promise.connect(pool)
    ->P.map(client =>
        Client.make(
          ~execRaw=runRaw,
          ~handle=client,
          ~onQuery?,
          ~onResult?,
          ~queryRaw=runRaw,
          ~queryToSql=PostgresRender.pgRender,
          ~resultToRows=
            (result: BsPostgres.Result.t(Js.Json.t)) => RowDecode.toRows(result##rows),
          (),
        )
      )
    ->P.flatMap(client
        // Wrap in an extra promise to ensure that releaseClient completes
        // before the promise resolves.
        =>
          P.make((~resolve, ~reject as _) => {
            let unit_ = ();
            action(client)
            ->finally(() =>
                releaseClient(Client.handle(client))->P.map(_ => resolve(. unit_))->ignore
              )
            ->ignore;
          })
        );

  // Abstracts setup/teardown of both a connection pool, and a client within
  // that pool.
  let runPoolClient = (~onQuery=?, ~onResult=?, config, action: client => Js.Promise.t('a)) =>
    runPool(config, pool => runClientInPool(~onQuery?, ~onResult?, pool, action));
};
