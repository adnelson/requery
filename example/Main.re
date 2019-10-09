module A = Utils.Array;
module L = Utils.List;
module J = Utils.Json;
module JD = Utils.Json.Decode;
module JE = Utils.Json.Encode;
module QR = AbstractDB.QueryResult;
let (then_, then2, resolve, catch, rLog, finally, all2, rLog2) =
  Utils.Promise.(then_, then2, resolve, catch, rLog, finally, all2, rLog2);
let exit = Utils_Process.exit;

module Seeds = (DB: AbstractDB.DBType, Rules: RenderQuery.SqlRenderingRules) => {
  module Query = AbstractDB.Query(DB, Rules);
  module Render = RenderQuery.WithRenderingRules(Rules);

  let customers = (client: DB.client): Js.Promise.t(unit) => {
    module C = Objects.Customer;
    let objects = [
      C.make("Alice", "alice@foo.com"),
      C.make("Bob", "bob@bar.com", ~phoneNumber="5552138787"),
      C.make("Clancy", "clanciepoo@blix.blox"),
    ];
    let insert =
      QueryBuilder.(
        C.table |> insertMany(C.Encode.toRow, objects) |> returningColumn(column("id"))
      );
    let decode = RowDecode.(decodeEach(J.(field("id", int))));
    let logQuery = Some(q => Js.log(Render.render(q)));

    Query.insert(~logQuery?, client, decode, insert)
    |> then_(ids => rLog(ids |> QR.encode(JE.(array(int)))));
  };
};

let main = () => {
  module DB = Postgres.DB;
  module Seeds = Seeds(DB, Postgres.Rules);

  let pool = DB.makePool({host: "localhost", database: "requery-example", port: 5999});

  DB.makeClient(pool)
  |> then_(client => Seeds.customers(client) |> finally(() => DB.releaseClient(client)))
  |> finally(() => DB.releasePool(pool));
};

let _ = main() |> then_(_ => exit()) |> catch(e => exit(~message=e, ()));
