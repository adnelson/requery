module DB: AbstractDB.DBType = {
  module P = BsPostgres;
  type result = P.Result.t(Js.Json.t);
  type pool = P.Pool.t;
  type client = P.Client.t;
  let makePool = ({AbstractDB.host, database, port}) =>
    P.Pool.make(~host, ~database, ~port, ());
  let makeClient = P.Pool.Promise.connect;
  let releaseClient = P.Pool.Pool_Client.release;
  let releasePool = P.Pool.Promise.end_;
  let query = (client, q) =>
    P.Client.Promise.query'(P.Query.make(~text=RenderQuery.render(q), ()), client);
  let resultToRows = (result: result) => RowDecode.toRows(result##rows);
};

module Query = AbstractDB.Query(DB);

let select = Query.select;
let retrieve = Query.retrieve;
