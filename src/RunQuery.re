// Execute queries against a database, decoding the resulting rows.
module Result = BsPostgres.Result;
module Query = BsPostgres.Query;

let (query', connect) = BsPostgres.Client.Promise.(query', connect);

type promise('a) = Js.Promise.t('a);
let (then_, resolve, catch) = Js.Promise.(then_, resolve, catch);

type rowDecode('t) = array(Js.Json.t) => 't;

let select = (~log=false, client, selectQuery, decode: rowDecode('t)): promise('t) => {
  let rendered = RenderQuery.Select.render(selectQuery);
  if (log) Js.log(rendered);
  query'(Query.make(~text=rendered, ()), client)
  |> then_((result: Result.t(Js.Json.t)) => resolve(decode(result##rows)));
};
