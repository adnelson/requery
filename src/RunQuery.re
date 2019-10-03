// Execute queries against a database, decoding the resulting rows.
module Result = BsPostgres.Result;
module Query = BsPostgres.Query;

let (query', connect) = BsPostgres.Client.Promise.(query', connect);

let (then_, resolve, catch) = Js.Promise.(then_, resolve, catch);

type rowDecode('t) = array(Js.Json.t) => 't;

let select =
    (~logQuery=false, ~logResult=false, client, selectQuery, decode: rowDecode('t))
    : Js.Promise.t('t) => {
  let rendered = RenderQuery.Select.render(selectQuery);
  if (logQuery) {
    Js.log(rendered);
  };
  query'(Query.make(~text=rendered, ()), client)
  |> then_((result: Result.t(Js.Json.t)) => {
       if (logResult) {
         Js.log(result);
       };
       resolve(decode(result##rows));
     });
};
