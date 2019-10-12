module QB = QueryBuilder;
module RE = RowEncode;
module Rules = RenderQuery.DefaultRules;
module Render = RenderQuery.WithRenderingRules(Rules);
module Client = AbstractClient;
module P = Utils.Promise;
module JD = Utils.Json.Decode;
let (then_, then2, resolve, catch, rLog, finally, all2, rLog2) =
  Utils.Promise.(then_, then2, resolve, catch, rLog, finally, all2, rLog2);

module Author = {
  let table = QB.tbl("author");
  let table_ = table;
  type t('id) = {
    id: 'id,
    first: string,
    last: string,
  };
  let make = (first, last) => {id: (), first, last};
  let toRow = ({first, last}) =>
    RE.(stringRow([("first", first |> string), ("last", last |> string)]));
  let fromJson = j =>
    JD.{
      id: j |> field("id", int),
      first: j |> field("first", string),
      last: j |> field("last", string),
    };
};

let run = (client, createTables) => {
  Client.execRaw(client, createTables)
  |> then_(_
       // Inserting with an explicit query, using columns2 to define the encoding on the fly
       =>
         Client.insert(
           client,
           QB.(
             [("Stephen", "King"), ("Jane", "Austen")]
             |> insertMany(RE.columns2("first", string, "last", string))
             |> into(tbl("author"))
           ),
         )
         |> then_(_
              // Inserting using the Author-specific functions
              =>
                Client.insert(
                  client,
                  QB.(
                    Author.[
                      make("Anne", "Rice"),
                      make("J.K.", "Rowling"),
                      make("Jonathan", "Irving"),
                    ]
                    |> insertMany(Author.toRow)
                    |> into(Author.table)
                  ),
                )
              )
         |> then_(_
              // Selecting author rows, as tuples
              =>
                Client.select(
                  client,
                  RowDecode.(decodeEach(columns2("first", string, "last", string))),
                  QB.(select([e(all)]) |> from(table(Author.table))),
                )
              )
         |> P.then_(r => P.rLog(r))
         |> then_(_
              // Selecting author rows, as Author objects
              =>
                Client.select(
                  client,
                  RowDecode.(decodeEach(Author.fromJson)),
                  QB.(select([e(all)]) |> from(table(Author.table))),
                )
              )
         |> P.then_(r => P.rLog(r))
       );
};
