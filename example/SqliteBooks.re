module QB = QueryBuilder;
module RE = RowEncode;
module S = Sqlite3;
module Rules = RenderQuery.DefaultRules;
module Render = RenderQuery.WithRenderingRules(Rules);
module DBClient = AbstractClient.DBClient;
module P = Utils.Promise;
module JD = Utils.Json.Decode;

let client = S.(makeClient(Memory));

DBClient.execRaw(
  client,
  "CREATE TABLE author (id INTEGER PRIMARY KEY, first TEXT NOT NULL, last TEXT NOT NULL);",
);

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

// Inserting with an explicit query, using columns2 to define the encoding on the fly
DBClient.insert(
  client,
  QB.(
    [("Stephen", "King"), ("Jane", "Austen")]
    |> insertMany(RE.columns2("first", string, "last", string))
    |> into(tbl("author"))
  ),
);

// Inserting using the Author-specific functions
DBClient.insert(
  client,
  QB.(
    Author.[make("Anne", "Rice"), make("J.K.", "Rowling"), make("Jonathan", "Irving")]
    |> insertMany(Author.toRow)
    |> into(Author.table)
  ),
);

// Selecting author rows, as tuples
DBClient.select(
  client,
  RowDecode.(decodeEach(columns2("first", string, "last", string))),
  QB.(select([e(all)]) |> from(table(Author.table))),
)
|> P.then_(r => P.rLog(r));

// Selecting author rows, as Author objects
DBClient.select(
  client,
  RowDecode.(decodeEach(Author.fromJson)),
  QB.(select([e(all)]) |> from(table(Author.table))),
)
|> P.then_(r => P.rLog(r));
