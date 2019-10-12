module QB = QueryBuilder;
module RE = RowEncode;
module S = Sqlite3;
module Rules = RenderQuery.DefaultRules;
module Render = RenderQuery.WithRenderingRules(Rules);

let conn = S.(connect(Memory));

S.runRaw(
  conn,
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
};

// Inserting with a "raw query"
S.insert(
  conn,
  QB.(
    [("Stephen", "King"), ("Jane", "Austen")]
    |> insertMany(RE.tuple2Row("first", string, "last", string))
    |> into(tbl("author"))
  ),
);

// Inserting using the Author-specific functions
S.insert(
  conn,
  QB.(
    Author.[make("Anne", "Rice"), make("J.K.", "Rowling"), make("Jonathan", "Irving")]
    |> insertMany(Author.toRow)
    |> into(Author.table)
  ),
);

S.select(conn, QB.(select([e(all)]) |> from(table(Author.table)))) |> Js.log;
