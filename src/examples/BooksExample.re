module QB = QueryBuilder;
module RE = RowEncode;
module Rules = RenderQuery.DefaultRules;
module Render = RenderQuery.WithRenderingRules(Rules);
open Utils.Abbreviations;
let (then_, then2, resolve, catch, rLog, finally, all2, rLog2) =
  P.(then_, then2, resolve, catch, rLog, finally, all2, rLog2);

// It's not necessary to pre-define your column names but it can help
// reduce the chance of typos.
let idCol = QB.cname("id");
let firstCol = QB.cname("first");
let lastCol = QB.cname("last");

module Author = {
  let tableName = QB.tname("author");
  type t('id) = {
    id: 'id,
    first: string,
    last: string,
  };
  let make = (first, last) => {id: (), first, last};
  let toRow = ({first, last}) => RE.[(firstCol, first |> string), (lastCol, last |> string)];
  let toJson = author =>
    JE.(
      object_([
        ("id", author.id |> int),
        ("first", author.first |> string),
        ("last", author.last |> string),
      ])
    );

  let fromJson = j =>
    JD.{
      id: j |> field("id", int),
      first: j |> field("first", string),
      last: j |> field("last", string),
    };

  let createTable = idType =>
    QueryBuilder.(
      [
        cdef(cname("id"), idType, ~primaryKey=true),
        cdef(cname("first"), Types.text),
        cdef(cname("last"), Types.text),
        constraint_(unique([cname("first"), cname("last")])),
      ]
      |> createTable(tableName, ~ifNotExists=true)
    );
};

module Book = {
  let tableName = QB.tname("book");
  type t('id, 'author) = {
    id: 'id,
    author: 'author,
    title: string,
  };
  let make = (author, title) => {id: (), author, title};
  let toRow = ({author, title}) =>
    RE.(stringRow([("author id", author |> int), ("title", title |> string)]));
  let fromJson = j =>
    JD.{
      id: j |> field("id", int),
      author: j |> field("author id", int),
      title: j |> field("title", string),
    };

  // Note: this demonstrates a column name with a space in it -- this will be
  // appear quoted in the output SQL.
  let createTable = idType =>
    QueryBuilder.(
      [
        cdef(cname("id"), idType, ~primaryKey=true),
        // Note: can have spaces in column name
        cdef(cname("author id"), idType),
        cdef(cname("title"), Types.text),
        constraint_(
          foreignKey(~onDelete=Cascade, cname("author id"), (tname("author"), cname("id"))),
        ),
      ]
      |> createTable(tname("book"), ~ifNotExists=true)
    );
};

let authorBooksSelect =
  QueryBuilder.(
    select(
      [e(tcol(tname("a"), cname("id")), ~a="author id")]
      |> from(
           table(Author.tableName, ~a="a")
           |> innerJoin(
                table(Book.tableName, ~a="b"),
                QB.Op.(
                  tcol(tname("a"), cname("id")) == tcol(tname("b"), cname("author id"))
                ),
              ),
         ),
    )
  );

let insertAuthors =
  QB.(
    [("Stephen", "King"), ("Jane", "Austen")]
    |> insertMany(RE.columns2("first", string, "last", string))
    |> into(tname("author"))
  );

let getAuthorIdsCTE =
  QB.(
    with_(
      tname("author_ids"),
      [cname("id")],
      authorBooksSelect,
      select([e(all)] |> from(table(tname("author_ids")))),
    )
  );

let run = (client, idType) => {
  client->Client.createTable(Author.createTable(idType))
  |> then_(_ => client->Client.createTable(Book.createTable(idType)))
  // TODO when figure out ifNotExists problem
  //  |> then_(_ => Client.createView(client, authorBooksSelect |> createView(tname("author_books"))))
  |> then_(_
       // Inserting with an explicit query, using columns2 to define the
       // encoding on the fly
       =>
         insertAuthors
         |> Client.insert(client)
         // Inserting using the Author-specific functions
         |> then_(_ =>
              QB.(
                Author.[
                  make("Anne", "Rice"),
                  make("J.K.", "Rowling"),
                  make("Jonathan", "Irving"),
                ]
                |> insertMany(Author.toRow)
                |> into(Author.tableName)
              )
              |> Client.insert(client)
            )
         // Selecting author rows, decoding as tuples
         |> then_(_ =>
              QB.(select([e(all)] |> from(table(Author.tableName))))
              |> Client.select(
                   client,
                   RowDecode.(
                     decodeEach(columns3(idCol, int, firstCol, string, lastCol, string))
                   ),
                 )
            )
         // Log them to console
         |> then_(rows => rLog(rows))
         // Selecting author rows, decoding as Author objects
         |> then_(_ =>
              QB.(select([e(all)] |> from(table(Author.tableName))))
              |> Client.select(client, RowDecode.(decodeEach(Author.fromJson)))
            )
         |> then_(rows => rLog(rows))
         // Use a WITH query (CTE)
         |> then_(_ =>
              QB.(
                with_(
                  tname("author_ids"),
                  [cname("id")],
                  authorBooksSelect,
                  select([e(all)] |> from(table(tname("author_ids")))),
                )
              )
              |> Client.select(client, RowDecode.(decodeEach(field("id", int))))
            )
         |> then_(rows => rLog(rows))
       );
};
