module Client = Requery.Client;
open Requery;
module QB = QueryBuilder;
module RE = RowEncode;
module Rules = RenderQuery.DefaultRules;
module Render = RenderQuery.WithRenderingRules(Rules);
module L = Utils.List;
module P = Utils.Promise;
module J = Utils.Json;
module JD = Utils.Json.Decode;
module JE = Utils.Json.Encode;
module C = Requery.Client;
let (then_, then2, resolve, catch, rLog, finally, all2, rLog2) =
  P.(then_, then2, resolve, catch, rLog, finally, all2, rLog2);

module Author = {
  let tableName = QB.tname("author");
  type t('id) = {
    id: 'id,
    first: string,
    last: string,
  };
  let make = (first, last) => {id: (), first, last};
  let toRow = ({first, last}) =>
    RE.(stringRow([("first", first |> string), ("last", last |> string)]));
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

  // This type and its use to create the table, is a conceptual work in progress.
  type fields('id, 'first, 'last) =
    | Id('id)
    | First('first)
    | Last('last);

  let fieldToColumnDef = idType =>
    QB.(
      fun
      | Id () => cdef("id", idType, ~primaryKey=true)
      | First () => cdef("first", Types.text)
      | Last () => cdef("last", Types.text)
    );

  let createTable = idType => {
    let cdefs = [Id(), First(), Last()]->(L.map(fieldToColumnDef(idType)));
    QueryBuilder.(
      L.concat(cdefs, [QB.(constraint_(unique([cname("first"), cname("last")])))])
      |> createTable(tableName, ~ifNotExists=true)
    );
  };
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
        cdef("id", idType, ~primaryKey=true),
        // Note: can have spaces in column name
        cdef("author id", idType),
        cdef("title", Types.text),
        constraint_(foreignKey(cname("author id"), (tname("author"), cname("id")))),
      ]
      |> createTable(tname("book"), ~ifNotExists=true)
    );
};

let authorBooksSelect =
  QueryBuilder.(
    select(
      [e(tcol("a", "id"), ~a="author id")]
      |> from(
           table(Author.tableName, ~a="a")
           |> innerJoin(
                table(Book.tableName, ~a="b"),
                tcol("a", "id") == tcol("b", "author id"),
              ),
         ),
    )
  );

let run = (client, idType) => {
  C.createTable(client, Author.createTable(idType))
  |> then_(_ => C.createTable(client, Book.createTable(idType)))
  // TODO when figure out ifNotExists problem
  //  |> then_(_ => C.createView(client, authorBooksSelect |> createView(tname("author_books"))))
  |> then_(_
       // Inserting with an explicit query, using columns2 to define the
       // encoding on the fly
       =>
         QB.(
           [("Stephen", "King"), ("Jane", "Austen")]
           |> insertMany(RE.columns2("first", string, "last", string))
           |> into(tname("author"))
         )
         |> C.insert(client)
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
              |> C.insert(client)
            )
         // Selecting author rows, decoding as tuples
         |> then_(_ =>
              QB.(select([e(all)] |> from(table(Author.tableName))))
              |> C.select(
                   client,
                   RowDecode.(decodeEach(columns3("id", int, "first", string, "last", string))),
                 )
            )
         // Log them to console
         |> then_(rows => rLog(rows))
         // Selecting author rows, decoding as Author objects
         |> then_(_ =>
              QB.(select([e(all)] |> from(table(Author.tableName))))
              |> C.select(client, RowDecode.(decodeEach(Author.fromJson)))
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
              |> C.select(client, RowDecode.(decodeEach(field("id", int))))
            )
         |> then_(rows => rLog(rows))
       );
};
