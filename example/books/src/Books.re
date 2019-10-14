open RequeryAbstract;
module QB = QueryBuilder;
module RE = RowEncode;
module Rules = RenderQuery.DefaultRules;
module Render = RenderQuery.WithRenderingRules(Rules);
module Client = AbstractClient;
module P = Utils.Promise;
module J = Utils.Json;
module JD = Utils.Json.Decode;
module JE = Utils.Json.Encode;
let (then_, then2, resolve, catch, rLog, finally, all2, rLog2) =
  Utils.Promise.(then_, then2, resolve, catch, rLog, finally, all2, rLog2);

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

  let createTable = idType =>
    QueryBuilder.(
      [
        cdef("id", idType, ~primaryKey=true),
        cdef("first", Types.text),
        cdef("last", Types.text),
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
        cdef("id", idType, ~primaryKey=true),
        cdef("author id", idType),
        cdef("title", Types.text),
        constraint_(foreignKey(cname("author id"), (tname("author"), cname("id")))),
      ]
      |> createTable(tname("book"), ~ifNotExists=true)
    );
};

// View of books written by each author
let authorBooksView =
  QueryBuilder.(
    select([e(all)])
    |> from(
         table(Author.tableName, ~a="a")
         |> innerJoin(table(Book.tableName, ~a="b"), tcol("a", "id") == tcol("b", "author id")),
       )
    |> createView(tname("author_books"))
  );

let run = (client, idType) => {
  Client.createTable(client, Author.createTable(idType))
  |> then_(_ => Client.createTable(client, Book.createTable(idType)))
  |> then_(_ => Client.createView(client, authorBooksView))
  |> then_(_
       // Inserting with an explicit query, using columns2 to define the encoding on the fly
       =>
         QB.(
           [("Stephen", "King"), ("Jane", "Austen")]
           |> insertMany(RE.columns2("first", string, "last", string))
           |> into(tname("author"))
         )
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
              QB.(select([e(all)]) |> from(table(Author.tableName)))
              |> Client.select(
                   client,
                   RowDecode.(decodeEach(columns3("id", int, "first", string, "last", string))),
                 )
            )
         |> P.then_(r => P.rLog(r))
         // Selecting author rows, decoding as Author objects
         |> then_(_ =>
              QB.(select([e(all)]) |> from(table(Author.tableName)))
              |> Client.select(client, RowDecode.(decodeEach(Author.fromJson)))
            )
       );
};
