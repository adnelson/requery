open Requery;

let (then_, resolve, catch) = Js.Promise.(then_, resolve, catch);
let client = RequerySqlite.Sqlite3.(makeClient(Memory, ~onQuery=Client.logQuery));
let authors = QueryBuilder.tname("authors");

// SELECT 1 AS x UNION SELECT 2 as x
QueryBuilder.(
  select([e(int(1), ~a="x")] |> fromNone) |> union1([e(int(2), ~a="x")] |> fromNone)
)
|> Client.select(client, RowDecode.(decodeEach(field("x", int))))
|> then_(Result.unwrapPromise)
|> then_(xs => Js.log(xs) |> resolve)
|> then_(_ =>
     QueryBuilder.(
       [
         cdef("first", Types.text),
         cdef("last", Types.text),
         constraint_(unique([cname("first"), cname("last")])),
       ]
       |> createTable(authors, ~ifNotExists=true)
     )
     |> Client.createTable(client)
   )
// CREATE TABLE IF NOT EXISTS authors (first TEXT NOT NULL, last TEXT NOT NULL, UNIQUE (first, last));
// INSERT INTO authors (first, last) VALUES ('Stephen', 'King'), ('Jane', 'Austen')
|> then_(_ =>
     RowEncode.(
       [("Stephen", "King"), ("Jane", "Austen")]
       |> insertMany(columns2("first", string, "last", string))
       |> into(authors)
     )
     // Run query on database
     |> Client.insert(client)
   )
// SELECT first || ' ' || last AS name FROM authors
|> then_(_ =>
     QueryBuilder.(
       select(
         [e(col("first") ++ string(" ") ++ col("last"), ~a="name")] |> from(table(authors)),
       )
     )
     // Run query, pulling the `name` from each result, getting [|"Stephen King", "Jane Austen"|]
     |> Client.select(client, RowDecode.(decodeEach(field("name", string))))
     |> then_(Result.unwrapPromise)
   )
|> then_(authors => authors |> Js.log |> resolve)
|> catch(r => resolve(Js.log(r)));
