open RequeryAbstract;
let (then_, resolve) = Js.Promise.(then_, resolve);
let client = RequerySqlite.Sqlite3.(makeClient(Memory, ~onQuery=AbstractClient.logQuery));
let authors = QueryBuilder.tname("authors");

// CREATE TABLE IF NOT EXISTS authors (first TEXT NOT NULL, last TEXT NOT NULL, UNIQUE (first, last));
QueryBuilder.(
  [
    cdef("first", Types.text),
    cdef("last", Types.text),
    constraint_(unique([cname("first"), cname("last")])),
  ]
  |> createTable(authors, ~ifNotExists=true)
)
|> AbstractClient.createTable(client)
// Create query: INSERT INTO authors (first, last) VALUES ('Stephen', 'King'), ('Jane', 'Austen')
|> then_(_ =>
     RowEncode.(
       [("Stephen", "King"), ("Jane", "Austen")]
       |> insertMany(columns2("first", string, "last", string))
       |> into(authors)
     )
     // Run query on database
     |> AbstractClient.insert(client)
   )
// SELECT first, last FROM authors
|> then_(_ =>
     QueryBuilder.([e(col("first")), e(col("last"))] |> selectFrom(table(authors)))
     // Run query, decoding result into [|("Stephen", "King"), ("Jane", "Austen")|]
     |> AbstractClient.select(
          client,
          RowDecode.(decodeEach(columns2("first", string, "last", string))),
        )
   )
|> then_(authors => authors |> Js.log |> resolve);
