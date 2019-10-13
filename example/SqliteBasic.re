let (then_, resolve) = Js.Promise.(then_, resolve);
let client = Sqlite3.(makeClient(Memory));
let authors = QueryBuilder.tname("authors");

// Create query: INSERT INTO authors (first, last) VALUES ('Stephen', 'King'), ('Jane', 'Austen')
RowEncode.(
  [("Stephen", "King"), ("Jane", "Austen")]
  |> insertMany(columns2("first", string, "last", string))
  |> into(authors)
)
// Run query on database
|> AbstractClient.insert(client)
|> then_(_
     // SELECT first, last FROM authors
     =>
       QueryBuilder.([e(col("first")), e(col("last"))] |> selectFrom(table(authors)))
       // Run query, decoding result into [|("Stephen", "King"), ("Jane", "Austen")|]
       |> AbstractClient.select(
            client,
            RowDecode.(decodeEach(columns2("first", string, "last", string))),
          )
     )
|> then_(authors => authors |> Js.log |> resolve);
