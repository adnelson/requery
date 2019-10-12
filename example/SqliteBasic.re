let (then_, resolve) = Js.Promise.(then_, resolve);
let client = Sqlite3.(makeClient(Memory));
let authors = QueryBuilder.tbl("authors");
RowEncode.(
  [("Stephen", "King"), ("Jane", "Austen"), ("Kurt", "Vonnegut")]
  |> insertMany(columns2("first", string, "last", string))
  |> into(authors)
)
|> AbstractClient.insert(client)
|> then_(_ =>
     QueryBuilder.([e(col("first")), e(col("last"))] |> selectFrom(table(authors)))
     |> AbstractClient.select(
          client,
          RowDecode.(decodeEach(columns2("first", string, "last", string))),
        )
   )
|> then_(authors => authors |> Js.log |> resolve);
