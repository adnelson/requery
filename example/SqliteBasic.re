let client = Sqlite3.(makeClient(Memory));

AbstractClient.(
  insert(
    client,
    RowEncode.(
      [("Stephen", "King"), ("Jane", "Austen")]
      |> insertMany(columns2("first", string, "last", string))
      |> into(tbl("authors"))
    ),
  )
  |> Js.Promise.then_(_ =>
       select(
         client,
         RowDecode.(decodeEach(columns2("first", string, "last", string))),
         QueryBuilder.(
           [e(col("first") ++ string(" ") ++ col("last"))]
           |> selectFrom(tableNamed("authors"))
         ),
       )
       |> Js.Promise.(then_(authors => authors |> Js.log |> resolve))
     )
);
