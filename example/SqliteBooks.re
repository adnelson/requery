let client: Sqlite3.client = Sqlite3.(makeClient(Memory));
let createTable =
  RenderQuery.Default.createTable(
    Sql.(
      CreateTable.(
        QueryBuilder.{
          name: tname("author"),
          statements: [|
            ColumnDef(makeColumnDef(cname("id"), Types.int, [|PrimaryKey1|])),
            ColumnDef(makeColumnDef(cname("first"), Types.text, [|NotNull|])),
            ColumnDef(makeColumnDef(cname("last"), Types.text, [|NotNull|])),
          |],
          ifNotExists: false,
        }
      )
    ),
  );

Books.run(client, createTable);
