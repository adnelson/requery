let client: Sqlite3.client = Sqlite3.(makeClient(Memory));

let createTable =
  QueryBuilder.(
    [
      cdef("id", Types.int, ~primaryKey=true),
      cdef("first", Types.text),
      cdef("last", Types.text),
    ]
    |> createTable(tname("author"), ~ifNotExists=true)
  );

Books.run(client, createTable);
