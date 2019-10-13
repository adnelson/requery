module Pool = Postgres.Pool;
let pool = Pool.makePool({host: "localhost", database: "requery-example", port: 5999});

let createTable =
  QueryBuilder.(
    [
      cdef("id", typeName("SERIAL"), ~primaryKey=true),
      cdef("first", Types.text),
      cdef("last", Types.text),
    ]
    |> createTable(tname("author"), ~ifNotExists=true)
  );

Pool.runClient(pool, client => Books.run(client, createTable))
|> Utils.Promise.finally(() => Pool.releasePool(pool));
