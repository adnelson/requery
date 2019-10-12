module Pool = Postgres.Pool;
let pool = Pool.makePool({host: "localhost", database: "requery-example", port: 5999});

let createTables = "CREATE TABLE author (id SERIAL PRIMARY KEY, first TEXT NOT NULL, last TEXT NOT NULL);";
Pool.runClient(pool, client => Books.run(client, createTables))
|> Utils.Promise.finally(() => Pool.releasePool(pool));
