open RequeryAbstract;
module Pool = RequeryPostgres.Postgres.Pool;
let pool = Pool.makePool({host: "localhost", database: "requery-example", port: 5999});

Pool.runClient(pool, client => Books.run(client, QueryBuilder.typeName("SERIAL")))
|> Utils.Promise.finally(() => Pool.releasePool(pool));
