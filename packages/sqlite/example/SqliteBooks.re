let logQuery = Requery.Client.logQuery;
let client = RequerySqlite.Sqlite3.(makeClient(Memory, ~onQuery=logQuery));

RequeryExample.Books.run(client, Requery.QueryBuilder.Types.int);
