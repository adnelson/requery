let logQuery = RequeryAbstract.Client.logQuery;
let client = RequerySqlite.Sqlite3.(makeClient(Memory, ~onQuery=logQuery));

RequeryExample.Books.run(client, RequeryAbstract.QueryBuilder.Types.int);
