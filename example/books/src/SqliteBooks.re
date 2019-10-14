let logQuery = RequeryAbstract.AbstractClient.logQuery;
let client = RequerySqlite.Sqlite3.(makeClient(Memory, ~onQuery=logQuery));

Books.run(client, RequeryAbstract.QueryBuilder.Types.int);
