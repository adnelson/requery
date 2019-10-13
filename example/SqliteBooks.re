let client: Sqlite3.client = Sqlite3.(makeClient(Memory, ~onQuery=AbstractClient.logQuery));

Books.run(client, QueryBuilder.Types.int);
