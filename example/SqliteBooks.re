let client: Sqlite3.client = Sqlite3.(makeClient(Memory));
Books.run(client);
