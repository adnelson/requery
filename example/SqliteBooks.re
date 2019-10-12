let client: Sqlite3.client = Sqlite3.(makeClient(Memory));
let createTables = "CREATE TABLE author (id INTEGER PRIMARY KEY, first TEXT NOT NULL, last TEXT NOT NULL);";

Books.run(client, createTables);
