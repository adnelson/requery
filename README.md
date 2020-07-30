# requery

`requery` is a library for interacting with a SQL database from a ReasonML/Ocaml application. It includes a generic SQL AST and combinators for constructing queries and parsing the results of these queries into domain objects. It is inspired by [`knex.js`](http://knexjs.org/), but leveraging the type system of ML for correctness and expressiveness.

`requery` is currently dependent on being built with `bucklescript` and the javascript ecosystem. Future work might enable it to be used in other ecosystems as well.

```reason
let (then_, resolve) = Js.Promise.(then_, resolve);
let client = RequerySqlite.Sqlite3.(makeClient(Memory));
let authors = QueryBuilder.tname("authors");
RowEncode.(
  [("Stephen", "King"), ("Jane", "Austen"), ("Kurt", "Vonnegut")]
  |> insertMany(columns2("first", string, "last", string))
  |> into(authors)
)
|> Client.insert(client)
|> then_(_ =>
     QueryBuilder.(select([e(col("first")), e(col("last"))] |> from(table(authors))))
     |> Client.select(
          client,
          RowDecode.(decodeEach(columns2("first", string, "last", string))),
        )
   )
|> then_(authors => authors |> Js.log |> resolve);
```

### Features

- A generic SQL abstract syntax tree as a suite of ReasonML types
- Functions and other tools for:
  - Building queries programmatically and composably
  - Decoding rows returned from a query into domain objects
  - Encoding domain objects into rows for database insertion
  - Orchestrating query execution with a database

### Goals

- Queries will always render into valid SQL, modulo bugs and unsupported databases.
- Query generation, query execution, and query result parsing are clearly separated at the type level.
- Modular abstractions which compose correctly, allowing you to avoid gotchas and write DRY code.

### Modular Design

The components of `requery` are designed to be modular and each can be used in whatever capacity you need. You might use it to:

- script out table and/or view creation in code, but write your queries by hand.
- automate your infrastructure tests for some existing database.
- seed tables for a unit or integration test suite.
- create a REST API or CLI to which is backed by a database.
- use the `RowDecode` library to unpack the results of queries you've written by hand
- set up a web app that can be configured to work with different databases (currently <strike>sqlite or</strike> postgres).

Note that while an ORM could be written using `requery` to structure queries, `requery` itself is not an ORM. It does not enforce or encourage any particular framework for how you structure your tables or do migrations; instead it (hopefully) provides you with the ability to build SQL however you'd like.

## Modules

- A non-exhaustive list of modules to be found in the library:
  - `Sql`: contains an abstact syntax tree for SQL. The AST is polymorphic to support DB-specific syntax. The types here are generally not used directly; instead use the functions in `QueryBuilder`.
  - `QueryBuilder`: functions for building SQL queries in a more ergonomic way than directly constructing an AST (although you can if you want). See the interface file `QueryBuilder.rei` for documentation on the various builder functions.
  - `RenderQuery`: Code to render the AST objects into actual SQL strings. You can use this library directly if you need access to the SQL, but if you're using the `Client` this will probably be abstracted away.
  - `RowEncode`: functions to serialize domain objects into "rows", that is, the data that goes into an `INSERT INTO` query.
  - `RowDecode`: functions to deserialize information returned by a query (e.g. a `SELECT` or an `INSERT` which returns data) into domain objects.
  - `Client`: an abstraction of the actual database object. This allows you to interact with your database using the `requery` abstractions.
  - `PostgresCustomSyntax`: type-safe AST for PostgresQL. Very much a WIP.
  - `PostgresClient`: functionality to connect to a postgres database.

## Examples

Let's say you have a Postgres database of books and authors, with the following tables and data. Note that we can use `requery` to create the table and insert rows, but since we're focusing on SELECT queries, we'll save that for later:

```sql
CREATE TABLE authors (id SERIAL PRIMARY KEY, first_name TEXT, last_name TEXT);
CREATE TABLE books (
  id SERIAL PRIMARY KEY,
  author_id INT NOT NULL,
  title TEXT NOT NULL,
  FOREIGN KEY (author_id) REFERENCES authors(id)
);

INSERT INTO authors (first_name, last_name) VALUES ('Stephen', 'King');
INSERT INTO books (author_id, title) VALUES (1, 'The Shining'), (1, 'Carrie');
```

Start off by adding `@adnelson/requery` as a dependency. Don't forget to update your `bsconfig.json` as well by putting `"@adnelson/requery"` under `bs-dependencies`.

One thing you might want to do is find all of the books that an author wrote. Here's an example of how that might look:

```reason
let booksByAuthor = (authorId: int): select => Requery.QueryBuilder.(
  select([
    e(tcol("authors", "first_name") ++ string(" ") ++ tcol("authors", "last_name"), ~a="name"),
    e(tcol("books", "title")),
  ])
  |> from(
    tableNamed("authors")
    |> innerJoin(tableNamed("books"),
                 tcol("authors", "id") == tcol("books", "author_id"))
    )
  |> where(tcol("authors", "id") == int(authorId))
);

Js.log(Requery.Postgres.Render.select(booksByAuthor(1)));
```

Output:

```sql
SELECT "authors"."first_name" || ' ' || "authors"."last_name" AS name, "books"."title"
FROM authors INNER JOIN books ON "authors"."id" = "books"."author_id"
WHERE "authors"."id" = 1
```

If I pipe this into `psql`:

```
⇒  node example/Books.bs.js | psql requery-example
     name     |    title
--------------+-------------
 Stephen King | The Shining
 Stephen King | Carrie
(2 rows)
```

Now of course, for a query like this the Reason code is considerably more verbose than the query which is generated at the end. But the advantage is that this query can be reused! Maybe all you need to know is the _number_ of books the author wrote. We can leverage the query we wrote before:

```reason
let bookCountByAuthor = (authorId: int): select => Requery.QueryBuilder.(
  select([e(col("name")), e(count(all))])
  |> from(booksByAuthor(authorId) |> selectAs("t"))
  |> groupBy1(column("name"))
);

Js.log(Requery.Postgres.Render.select(bookCountByAuthor(1)));
```

Output:

```sql
SELECT "name", COUNT(*) FROM (
  SELECT "authors"."first_name" || ' ' || "authors"."last_name" AS name, "books"."title"
  FROM authors INNER JOIN books ON "authors"."id" = "books"."author_id"
  WHERE "authors"."id" = 1
) AS t
GROUP BY "name"
```

Result:

```
⇒  node example/Books.bs.js | psql requery-example
     name     | count
--------------+-------
 Stephen King |     2
(1 row)
```

The `QueryBuilder` library will ensure that whatever logic you follow to construct a query, the end result will be syntactically valid SQL. Of course, it does _not_ ensure that the query will return the data you expect, or any data at all -- that's still up to you.

For a more complete example, which includes table creation, insertion and selection, see `examples/Books.re`, `examples/SqliteBooks.re` and `examples.PostgresBooks.re`.

## Supported queries

At present, the following query types have been implemented, with the following components. This list will be updated over time.

### SELECT

- Expressions
  - Primitives like ints, floats, strings, booleans, tuples
  - Combinators for operators like `&&`, `||`, `LIKE` `IS NOT NULL`, etc
  - Function calls, e.g. `COUNT(*)`
  - Encoders to translate your domain objects into SQL expressions
- `FROM` clauses
  - Tables
  - Subqueries (`SELECT * FROM (SELECT ...) AS t`)
  - `JOIN` clauses
    - `INNER JOIN`, `LEFT JOIN`, `RIGHT JOIN`, `FULL JOIN` and `CROSS JOIN`
- `GROUP BY` one or more columns
- `ORDER BY` one or more columns (with optional `DESC`/`ASC`)
- `LIMIT` clauses
- `WHERE` clauses

### INSERT

- `VALUES`, organized as one or more tuples of `(column, expression)`
- Inserting an inner `SELECT` query

### CREATE TABLE

- `IF NOT EXISTS`
- Per-column `PRIMARY KEY`, `UNIQUE`, `NOT NULL`, `CHECK` and `DEFAULT` constraints
- Per-table `PRIMARY KEY`, `FOREIGN KEY`, `UNIQUE`, and `CHECK` constraints

### CREATE VIEW

- Using a `SELECT` query
- `IF NOT EXISTS`

## Supported databases

PostgresQL. At one point SQLite had support and that might return, but I don't use it, the package doesn't build out of the box on nixos and I just haven't figured out how to get around it yet. Of course anyone can write their own library around it.

## Status and future work

**_NOTE: Requery's API is unstable and subject to change without notice._** This doesn't mean that the code is expected to be of poor quality, just that there may be any number of breaking changes until a hypothetical 1.0 release.

There's plenty left to do, and much will likely change, but at this point the library is at least worthy of playing around with for personal projects. The `QueryBuilder` library can be used to build useful queries of pretty sophiticated complexity, the `RenderQuery` library can render these into valid SQL, and functions exist for basic database interaction including object serialization/deserialization.

Planned upcoming work includes:

- Improving the abstraction of the database backend to provide an ergonomic interface, make it easy to extend, and avoid code duplication between different DBs.
- A richer set of tools for composing database actions. For example:
  - Higher-level abstractions for query building, enabling complex queries to be generated correctly
  - Query orchestration tools, enabling database interactions to be scripted for things like inserting objects which are stored across multiple tables.
- A test suite. Query generation, object encoding/decoding, SQL rendering (per DB), and query execution (per DB) should all be backed by tests.
- `DELETE FROM` and `DROP TABLE` queries.
- `WITH`, `UNION` and `UNION ALL` syntax for `SELECT` queries.
- Configurable pretty-printing of rendered SQL.
- Error handling for when queries fail.

Contributions and issue reports are very much welcome!
