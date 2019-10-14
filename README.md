# requery

`requery` is a library for interacting with a SQL database from a ReasonML/Ocaml application. It includes a generic SQL AST and combinators for constructing queries and parsing the results of these queries into domain objects. It is inspired by [`knex.js`](http://knexjs.org/), but leveraging the type system of ML for correctness and expressiveness.

`requery` is currently dependent on being built with `bucklescript` and the javascript ecosystem. Future work might enable it to be used in other ecosystems as well.

```reason
let (then_, resolve) = Js.Promise.(then_, resolve);
let client = Sqlite3.(makeClient(Memory));
let authors = QueryBuilder.tbl("authors");
RowEncode.(
  [("Stephen", "King"), ("Jane", "Austen"), ("Kurt", "Vonnegut")]
  |> insertMany(columns2("first", string, "last", string))
  |> into(authors)
)
|> AbstractClient.insert(client)
|> then_(_ =>
     QueryBuilder.([e(col("first")), e(col("last"))] |> selectFrom(table(authors)))
     |> AbstractClient.select(
          client,
          RowDecode.(decodeEach(columns2("first", string, "last", string))),
        )
   )
|> then_(authors => authors |> Js.log |> resolve);
```

### Features

* A generic SQL abstract syntax tree as a suite of ReasonML types
* Functions and other tools for:
  * Building queries programmatically and composably
  * Decoding rows returned from a query into domain objects
  * Encoding domain objects into rows for database insertion
  * Orchestrating query execution with a database

### Goals

* Queries will always render into valid SQL, modulo bugs and unsupported databases.
* Query generation, query execution, and query result parsing are clearly separated at the type level.
* Abstractions compose correctly, allowing you to avoid gotchas and write DRY code.

Note that while an ORM could be written using `requery` to structure queries, `requery` itself is not an ORM. The tools it provides -- powerful abstractions for query generation, encoding/decoding objects, and database interaction -- are designed to be modular and used as much or as little as you want. You might just want to script out database seeding for a test suite, write a client library for a database that already exists, write your queries by hand and use the `RowDecode` library to unpack the results of queries you've written by hand, or set up a web app that can work with different databases agnostic.

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

Start off by adding `requery-abstract` and `requery-postgres` as dependencies. Don't forget to update your `bsconfig.json` as well.

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

Now of course, for a query like this the Reason code is considerably more verbose than the query which is generated at the end. But the advantage is that this query can be reused! Maybe all you need to know is the *number* of books the author wrote. We can leverage the query we wrote before:

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

The `QueryBuilder` library will ensure that whatever logic you follow to construct a query, the end result will be syntactically valid SQL. Of course, it does *not* ensure that the query will return the data you expect, or any data at all -- that's still up to you.

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
- Per-column `PRIMARY KEY`, `FOREIGN KEY`, `UNIQUE`, `NOT NULL`, `CHECK` and `DEFAULT` constraints
- Per-table `PRIMARY KEY`, `FOREIGN KEY`, `UNIQUE`, and `CHECK` constraints

### CREATE VIEW

- Using a `SELECT` query
- `IF NOT EXISTS`

## Supported databases

PostgresQL and SQLite so far.

## Status and future work

There's plenty left to do, and much will likely change, but at this point the library is at least worthy of playing around with for personal projects. The `QueryBuilder` library can be used to build useful queries of pretty sophiticated complexity, the `RenderQuery` library can render these into valid SQL, and functions exist for basic database interaction including object serialization/deserialization.

Planned upcoming work includes:

* Improving the abstraction of the database backend to provide an ergonomic interface, make it easy to extend, and avoid code duplication between different DBs.
* A richer set of tools for composing database actions. For example, making it easy to insert objects which are stored across multiple tables.
* A test suite. Query generation, object encoding/decoding, SQL rendering (per DB), and query execution (per DB) should all be backed by tests.
* `DELETE FROM` queries.
* Pretty-printing of rendered SQL.
* Error handling for when queries fail.

Contributions are very much welcome!
