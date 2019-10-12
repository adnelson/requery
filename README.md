# requery

`requery` is a library for interacting with a SQL database from a ReasonML/Ocaml application. It includes a generic SQL AST and combinators for constructing queries and parsing the results of these queries into domain objects. It is inspired by [`knex.js`](http://knexjs.org/), but leveraging the type system of ML for correctness and expressiveness.

`requery` is currently dependent on being built with `bucklescript` and the javascript ecosystem. Future work might enable it to be used in other ecosystems as well.

```reason
let conn = Sqlite.(connect({path: "test.db", memory: true}));

Sqlite.insert(conn,
  [("Stephen", "King"), ("Jane", "Austen")]
  |> insertMany(tuple2Row("first", string, "last", string))
  |> into(tbl("authors")));

Sqlite.select(conn,
  [e(col("first") ++ string(" ") ++ col("last"))]
  |> selectFrom(tableNamed("authors")))
  |> Js.log;
```

### Features

* A generic SQL abstract syntax tree as a suite of ReasonML types
* Functions to build queries programmatically and composably
* Functions to decode rows returned from a query into domain objects
* Functions to encode domain objects into rows for database insertion
* Functors for using these queries to interact with a database

### Goals

* Queries will always render into valid SQL, modulo bugs and unsupported databases.
* Query generation, query execution, and query result parsing are clearly separated at the type level.
* Abstractions compose correctly, allowing you to avoid gotchas and write DRY code.

#### Not an ORM

`requery` is not intended to be an ORM. It instead is intentended to provide powerful abstractions for query generation, encoding/decoding objects, and database interaction. As a user you can use as much or as little of the types as you want, whether you just want to just dump some SQL to stdout, or you want to use the `Query` functor to set up an app that interacts with a database, or maybe you want to write your queries by hand, but you want to use the `RowDecode` library to unpack the results. That said, an ORM might at some point use `requery` as a library for parts of its implementation.

## Examples

Let's say you have a Postgres database of books and authors. (Note: we can use `requery` to insert the rows, but we'll save that for later)

```sql
CREATE TABLE authors (id SERIAL PRIMARY KEY, first_name TEXT, last_name TEXT);
CREATE TABLE books (
  id SERIAL PRIMARY KEY,
  author_id INT NOT NULL,
  title TEXT NOT NULL,
  FOREIGN KEY (author_id) REFERENCES authors(id)
);

INSERT INTO authors(first_name, last_name) VALUES ('Stephen', 'King');
INSERT INTO books(author_id, title) VALUES (1, 'The Shining'), (1, 'Carrie');
```

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

## Supported databases

PostgresQL and SQLite so far.

## Status and future work

There's plenty left to do, and much will likely change, but at this point the library is at least worthy of playing around with for personal projects. The `QueryBuilder` library can be used to build useful queries of pretty sophiticated complexity, the `RenderQuery` library can render these into valid SQL, and functions exist for basic database interaction including object serialization/deserialization.

Planned upcoming work includes:

* Figure out the best way to abstract the database backend to provide an ergonomic interface, make it easy to extend, and avoid code duplication between different DBs.
* A test suite. Query generation, object encoding/decoding, SQL rendering (per DB), and query execution (per DB) should all be backed by tests.
* `DELETE FROM` queries.
* `CREATE VIEW`. While tools for table creation are not currently planned, it should be easy to create views, since we can create `SELECT` queries.
* A richer set of tools for composing database actions. For example, making it easy to insert objects which are stored across multiple tables.
* Pretty-printing of rendered SQL.

Contributions are very much welcome!
