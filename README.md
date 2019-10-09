# requery

`requery` is a library for interacting with a SQL database from a ReasonML application. It includes a generic SQL AST and combinators for constructing queries and parsing the results of these queries into domain objects. It is inspired by [`knex.js`](http://knexjs.org/), but leveraging the type system of ReasonML/Ocaml for correctness and expressiveness.

`requery` is currently dependent on being built with `bucklescript` and the javascript ecosystem. Future work might enable it to be used from native ReasonML/Ocaml apps as well.

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
- `GROUP BY`
- `ORDER BY` (with optional `DESC`/`ASC`)
- `LIMIT` clauses
- `WHERE` clauses

### INSERT

- `VALUES`, organized as one or more tuples of `(column, expression)`
- Inserting an inner `SELECT` query

## Supported databases

Only PostgresQL so far, but SQLite will be hopefully following close behind. Database interaction is abstracted via a functor called Query. You can keep your code backend-agnostic by abstracting with a functor. You can see an example of this in `example/Main.re`.

## Examples

More examples will be forthcoming soon, but here's an example `SELECT` query:

```reason
let booksByAuthor = (authorId: int) => Requery.QueryBuilder.(
  select([
    e(tcol("authors", "first_name") ++ tcol("authors", "last_name"), ~a="name"),
    e(tcol("books", "title")),
  ])
  |> from(
    tableNamed("authors")
    |> innerJoin(tableNamed("books"),
                 tcol("authors", "id") == tcol("books", "author_id"))
    )
  |> where(tcol("author", "id") == int(authorId))
);

Js.log(Postgres.Render.select(booksByAuthor(10)));
```

Output:

```sql
SELECT "authors"."first_name" || "authors"."last_name" AS name, "books"."title"
FROM authors INNER JOIN books ON "authors"."id" = "books"."author_id"
WHERE "author"."id" = 10
```

## Status and future work

There's plenty left to do, and much will likely change, but at this point the library is at least worthy of playing around with for personal projects. The `QueryBuilder` library can be used to build useful queries of pretty sophiticated complexity, the `RenderQuery` library can render these into valid SQL, and functions exist for basic database interaction including object serialization/deserialization.

Planned upcoming work includes:

* SQLite support. Besides being useful, this will provide guidance for and validation of the best way to abstract the database backend.
* A test suite. Query generation, object encoding/decoding, SQL rendering (per DB), and query execution (per DB) should all be backed by tests.
* `DELETE FROM` queries.
* `CREATE VIEW`. While tools for table creation are not currently planned, it should be easy to create views, since we can create `SELECT` queries.
* A richer set of tools for composing database actions. For example, making it easy to insert objects which are stored across multiple tables.
* Pretty-printing of rendered SQL.
