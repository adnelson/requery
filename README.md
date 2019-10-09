# requery

`requery` is a library for interacting with a SQL database from a ReasonML application. It includes a generic SQL AST and combinators for constructing queries and parsing the results of these queries into domain objects. It is inspired by [`knex.js`](http://knexjs.org/), but leveraging the type system of ReasonML/Ocaml for new awesomeness.

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

### `SELECT` queries

- Expressions
  - Primitives like ints, floats, strings, booleans, tuples
  - Combinators for operators like `&&`, `||`, `LIKE` `IS NOT NULL`, etc
  - Function calls, e.g. `COUNT(*)`
  - Custom encoders
- `FROM` clauses
  - Tables
  - Subqueries (`SELECT * FROM (SELECT ...) AS t`)
  - `JOIN` clauses
    - `INNER JOIN`, `LEFT JOIN`, `RIGHT JOIN`, `FULL JOIN` and `CROSS JOIN`
- `GROUP BY`
- `ORDER BY` (`DESC`/`ASC`)
- `LIMIT` clauses
- `WHERE` clauses

### `INSERT` queries

- `VALUES`, organized as one or more tuples of `(column, expression)`
- Inserting an inner `SELECT` query

## Supported databases

Only PostgresQL so far, but SQLite will be hopefully following close behind. Database interaction is abstracted via a functor called Query. You can keep your code backend-agnostic by abstracting with a functor. You can see an example of this in `example/Main.re`.

## Examples

TODO! There's a tiny smattering of example code in the `examples` directory,
and it's not well (or at all) documented. Much more will be forthcoming soon.

## Status and future work

There's plenty left to do, and much will likely change, but at this point the library is at least worthy of playing around with for personal projects. The `QueryBuilder` library can be used to build useful queries of pretty sophiticated complexity, the `RenderQuery` library can render

in addition to a test suite. But in the meantime hopefully the code is
readable enough.
