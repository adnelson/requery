type target;
type expr;
type aliasedExpr;
type query;

/***************************
* Expressions
****************************/

// Literals
let int: int => expr;
let bigint: int => expr;
let float: float => expr;
let string: string => expr;
let bool: bool => expr;

// Add an explicit type cast to an expression
let typed: (expr, string) => expr;

// A single column
let col: string => expr;

// All columns (*)
let all: expr;
// All columns from a particular table (foo.*)
let allFrom: string => expr;

// Binary operators
let eq: (expr, expr) => expr;
let neq: (expr, expr) => expr;
let and_: (expr, expr) => expr;
let or_: (expr, expr) => expr;
let lt: (expr, expr) => expr;
let gt: (expr, expr) => expr;
let leq: (expr, expr) => expr;
let geq: (expr, expr) => expr;

// Functions
let count: expr => expr;
let distinct: expr => expr;
let max: expr => expr;
let sum: expr => expr;
let avg: expr => expr;
let coalesce: (expr, expr) => expr;

// Aliased expressions (appear after a SELECT, can have an alias)
let e: (expr, ~a: option(string) = ?) => aliasedExpr;

/***************************
* Targets
****************************/

// A named table
let table: (string, ~a: option(string) = ?) => target;

// An inner SELECT query, with an alias
let subQuery: (query, string) => target;

// Joins
let innerJoin: (target, expr, target) => target;
let leftJoin: (target, expr, target) => target;
let rightJoin: (target, expr, target) => target;
let crossJoin: (target, target) => target;

/***************************
* Queries
****************************/

// The top-level select statement.
let select: (
  list(aliasedExpr),
  ~from: option(target) = ?,
  ~groupBy: list(string) = [],
  ~where: option(expr) = ?
) => query;
