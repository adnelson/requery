type column = SqlQuery.Column.t;
type tableName = SqlQuery.TableName.t;
type target = SqlQuery.Select.target;
type select = SqlQuery.Select.t;
type expr = SqlQuery.Expression.t;
type aliasedExpr = SqlQuery.Aliased.t(expr);
type direction = SqlQuery.Select.direction;

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

// A single column, from a string
let col: string => expr;

// A single column, from a column name
let col_: column => expr;

// All columns (*)
let all: expr;
// All columns from a particular table (foo.*)
let allFrom: string => expr;

// Binary operators
let eq: (expr, expr) => expr;
let neq: (expr, expr) => expr;
let lt: (expr, expr) => expr;
let gt: (expr, expr) => expr;
let leq: (expr, expr) => expr;
let geq: (expr, expr) => expr;
let and_: (expr, expr) => expr;
let or_: (expr, expr) => expr;

// AND all of the expressions in the list (true if empty)
let ands: list(expr) => expr;
// OR all of the expressions in the list (false if empty)
let ors: list(expr) => expr;

// Given

// Functions
let count: expr => expr;
let distinct: expr => expr;
let max: expr => expr;
let min: expr => expr;
let sum: expr => expr;
let avg: expr => expr;
let coalesce: (expr, expr) => expr;
let call: (string, list(expr)) => expr;

// Aliased expressions (appear after a SELECT, can have an alias)
let e: (~a: string=?, expr) => aliasedExpr;

/***************************
 * Targets
 ****************************/

// A named table
let table: (~a: string=?, tableName) => target;

// Joins
let innerJoin: (target, expr, target) => target;
let leftJoin: (target, expr, target) => target;
let rightJoin: (target, expr, target) => target;
let crossJoin: (target, target) => target;

// An inner SELECT query, requires an alias
let sub: (select, string) => target;

/***************************
 * Queries
 ****************************/

let column: string => column;
let columns: list(string) => list(column);

// For ORDER BY clauses
let asc: direction;
let desc: direction;

// Modify or add an alias to a target.
let as_: (string, target) => target;

// The top-level select statement.
let select:
  (
    ~from: target=?,
    ~groupBy: list(column)=?,
    ~orderBy: list((column, option(direction)))=?,
    ~limit: int=?,
    ~where: expr=?,
    list(aliasedExpr)
  ) =>
  select;

// Change what's being selected.
let selecting: (list(aliasedExpr), select) => select;
let from: (target, select) => select;
let limit: (int, select) => select;
let where: (expr, select) => select;
let orderBy_: (list(column), select) => select;
let orderBy: (list((column, direction)), select) => select;
let orderBy1_: (column, select) => select;
let orderBy1: (column, direction, select) => select;
let orderBy2_: (column, column, select) => select;
let orderBy2: (column, direction, column, direction, select) => select;
let groupBy: (list(column), select) => select;
