type target;
type expr;
type aliasedExpr;
type query;

// Creating expressions
let int: int => expr;
let bigint: int => expr;
let float: float => expr;
let string: string => expr;
let bool: bool => expr;

let typed: (expr, string) => expr;
let col: string => expr;
let all: expr;
let allFrom: string => expr;
let eq: (expr, expr) => expr;
let neq: (expr, expr) => expr;
let and_: (expr, expr) => expr;
let or_: (expr, expr) => expr;
let count: expr => expr;
let distinct: expr => expr;
let max: expr => expr;
let sum: expr => expr;
let avg: expr => expr;
let coalesce: (expr, expr) => expr;

// Aliased expressions
let e: (expr, ~a: option(string) = ?) => aliasedExpr;

// Creating targets
let table: (string, ~a: option(string) = ?) => target;
let innerJoin: (target, expr, target) => target;
let leftJoin: (target, expr, target) => target;
let rightJoin: (target, expr, target) => target;
let crossJoin: (target, target) => target;
let subQuery: (query, string) => target;

let select: (
  list(aliasedExpr),
  ~from: option(target) = ?,
  ~groupBy: list(string) = [],
  ~where: option(expr) = ?
) => query;
