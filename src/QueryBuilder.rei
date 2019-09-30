type target;
type expression;
type query;

let int: int => expression;
let col: string => expression;
let eq: (expression, expression) => expression;
let neq: (expression, expression) => expression;
let and_: (expression, expression) => expression;
let or_: (expression, expression) => expression;
let count: expression => expression;
let distinct: expression => expression;
let max: expression => expression;
let sum: expression => expression;
let avg: expression => expression;
let coalesce: (expression, expression) => expression;

let innerJoin: (target, expression, target) => target;
let leftJoin: (target, expression, target) => target;
let rightJoin: (target, expression, target) => target;
let crossJoin: (target, target) => target;
let subQuery: (query, string) => target;

let selectOnly: list(expression) => query;
let selectFrom: (list(expression), target) => query;

let select: (
  list(expression),
  ~from: option(target) = ?,
  ~groupBy: list(string) = [],
  ~where: option(expression) = ?
) => query;
