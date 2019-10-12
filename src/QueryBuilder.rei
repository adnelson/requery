type column = Sql.Column.t;
type table = Sql.Table.t;
type target = Sql.Select.target;
type select = Sql.Select.t;
type expr = Sql.Expression.t;
type aliasedExpr = Sql.Aliased.t(expr);
type direction = Sql.Select.direction;
type insert = Sql.Insert.t;
type row = list((column, expr));
type toSelect('t) = 't => select;
type toInsert('t) = ('t, table) => insert;
type toColumn('t) = 't => column;
type toExpr('t) = 't => expr;
type toRow('t) = 't => row;

/***************************
 * Expressions
 ****************************/

// Literals
let int: int => expr;
let bigint: int => expr;
let float: float => expr;
let string: string => expr;
let bool: bool => expr;
let tuple: list(expr) => expr;

/************************
 *  Dealing with nulls
 ***********************/

let null: expr;
let isNull: expr => expr;
let isNotNull: expr => expr;

// Add an explicit type cast to an expression
let typed: (expr, string) => expr;

// A single column, from a string
let col: string => expr;

// A single column, from a column name
let col_: column => expr;

// A single column from a table name and column name
let tcol: (string, string) => expr;

// All columns (*)
let all: expr;

// All columns from a particular table (foo.*)
let allFrom: string => expr;

// Binary operators
let concat: (expr, expr) => expr;
let add: (expr, expr) => expr;
let subtract: (expr, expr) => expr;
let multiply: (expr, expr) => expr;
let divide: (expr, expr) => expr;
let eq: (expr, expr) => expr;
let neq: (expr, expr) => expr;
let lt: (expr, expr) => expr;
let gt: (expr, expr) => expr;
let leq: (expr, expr) => expr;
let geq: (expr, expr) => expr;
let like: (expr, expr) => expr;
let and_: (expr, expr) => expr;
let or_: (expr, expr) => expr;

// Symbolic versions of binary operators
let (++): (expr, expr) => expr;
let (+): (expr, expr) => expr;
let (-): (expr, expr) => expr;
let ( * ): (expr, expr) => expr;
let (/): (expr, expr) => expr;
let (==): (expr, expr) => expr;
let (!=): (expr, expr) => expr;
let (<): (expr, expr) => expr;
let (<=): (expr, expr) => expr;
let (>): (expr, expr) => expr;
let (>=): (expr, expr) => expr;
let (&&): (expr, expr) => expr;
let (||): (expr, expr) => expr;

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

/*********** Higher order functions **********/

// null if the value is None, else convert the Some value.
let nullable: ('t => expr, option('t)) => expr;

// Convert to a tuple.
let tuple2: (toExpr('a), toExpr('b)) => toExpr(('a, 'b));

/*********************************************/

// Aliased expressions (appear after a SELECT, can have an alias)
let e: (~a: string=?, expr) => aliasedExpr;

/***************************
 * Targets
 ****************************/

// A table target
let table: (~a: string=?, table) => target;

// A table target, via a string.
let tableNamed: (~a: string=?, string) => target;

// Joins
let innerJoin: (target, expr, target) => target;
let leftJoin: (target, expr, target) => target;
let rightJoin: (target, expr, target) => target;
let fullJoin: (target, expr, target) => target;
let crossJoin: (target, target) => target;

// An inner SELECT query, requires an alias
let selectAs: (string, select) => target;

/***************************
 * SELECT Queries
 ****************************/

// Make a `table` from a string
let tbl: string => table;

// Make a `column` from a string, without a table name.
let column: string => column;

// Make a `column` with a table name, e.g. `fruits.color`. Table name
// comes first.
let tcolumn: (string, string) => column;

// Make multiple `column`s from strings
let columns: list(string) => list(column);

// Make multiple `table`.`column`s from string pairs.
let tcolumns: list((string, string)) => list(column);

// For ORDER BY clauses
let asc: direction;
let desc: direction;

// Modify or add an alias to a target.
let as_: (string, target) => target;

// The top-level select statement.
let select:
  (
    ~from: target=?,
    // TODO groupBy can contain expressions
    ~groupBy: list(column)=?,
    ~orderBy: list((column, option(direction)))=?,
    ~limit: int=?,
    ~where: expr=?,
    list(aliasedExpr)
  ) =>
  select;

// Change what's being selected.
let selecting: (list(aliasedExpr), select) => select;

/*
 Allows the items being selected to be put first.

 let sql =
   [e(col("first") ++ string(" ") ++ col("last"))]
   |> selectFrom(tableNamed("authors"));
 */
let selectFrom: (target, list(aliasedExpr)) => select;
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
let groupBy1: (column, select) => select;

/***************************
 * INSERT Queries
 ****************************/

// Inserting literal columns/expressions.
let insertColumns: toInsert(list((column, list(expr))));
let insertRows: toInsert(list(list((column, expr))));
let insertRow: toInsert(list((column, expr)));

// Apply a conversion function to create columns and expressions.
let insertRowsWith: (toColumn('a), toExpr('b)) => toInsert(list(list(('a, 'b))));
let insertRowWith: ('a => column, 'b => expr) => toInsert(list(('a, 'b)));
let insertColumnsWith: (toColumn('a), toExpr('b)) => toInsert(list(('a, list('b))));

// Given a function to convert an object to a row, insert one or more objects.
let insertOne: toRow('t) => toInsert('t);
let insertMany: toRow('t) => toInsert(list('t));

// Insert with a SELECT query.
let insertSelect: toInsert(select);

// Set what's to be returned by the insertion
let returningColumns: (list(column), insert) => insert;
let returningColumn: (column, insert) => insert;

/* Apply a table-to-query conversion.
   let insertAuthors =
     [("Stephen", "King"), ("Jane", "Austen")]
     |> insertMany(RE.tuple2Row("first", string, "last", string))
     |> into(tbl("authors"));
   */
let into: (table, table => insert) => insert;
