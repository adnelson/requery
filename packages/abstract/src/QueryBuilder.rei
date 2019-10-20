type columnName = Sql.ColumnName.t;
type tableName = Sql.TableName.t;
type aliased('t) = Sql.Aliased.t('t);
type constraintName = Sql.ConstraintName.t;
type tableConstraint = Sql.CreateTable.tableConstraint;
type column = Sql.Column.t;
type typeName = Sql.TypeName.t;
type target = Sql.Select.target;
type selectInUnion = Sql.Select.selectInUnion;
type selectVariant = Sql.Select.selectVariant;
type select = Sql.Select.select;
type expr = Sql.Expression.t;
type direction = Sql.Select.direction;
type insert('r) = Sql.Insert.t('r);
type statement = Sql.CreateTable.statement;
type createTable = Sql.CreateTable.t;
type createView = Sql.CreateView.t;
type whereClause = Sql.Select.whereClause;

/****************************
 * Encoder types
 ***************************/

type row = list((column, expr));
type toSelect('t) = 't => select;
type toInsert('r, 't) = ('t, tableName) => insert('r);
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

/************************
 *  Dealing with types
 ***********************/

// Add an explicit type cast to an expression
let typed: (expr, typeName) => expr;

/**********************************************
 *  Dealing with specialized name types
 **********************************************/

// A single column, from a string
let col: string => expr;

// Multiple columns
let cols: list(string) => list(expr);

// A single column, from a column name
let col_: column => expr;

// A single column from a table name and column name
let tcol: (string, string) => expr;

// Make a `tableName` from a string
let tname: string => tableName;

// Make a `column` from a string
let cname: string => columnName;

// Make multiple `columnNames` from strings
let cnames: list(string) => list(columnName);

// Make a type name from a string.
let typeName: string => typeName;

// Make a `column` from a string, without a table name.
// Remember the difference between the `column` and `columnName` types
// is that the former can include a table name prefix (see `tcolumn`).
let column: string => column;

// Make a `column` with a table name, e.g. `fruits.color`. Table name
// comes first.
let tcolumn: (string, string) => column;

// Make multiple `column`s from strings.
let columns: list(string) => list(column);

// Make multiple `table`.`column`s from string pairs.
let tcolumns: list((string, string)) => list(column);

// All columns (*)
let all: expr;

// All columns from a particular table (foo.*)
let allFrom: string => expr;

// Operators
let between: (expr, expr, expr) => expr;
let in_: (expr, expr) => expr;
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
let not: expr => expr;

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

// Convert a list to a tuple, given a way to convert each item in the list.
let tupleOf: toExpr('a) => toExpr(list('a));

/*********************************************/

// Aliased expressions (appear after a SELECT, can have an alias)
let e: (~a: string=?, expr) => aliased(expr);

/***************************
 * Targets
 ****************************/

// A table target
let table: (~a: string=?, tableName) => target;

// A table target, via a string.
let tableNamed: (~a: string=?, string) => target;

// Joins
let innerJoin: (target, expr, target) => target;
let leftJoin: (target, expr, target) => target;
let rightJoin: (target, expr, target) => target;
let fullJoin: (target, expr, target) => target;
let crossJoin: (target, target) => target;

// An inner SELECT query. Requires an alias:
// `SELECT * FROM (SELECT ..) AS alias`
let selectAs: (string, select) => target;

/***************************
 * SELECT Queries
 ****************************/

// For ORDER BY clauses
let asc: direction;
let desc: direction;

// Modify or add an alias to a target.
let as_: (string, target) => target;

// Creates a top-level select statement.
let select: selectInUnion => select;

// Creates a top-level select statement with a limit of N.
let selectN: (int, selectInUnion) => select;

// Shorthand: creates a top-level select statement, with a limit of 1.
let select1: selectInUnion => select;

// Used to select from a table or another target.
//
// For example:
//
// select([e(col("x")), e(col("y"))] |> from(tableNamed(tname("points"))))
//
//   ==> SELECT x, y FROM points;
//
let from: (target, list(aliased(expr))) => selectInUnion;

// Used to select static values or constants. For example
// select([e(int(1) + int(2))] |> fromNone)
//   ==> SELECT 1 + 2;
let fromNone: list(aliased(expr)) => selectInUnion;

// Add a WHERE clause to a selectInUnion.
let where: (expr, selectInUnion) => selectInUnion;

// Add an additional condition to a WHERE. If no WHERE is present on the input
// this alone will be set.
// NOTE: this stomps on a WHERE EXISTS. This may change in the future.
let andWhere: (expr, selectInUnion) => selectInUnion;

// Add an alternative condition to a WHERE. If no WHERE is present on the input
// this alone will be set.
// NOTE: this stomps on a WHERE EXISTS. This may change in the future.
let orWhere: (expr, selectInUnion) => selectInUnion;

let whereExists: (select, selectInUnion) => selectInUnion;

let groupBy: (~having: expr=?, list(expr), selectInUnion) => selectInUnion;
let groupBy1: (~having: expr=?, expr, selectInUnion) => selectInUnion;
let groupByColumn: (~having: expr=?, string, selectInUnion) => selectInUnion;
let groupByCol: (~having: expr=?, string, selectInUnion) => selectInUnion; // alias
let groupByColumns: (~having: expr=?, list(string), selectInUnion) => selectInUnion;
let groupByCols: (~having: expr=?, list(string), selectInUnion) => selectInUnion; // alias

let union1: (selectInUnion, select) => select;
let union: (selectVariant, select) => select;
let unionAll: (selectVariant, select) => select;
let with_: (tableName, list(columnName), select, select) => select;
let withs: (list((tableName, list(columnName), select)), select) => select;

// let orderByDir: (list(expr), select) => select;

// Apply a limit
let limit: (expr, select) => select;
// Limit to 1 (shorthand)
let limit1: select => select;

// Order by using default direction
let orderBy_: (list(expr), select) => select;

// Order using specific direction(s)
let orderBy: (list((expr, direction)), select) => select;

// Shorthand: Order by a single expression
let orderBy1_: (expr, select) => select;

// Shorthand: Order by a single expression with a direction
let orderBy1: (expr, direction, select) => select;

// Shorthand: Order by two expressions
let orderBy2_: (expr, expr, select) => select;

// Shorthand: Order by two expressions with two directions
let orderBy2: (expr, direction, expr, direction, select) => select;

/***************************
 * INSERT Queries
 ****************************/

// Inserting literal columns/expressions.
let insertColumns: toInsert('r, list((column, list(expr))));
let insertRows: toInsert('r, list(list((column, expr))));
let insertRow: toInsert('r, list((column, expr)));

// Apply a conversion function to create columns and expressions.
let insertRowsWith: (toColumn('a), toExpr('b)) => toInsert('r, list(list(('a, 'b))));
let insertRowWith: ('a => column, 'b => expr) => toInsert('r, list(('a, 'b)));
let insertColumnsWith: (toColumn('a), toExpr('b)) => toInsert('r, list(('a, list('b))));

// Given a function to convert an object to a row, insert one or more objects.
let insertOne: toRow('t) => toInsert('r, 't);
let insertMany: toRow('t) => toInsert('r, list('t));
let returning: ('r, insert('r)) => insert('r);

// Insert with a SELECT query.
let insertSelect: toInsert('r, select);

/*******************************************************************************
 Apply a table-to-query conversion.

 let insertAuthors =
   [("Stephen", "King"), ("Jane", "Austen")]
   |> insertMany(RE.columns2("first", string, "last", string))
   |> into(tname("authors"));
 ********************************************************************************/
let into: (tableName, tableName => insert('r)) => insert('r);

/***************************
  * CREATE TABLE Queries


 [
   cdef("id", Types.int, ~primaryKey=true),
   cdef("first", Types.text),
   cdef("last", Types.text),
 ]
 |> createTable(tname("author"), ~ifNotExists=true)
  ****************************/

// Defining a column
let cdef:
  (
    ~primaryKey: bool=?,
    ~notNull: bool=?,
    ~unique: bool=?,
    ~check: expr=?,
    ~default: expr=?,
    string,
    typeName
  ) =>
  statement;

let constraintName: string => constraintName;

let constraint_: (~a: string=?, tableConstraint) => statement;

// Table-level constraints
let primaryKey: list(columnName) => tableConstraint;
let foreignKey: (columnName, (tableName, columnName)) => tableConstraint;
let unique: list(columnName) => tableConstraint;
let check: expr => tableConstraint;

// Creating a table
let createTable: (~ifNotExists: bool=?, tableName, list(statement)) => createTable;

// Creating a view
let createView: (~ifNotExists: bool=?, tableName, select) => createView;

/************************************
 * Commonly used sql type names
 ***********************************/

module Types: {
  let int: typeName;
  let text: typeName;
};
