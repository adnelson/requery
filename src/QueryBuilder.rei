type columnName = Sql.ColumnName.t;
type databaseName = Sql.DatabaseName.t;
type tableName = Sql.TableName.t;
type functionName = Sql.FunctionName.t;
type aliased('t) = Sql.Aliased.t('t);
type constraintName = Sql.ConstraintName.t;
type tableConstraint('tr) = Sql.CreateTable.tableConstraint('tr);
type column = Sql.Column.t;
type typeName = Sql.TypeName.t;
type target = Sql.Select.target;
type selectInUnion = Sql.Select.selectInUnion;
type selectVariant = Sql.Select.selectVariant;
type select = Sql.Select.select;
type expr = Sql.Expression.t;
type direction = Sql.Select.direction;
type insert('r, 'oc) = Sql.Insert.t('r, 'oc);
type tableStatement('tr) = Sql.CreateTable.statement('tr);
type createTable('tr) = Sql.CreateTable.t('tr);
type createView = Sql.CreateView.t;
type whereClause = Sql.Select.whereClause;
type onDelete = Sql.CreateTable.onDelete;
type columnDef = Sql.CreateTable.columnDef;

/****************************
 * Encoder types
 ***************************/

type row = list((columnName, expr));
type toSelect('t) = 't => select;
type toInsert('r, 'oc, 't) = ('t, tableName) => insert('r, 'oc);
type toColumnName('t) = 't => columnName;
type toExpr('t) = 't => expr;
type toRow('t) = 't => row;
type toColumnDef('t) = 't => columnDef;
type toColumnDefs('a) = 'a => list(columnDef);
type toTableStatement('tr, 't) = 't => tableStatement('tr);
type toTypeName('a) = 'a => typeName;

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

// A single column, from a columnName. Recall that columns and columnNames
// differ in that the former can be `*` and can be prefixed with a table
// name, while columnNames are just wrapped strings.
let col: columnName => expr;

// Multiple columns
let cols: list(columnName) => list(expr);

// A single column, from a column name
let col_: column => expr;

// A single column from a table name and column name
let tcol: (tableName, columnName) => expr;

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
// To make a `columnName` use `cname`.
let column: string => column;

// Make a `column` object from a table name and column name.
let tcolumn: (tableName, columnName) => column;

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

// Boolean operators
let and_: (expr, expr) => expr;
let or_: (expr, expr) => expr;
let xor: (expr, expr) => expr;

// AND all of the expressions in the list (true if empty)
let ands: list(expr) => expr;

// OR all of the expressions in the list (false if empty)
let ors: list(expr) => expr;

// XOR all of the expressions in the list (false if empty)
let xors: list(expr) => expr;

let not: expr => expr;

// Symbolic versions of binary operators. Put into their own module
// because they clash with operators from pervasives.
module Op: {
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
};

// Functions
let fname: string => functionName;
let count: expr => expr;
let distinct: expr => expr;
let max: expr => expr;
let min: expr => expr;
let sum: expr => expr;
let avg: expr => expr;
let coalesce: (expr, expr) => expr;
let call: (functionName, list(expr)) => expr;

/*********** Higher order functions **********/

// null if the value is None, else convert the Some value.
let nullable: ('t => expr, option('t)) => expr;

// Convert to a tuple.
let tuple2: (toExpr('a), toExpr('b)) => toExpr(('a, 'b));

// Convert a list to a tuple, given a way to convert each item in the list.
let tupleOf: toExpr('a) => toExpr(list('a));

// Check if the first argument is a member of the second argument tuple.
let inTuple: (expr, list(expr)) => expr;

// Check if the first argument is a member of the second argument tuple, after converting the tuple.
let inTupleOf: (expr, toExpr('a), list('a)) => expr;

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

// Adds an `INTO` clause to the select, inserting the result into another table
// within the same database. To specify a different database, use `selectIntoIn`.
let selectInto: (tableName, selectInUnion) => selectInUnion;

// Like `selectInto` but specifies a different database to put the table in.
let selectIntoIn: (tableName, databaseName, selectInUnion) => selectInUnion;

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
let groupByColumn: (~having: expr=?, columnName, selectInUnion) => selectInUnion;
let groupByCol: (~having: expr=?, columnName, selectInUnion) => selectInUnion; // alias
let groupByColumns: (~having: expr=?, list(columnName), selectInUnion) => selectInUnion;
let groupByCols: (~having: expr=?, list(columnName), selectInUnion) => selectInUnion; // alias

let union1: (selectInUnion, select) => select;
let union: (selectVariant, select) => select;
let unionAll: (selectVariant, select) => select;
let with_: (tableName, list(columnName), select, select) => select;
let withs: (list((tableName, list(columnName), select)), select) => select;

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
let insertColumns: toInsert('r, 'oc, list((columnName, list(expr))));
let insertRows: toInsert('r, 'oc, list(list((columnName, expr))));
let insertRow: toInsert('r, 'oc, list((columnName, expr)));

// Apply a conversion function to create columns and expressions.
let insertRowsWith:
  (toColumnName('a), toExpr('b)) => toInsert('r, 'oc, list(list(('a, 'b))));
let insertRowWith: (toColumnName('a), 'b => expr) => toInsert('r, 'oc, list(('a, 'b)));
let insertColumnsWith:
  (toColumnName('a), toExpr('b)) => toInsert('r, 'oc, list(('a, list('b))));

// Given a function to convert an object to a row, insert one or more objects.
let insertOne: toRow('t) => toInsert('r, 'oc, 't);
let insertMany: toRow('t) => toInsert('r, 'oc, list('t));

// Add a `RETURNING` clause to an `INSERT` statement (for supported syntax)
let returning: ('r, insert('r, 'oc)) => insert('r, 'oc);

// Add an `ON CONFLICT` clause to an `INSERT` statement (for supported syntax)
let onConflict: ('oc, insert('r, 'oc)) => insert('r, 'oc);

// Insert with a SELECT query.
let insertSelect: toInsert('r, 'oc, select);

/*******************************************************************************
 Apply a table-to-query conversion.

 let insertAuthors =
   [("Stephen", "King"), ("Jane", "Austen")]
   |> insertMany(RE.columns2("first", string, "last", string))
   |> into(tname("authors"));
 ********************************************************************************/
let into: (tableName, tableName => insert('r, 'oc)) => insert('r, 'oc);

/***************************
 * CREATE TABLE Queries
 *
 * Made up of some number of "statements", including
   * Column definitions (`cdef`)
   * Constraint definitions (`constraint_`)

 [
   cdef(cname("id"), Types.int, ~primaryKey=true),
   cdef(cname("first"), Types.text),
   cdef(cname("last"), Types.text),
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
    columnName,
    typeName
  ) =>
  tableStatement('tr);

let nullableCol:
  (~unique: bool=?, ~check: expr=?, ~default: expr=?, columnName, typeName) => tableStatement('tr);

let notNullCol:
  (~unique: bool=?, ~check: expr=?, ~default: expr=?, columnName, typeName) => tableStatement('tr);

let primaryKeyCol:
  (~check: expr=?, ~default: expr=?, columnName, typeName) => tableStatement('tr);

let constraintName: string => constraintName;

// Define a single constraint as a statement.
let constraint_: (~a: constraintName=?, tableConstraint('tr)) => tableStatement('tr);

// Table-level constraints
let primaryKey: list(columnName) => tableConstraint('tr);

let primaryKey1: columnName => tableConstraint('tr);

let foreignKey: (~onDelete: onDelete=?, columnName, ('tr, columnName)) => tableConstraint('tr);

let unique: list(columnName) => tableConstraint('tr);
let check: expr => tableConstraint('tr);

// Creating a table
let createTable:
  (~ifNotExists: bool=?, tableName, list(tableStatement(tableName))) => createTable(tableName);

// Creating a table with a custom reference type
let createTableWith:
  (~ifNotExists: bool=?, tableName, list(tableStatement('tr))) => createTable('tr);

// Creating a view
let createView: (~ifNotExists: bool=?, tableName, select) => createView;

/************************************
 * Commonly used sql type names
 ***********************************/

module Types: {
  let int: typeName;
  let text: typeName;
  let char: int => typeName;
};
