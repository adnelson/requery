module A = Utils.Array;
module L = Utils.List;
module O = Utils.Option;
module SMap = Belt.Map.String;

module type OpaqueString = {
  type t;
  let fromString: string => t;
  let toString: t => string;
};

module MakeOpaqueString = (()) : OpaqueString => {
  type t = string;
  external fromString: string => t = "%identity";
  external toString: t => string = "%identity";
};

module TableName =
  MakeOpaqueString({});
module ColumnName =
  MakeOpaqueString({});
module TypeName =
  MakeOpaqueString({});
module ConstraintName =
  MakeOpaqueString({});

module type ColumnType = {
  type col =
    | All
    | Named(ColumnName.t);
  type t;
  let fromString: string => t;
  let fromStringWithTable: (TableName.t, string) => t;
  let all: t;
  let allFrom: TableName.t => t;
  let fromTuples: array((TableName.t, string)) => array(t);
  let fromTupleList: list((TableName.t, string)) => list(t);
  let fromStringArray: array(string) => array(t);
  let fromStringList: list(string) => list(t);
  let toTuple: t => (option(TableName.t), col);
  //let toTupleArray: array((t, 'a)) => array((string, 'a));
  // let toString: t => string;
};

module Column: ColumnType = {
  module CN = ColumnName;
  // `*` or `some_column`
  type col =
    | All
    | Named(CN.t);

  let named: string => col = s => Named(CN.fromString(s));

  // e.g. 'foo' or 'mytable.foo'
  type t = (option(TableName.t), col);
  let fromString: string => t = c => (None, named(c));
  let fromStringWithTable: (TableName.t, string) => t = (t, c) => (Some(t), named(c));
  let all: t = (None, All);
  let allFrom: TableName.t => t = t => (Some(t), All);
  let fromTuples: array((TableName.t, string)) => array(t) =
    a => A.map(a, Utils.uncurry(fromStringWithTable));
  let fromTupleList: list((TableName.t, string)) => list(t) =
    l => L.map(l, Utils.uncurry(fromStringWithTable));
  let fromStringArray: array(string) => array(t) = a => A.map(a, fromString);
  let fromStringList: list(string) => list(t) = l => L.map(l, fromString);
  external toTuple: t => (option(TableName.t), col) = "%identity";
};

module type AliasedType = {
  type t('a);
  let make: (~a: string=?, 'a) => t('a);
  let as_: (t('a), string) => t('a);
  let toTuple: t('a) => ('a, option(string));
};

module Aliased: AliasedType = {
  type t('a) = ('a, option(string));
  let as_ = ((x, _), alias) => (x, Some(alias));
  external toTuple: t('a) => ('a, option(string)) = "%identity";
  let make = (~a=?, x) => (x, a);
};

module Expression = {
  type atom =
    | Null
    | Column(Column.t)
    | Int(int)
    | Float(float)
    | String(string)
    | Bool(bool);

  type t =
    | Atom(atom)
    | Typed(t, TypeName.t)
    | Between(t, t, t)
    | In(t, t)
    | Concat(t, t)
    | Add(t, t)
    | Subtract(t, t)
    | Multiply(t, t)
    | Divide(t, t)
    | Eq(t, t)
    | Neq(t, t)
    | And(t, t)
    | Leq(t, t)
    | Gt(t, t)
    | Geq(t, t)
    | Or(t, t)
    | Lt(t, t)
    | Like(t, t)
    | IsNull(t)
    | IsNotNull(t)
    | Not(t)
    | Call(string, array(t))
    | Tuple(array(t));
};

module Select = {
  type direction =
    | ASC
    | DESC;
  type joinType =
    | Inner(Expression.t)
    | Left(Expression.t)
    | Right(Expression.t)
    | Full(Expression.t)
    | Cross;

  // What comes after the FROM of a select.
  type target =
    | Table(Aliased.t(TableName.t))
    | SubSelect(t, string)
    | Join(joinType, target, target)

  and whereClause =
    | Where(Expression.t)
    | WhereExists(t)

  // Renders into a SELECT query.
  and t = {
    selections: array(Aliased.t(Expression.t)),
    from: option(target),
    groupBy: (array(Expression.t), option(Expression.t)),
    orderBy: array((Expression.t, option(direction))),
    limit: option(Expression.t),
    where: option(whereClause),
  };
};

module Insert = {
  type row = array((Column.t, Expression.t));
  type error =
    | EmptyValues
    | MissingColumn(Column.t)
    | RowIsMissingColumn(int, row, int, Column.t);

  exception Error(error);

  // There are two isomorphic representations of values to insert.
  // Row-based values: a nested array of tuples.
  type rowValues = array(row);
  // Column-based values: an array of expressions for each column.
  type columnValues = array((Column.t, array(Expression.t)));

  // Since either representation equivalent information, we can
  // translate from one to another.
  let rowsToColumns: rowValues => columnValues =
    rows => {
      switch (A.head(rows)) {
      // Rows can't be empty.
      | None
      | Some([||]) => raise(Error(EmptyValues))
      | Some(row) =>
        // Take each column's value from each row.
        A.mapWithIndex(row, (colIndex, (col, _)) =>
          (
            col,
            A.mapWithIndex(rows, (rowIndex, row) =>
              switch (A.get(row, colIndex)) {
              | Some((c, e)) when c == col => e
              | _ => raise(Error(RowIsMissingColumn(rowIndex, row, colIndex, col)))
              }
            ),
          )
        )
      };
    };

  type data =
    | Values(columnValues)
    | Select(Select.t);

  // TODO the way returning is handled is pretty different for different DBs. We'll
  // eventually want to make this a type parameter.
  type returning =
    | Columns(array(Column.t));

  type t('returning) = {
    into: TableName.t,
    data,
    returning: option('returning),
  };

  let make = (~returning=?, data, into) => {into, data, returning};
};

// CREATE TABLE query
module CreateTable = {
  type columnConstraints = {
    primaryKey: bool,
    notNull: bool,
    unique: bool,
    check: option(Expression.t),
    default: option(Expression.t),
  };

  type columnDef = {
    name: ColumnName.t,
    type_: TypeName.t,
    constraints: columnConstraints,
  };

  let makeColumnDef = (~name, type_, constraints) => {name, type_, constraints};

  type tableConstraint =
    | PrimaryKey(array(ColumnName.t))
    | ForeignKey(ColumnName.t, (TableName.t, ColumnName.t))
    | Unique(array(ColumnName.t))
    | Check(Expression.t);

  type statement =
    | ColumnDef(columnDef)
    | Constraint(option(ConstraintName.t), tableConstraint);

  type t = {
    name: TableName.t,
    statements: array(statement),
    ifNotExists: bool,
  };
};

module CreateView = {
  type t = {
    // Since views act like tables, reuse this type. Eventually we
    // might want to separate them
    name: TableName.t,
    query: Select.t,
    ifNotExists: bool,
  };
};

type query('returning) =
  | Select(Select.t)
  | Insert(Insert.t('returning))
  | CreateTable(CreateTable.t)
  | CreateView(CreateView.t);
// let renderSelect = Select.render;
