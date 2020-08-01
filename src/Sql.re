module A = ArrayUtils;
module L = ListUtils;
module O = OptionUtils;
module SMap = Belt.Map.String;

// Provides basic validation for SQL identifiers.
// Right now this is overly strict. In reality many more things can be valid
// SQL identifiers, they just might need to be quoted (which RenderQuery
// takes care of)
module IdentifierValidation =
  Opaque.String.Validation.MatchRegex({
    // TODO expand this
    let regex = [%re {|/^[\w\- _#@]+$/|}];
  });

// TODO validation
module DatabaseName =
  Opaque.String.Make(
    IdentifierValidation,
    {},
  );

module TableName =
  Opaque.String.Make(
    IdentifierValidation,
    {},
  );

module ColumnName =
  Opaque.String.Make(
    IdentifierValidation,
    {},
  );

module TypeName =
  Opaque.String.Make(
    (
      Opaque.String.Validation.MatchRegex({
        // TODO expand this
        let regex = [%re {|/^[\w\- _#@]+(\(\d+\))?$/|}];
      })
    ),
    {},
  );

module ConstraintName =
  Opaque.String.Make(
    IdentifierValidation,
    {},
  );

module FunctionName =
  Opaque.String.Make(
    IdentifierValidation,
    {},
  );

module type ColumnType = {
  type col =
    | All
    | Named(ColumnName.t);
  type t;
  let fromString: string => t;
  let fromStringWithTable: (TableName.t, string) => t;
  let fromColumnNameWithTable: (TableName.t, ColumnName.t) => t;
  let all: t;
  let allFrom: TableName.t => t;
  let fromTuples: array((TableName.t, string)) => array(t);
  let fromTupleList: list((TableName.t, string)) => list(t);
  let fromStringArray: array(string) => array(t);
  let fromStringList: list(string) => list(t);
  let toTuple: t => (option(TableName.t), col);
  let colEq: (col, col) => bool;
  let eq: (t, t) => bool;
};

module Column: ColumnType = {
  // `*` or `some_column`
  type col =
    | All
    | Named(ColumnName.t);

  let named: string => col = s => Named(ColumnName.fromString(s));
  let colFromString: string => col =
    fun
    | "*" => All
    | c => named(c);

  // e.g. 'foo' or 'mytable.foo'
  type t = (option(TableName.t), col);
  let fromString: string => t = c => (None, colFromString(c));
  let fromStringWithTable: (TableName.t, string) => t = (t, c) => (Some(t), colFromString(c));
  let all: t = (None, All);
  let allFrom: TableName.t => t = t => (Some(t), All);
  let fromTuples: array((TableName.t, string)) => array(t) =
    a => A.map(a, Utils.uncurry(fromStringWithTable));
  let fromTupleList: list((TableName.t, string)) => list(t) =
    l => L.map(l, Utils.uncurry(fromStringWithTable));
  let fromStringArray: array(string) => array(t) = a => A.map(a, fromString);
  let fromStringList: list(string) => list(t) = l => L.map(l, fromString);
  external toTuple: t => (option(TableName.t), col) = "%identity";
  let fromColumnNameWithTable = (tn, cn) => (Some(tn), Named(cn));

  let colEq = (c1, c2) =>
    switch (c1, c2) {
    | (All, All) => true
    | (Named(cn1), Named(cn2)) => cn1 == cn2
    | _ => false
    };

  let eq = ((t1, c1), (t2, c2)) => t1 == t2 && colEq(c1, c2);
};

module type AliasedType = {
  type t('a);
  let make: (~a: string=?, 'a) => t('a);
  let as_: (t('a), string) => t('a);
  let toTuple: t('a) => ('a, option(string));
  let eq: (('a, 'a) => bool, t('a), t('a)) => bool;
};

module Aliased: AliasedType = {
  // TODO the alias should also be polymorphic. E.g. aliases on expressions
  // become column names, aliases on tables become other table names, etc
  type t('a) = ('a, option(string));
  let as_ = ((x, _), alias) => (x, Some(alias));
  external toTuple: t('a) => ('a, option(string)) = "%identity";
  let make = (~a=?, x) => (x, a);
  let eq = (inner, (x, a), (y, b)) => a == b && inner(x) == inner(y);
};

module Expression = {
  type atom =
    | Null
    | Column(Column.t)
    | Int(int)
    | Float(float)
    | String(string)
    | Bool(bool);

  let atomEq: (atom, atom) => bool =
    (a1, a2) =>
      switch (a1, a2) {
      | (Null, Null) => true
      | (Column(a), Column(b)) => Column.eq(a, b)
      | (Int(a), Int(b)) => a == b
      | (Float(a), Float(b)) => a == b
      | (String(a), String(b)) => a == b
      | (Bool(a), Bool(b)) => a == b
      | _ => false
      };

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
    | Call(FunctionName.t, array(t))
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
    | SubSelect(select, string)
    | Join(joinType, target, target)

  and whereClause =
    | Where(Expression.t)
    | WhereExists(select) // confirm this shouldn't be selectInUnion

  // The parts of a SELECT query which can appear in a UNION.
  and selectInUnion = {
    selections: array(Aliased.t(Expression.t)),
    into: option((TableName.t, option(DatabaseName.t))),
    from: option(target),
    groupBy: option((array(Expression.t), option(Expression.t))),
    where: option(whereClause),
  }

  // Encapsulates SELECTs, WITH clauses, and UNIONs.
  and selectVariant =
    | Select(selectInUnion)
    | Union(selectVariant, selectVariant)
    | UnionAll(selectVariant, selectVariant)

  // Renders into a SELECT query.
  and select = {
    with_: option((TableName.t, array(ColumnName.t), select)),
    select: selectVariant,
    orderBy: option(array((Expression.t, option(direction)))),
    limit: option(Expression.t),
  };

  type t = select;
};

module Insert = {
  type row = array((ColumnName.t, Expression.t));
  type error =
    | EmptyValues
    | MissingColumn(Column.t)
    | RowIsMissingColumn(int, row, int, ColumnName.t);

  exception Error(error);

  // There are two isomorphic representations of values to insert.
  // Row-based values: a nested array of tuples.
  type rowValues = array(row);
  // Column-based values: an array of expressions for each column.
  type columnValues = array((ColumnName.t, array(Expression.t)));

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

  type t('returning, 'onConflict) = {
    into: TableName.t,
    data,
    returning: option('returning),
    onConflict: option('onConflict),
  };

  let make = (~returning=?, ~onConflict=?, data, into) => {into, data, returning, onConflict};
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

  // TODO this is only a prototype
  type onDelete =
    | Cascade
    | SetNull;

  type tableConstraint('tableRef) =
    | PrimaryKey(array(ColumnName.t))
    | ForeignKey(ColumnName.t, ('tableRef, ColumnName.t), option(onDelete))
    | Unique(array(ColumnName.t))
    | Check(Expression.t);

  type statement('tableRef) =
    | ColumnDef(columnDef)
    | Constraint(option(ConstraintName.t), tableConstraint('tableRef));

  // Generic to any table reference
  type t_('tableRef) = {
    name: TableName.t,
    statements: array(statement('tableRef)),
    ifNotExists: bool,
  };

  // Referencing a table by name
  type t = t_(TableName.t);
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

type query('returning, 'onConflict, 'createCustom, 'tableRef) =
  | Select(Select.t)
  | Insert(Insert.t('returning, 'onConflict))
  | CreateTable(CreateTable.t_('tableRef))
  | CreateView(CreateView.t)
  | CreateCustom('createCustom);

type queryRenderer('q) = 'q => string;

// A generic query, should be mostly portable across databases.
type defaultQuery = query(unit, unit, unit, TableName.t);
