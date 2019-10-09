module A = Utils.Array;
module L = Utils.List;
module O = Utils.Option;
module SMap = Belt.Map.String;

module type TableType = {
  type t;
  let fromString: string => t;
  let toString: t => string;
};

module Table: TableType = {
  type t = string;
  external fromString: string => t = "%identity";
  external toString: t => string = "%identity";
};

module type ColumnType = {
  type t;
  let fromString: string => t;
  let fromStringWithTable: (Table.t, string) => t;
  let fromTuples: array((Table.t, string)) => array(t);
  let fromTupleList: list((Table.t, string)) => list(t);
  let fromStringArray: array(string) => array(t);
  let fromStringList: list(string) => list(t);
  let toTuple: t => (option(Table.t), string);
  //let toTupleArray: array((t, 'a)) => array((string, 'a));
  // let toString: t => string;
};

module Column: ColumnType = {
  // e.g. 'foo' or 'mytable.foo'
  type t = (option(Table.t), string);
  let fromString: string => t = c => (None, c);
  let fromStringWithTable: (Table.t, string) => t = (t, c) => (Some(t), c);
  let fromTuples: array((Table.t, string)) => array(t) =
    a => A.map(a, Utils.uncurry(fromStringWithTable));
  let fromTupleList: list((Table.t, string)) => list(t) =
    l => L.map(l, Utils.uncurry(fromStringWithTable));
  let fromStringArray: array(string) => array(t) = a => A.map(a, fromString);
  let fromStringList: list(string) => list(t) = l => L.map(l, fromString);
  //  let fromTupleArray: array((string, 'a)) => array((t, 'a)) = a => A.map(a,
  //  let toTupleArray: array((t, 'a)) => array((string, 'a))
  //    = a => A.map(a, ((c, x)) => (toString(c), x));
  /* let toStrings: t => (string, string) = fun */
  /*   | (None, c) => ("", c) */
  /*   | (Some(t), c) => (Table.toString(t), c); */
  external toTuple: t => (option(Table.t), string) = "%identity";
  //type map('v) = SMap.t('v);
  //external toStringMap: map('v) => SMap.t('v) = "%identity";
  //external fromStringMap: SMap.t('v) => map('v) = "%identity";
  //let mapFromArray = arr => SMap.fromArray(fromTupleArray(arr));
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
    | Typed(t, string)
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
    | Table(Aliased.t(Table.t))
    | SubSelect(t, string)
    | Join(joinType, target, target)

  // Renders into a SELECT query.
  and t = {
    selections: array(Aliased.t(Expression.t)),
    from: option(target),
    groupBy: array(Column.t),
    orderBy: array((Column.t, option(direction))),
    limit: option(int),
    where: option(Expression.t),
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
    | Columns(list(Column.t));

  type t = {
    into: Table.t,
    data,
    returning: option(returning),
  };

  let make = (~returning=?, data, into) => {into, data, returning};
};

type query =
  | Select(Select.t)
  | Insert(Insert.t);
// let renderSelect = Select.render;
