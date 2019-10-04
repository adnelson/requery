module A = Belt.Array;
module O = Belt.Option;

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
  let fromStringArray: array(string) => array(t);
  let fromStringList: list(string) => list(t);
  let toString: t => string;
};

module Column: ColumnType = {
  // e.g. 'foo' or 'mytable.foo'
  type t = string;
  external fromString: string => t = "%identity";
  external fromStringArray: array(string) => array(t) = "%identity";
  external fromStringList: list(string) => list(t) = "%identity";
  external toString: t => string = "%identity";
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
    | Call(string, array(t));
};

module Select = {
  type direction =
    | ASC
    | DESC;
  type joinType =
    | Inner(Expression.t)
    | Left(Expression.t)
    | Right(Expression.t)
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
  type data =
    | Values(array((Column.t, array(Expression.t))))
    | Select(Select.t);

  type t = {
    into: Table.t,
    data,
  };
};

type query =
  | Select(Select.t)
  | Insert(Insert.t);
// let renderSelect = Select.render;
