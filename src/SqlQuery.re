module A = Belt.Array;
module O = Belt.Option;

type tableName = string;

module type ColumnType = {
  type t;
  let fromString: string => t;
  let toString: t => string;
};

module Column: ColumnType = {
  // e.g. 'foo' or 'mytable.foo'
  type t = string;
  external fromString: string => t = "%identity";
  external toString: t => string = "%identity";
};

module type AliasedType = {
  type t('a);
  let make: (~a: string=?, 'a) => t('a);
  let toTuple: t('a) => ('a, option(string));
};

module Aliased: AliasedType = {
  type t('a) = ('a, option(string));
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
  type joinType =
    | Inner(Expression.t)
    | Left(Expression.t)
    | Right(Expression.t)
    | Cross;

  // What comes after the FROM of a select.
  type target =
    | TableName(Aliased.t(tableName))
    | SubSelect(t, string)
    | Join(joinType, target, target)

  // Renders into a SELECT query.
  and t = {
    selections: array(Aliased.t(Expression.t)),
    from: option(target),
    groupBy: array(Column.t),
    limit: option(int),
    where: option(Expression.t),
  };
};

// let renderSelect = Select.render;
