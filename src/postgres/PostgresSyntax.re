// Postgres-specific syntax
module A = ArrayUtils;
module O = Utils.Option;
module S = Utils.String;
module QB = QueryBuilder;

open Sql;

// Expresses the `RETURNING` clause for inserts
module Returning = {
  type t =
    | Columns(array(Column.t));
};

// What do do if there's a conflict while inserting rows
// Subset of the syntax described here:
// https://www.postgresql.org/docs/12/sql-insert.html
module OnConflict = {
  type conflictIndex =
    | IndexColumn(QB.columnName)
    | IndexExpression(QB.expr);

  type conflictTarget = {
    index: option(conflictIndex),
    where: option(QB.expr),
    onConstraint: option(QB.constraintName),
  };

  let makeTarget = (~index=?, ~where=?, ~onConstraint=?, ()) => {index, where, onConstraint};

  // TODO `DO UPDATE` syntax
  type conflictAction =
    | DoNothing;

  type t = {
    target: option(conflictTarget),
    action: conflictAction,
  };

  let make = (~target=?, action) => {target, action};
};

module CreateType = {
  module EnumValue =
    Opaque.String.Make(
      (
        Opaque.String.Validation.MatchRegex({
          let regex = [%re {|/^\w+$/|}];
        })
      ),
      {},
    );

  // Raised in the query builder function. Might be worth
  // moving this logic here
  exception NoValuesInEnum(TypeName.t);

  type typeVariant =
    | Enum(array(EnumValue.t));

  type t = {
    name: TypeName.t,
    variant: typeVariant,
  };

  let makeEnum = (name, values) => {
    switch (values) {
    | [||] => raise(NoValuesInEnum(name))
    | _ => {name, variant: Enum(values)}
    };
  };
};

// Type aliases for postgres-specific queries
type pgEnumValue = CreateType.EnumValue.t;
type pgCreateType = CreateType.t;
type pgReturning = Returning.t;
type pgOnConflict = OnConflict.t;
type pgCreateCustom = CreateType.t;
type pgInsert = Sql.Insert.t(pgReturning, pgOnConflict);
type pgQueryOf('t) = Sql.query(pgReturning, pgOnConflict, pgCreateCustom, 't);
type pgQuery = Sql.query(pgReturning, pgOnConflict, pgCreateCustom, Sql.TableName.t);
