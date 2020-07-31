// Extending the generic query builder with postgres-specific syntax
open Requery.QueryBuilder;
include PostgresSyntax;

let pgReturning: (array(column), insert(pgReturning, 'a)) => insert(pgReturning, 'a) =
  columns => QB.returning(Returning.Columns(columns));

let pgReturning1: (column, insert(pgReturning, 'a)) => insert(pgReturning, 'a) =
  col => QB.returning(Returning.Columns([|col|]));

let pgEnumValue: string => pgEnumValue = CreateType.EnumValue.fromString;
let pgEnumValues: list(string) => list(pgEnumValue) = vs => vs->Belt.List.map(pgEnumValue);
let pgEnumValuesArray: array(string) => array(pgEnumValue) =
  vs => vs->Belt.Array.map(pgEnumValue);
let pgCreateEnumType: (typeName, list(pgEnumValue)) => pgCreateType =
  (name, values) => CreateType.makeEnum(name, values->Belt.List.toArray);

let pgOnConstraint = cname => OnConflict.{index: None, onConstraint: Some(cname), where: None};

// Attaches `ON CONFLICT DO NOTHING` to an insert
let pgOnConflictNothing: 'r. insert('r, pgOnConflict) => insert('r, pgOnConflict) =
  ins => ins |> onConflict(OnConflict.make(DoNothing));
