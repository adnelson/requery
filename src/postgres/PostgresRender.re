// Rendering custom postgres syntax
include RenderQuery;

// TODO reexamine this whole rules thing
module Rules = Requery.RenderQuery.DefaultRules;
module Render = Requery.RenderQuery.WithRenderingRules(Rules);

include Render;

open PostgresSyntax;

module Returning = {
  let render: Returning.t => string =
    fun
    | Columns(columns) => " RETURNING " ++ A.mapJoinCommasParens(columns, Render.Column.render);
};

module OnConflict = {
  open OnConflict;
  let renderIndex =
    fun
    | IndexColumn(cn) => cn->ColumnName.render
    | IndexExpression(e) => "(" ++ e->Render.Expression.render ++ ")";
  let renderTarget = ({index, where, onConstraint}) =>
    StringUtils.joinSpaces([|
      onConstraint->O.mapString(cn => "ON CONSTRAINT " ++ cn->Render.ConstraintName.render),
      index->O.mapString(renderIndex),
      where->O.mapString(e => "WHERE " ++ e->Render.Expression.render),
    |]);
  let renderAction =
    fun
    | DoNothing => "DO NOTHING";
  let render: t => string =
    ({target, action}) =>
      S.joinSpaces([|"ON CONFLICT", target->O.mapString(renderTarget), action->renderAction|]);
};

module CreateType = {
  open CreateType;
  module EnumValue = {
    include EnumValue;
    // This should be safe because the enum value regex prevents quotes,
    // but if that changes this will need to be revisited.
    let render = ev => "'" ++ ev->EnumValue.toString ++ "'";
  };

  let renderVariant =
    fun
    | Enum(values) => "AS ENUM " ++ values->map(EnumValue.render)->commas->parens;

  let render = ({name, variant}) =>
    [|"CREATE TYPE", name->TypeName.render, variant->renderVariant|]->spaces;
};

let pgRender: Sql.queryRenderer(pgQuery) =
  renderGeneric(
    ~returning=Returning.render,
    ~onConflict=OnConflict.render,
    ~createCustom=CreateType.render,
    ~tableRef=TableName.render,
  );

let pgRenderWithTable: ('t => string) => Sql.queryRenderer(pgQueryOf('t)) =
  tableRef =>
    renderGeneric(
      ~returning=Returning.render,
      ~onConflict=OnConflict.render,
      ~createCustom=CreateType.render,
      ~tableRef,
    );
