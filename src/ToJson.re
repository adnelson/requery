// Convert SQL AST to JSON
// TODO much more to go here!!
open JsonUtils.Encode;

let rowToJson: toJson(QueryBuilder.row) =
  l =>
    l->ListUtils.toArray->ArrayUtils.mapFst(Sql.ColumnName.toString)->Js.Dict.fromArray
    |> dict(e => e->RenderQuery.Default.Expression.render->string);
