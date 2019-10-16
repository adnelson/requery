module S = Sql.Select.New;
module E = Sql.Expression;
module L = Utils.List;
module O = Utils.Option;

module TableName = Sql.TableName;
module ColumnName = Sql.ColumnName;

type columnName = ColumnName.t;
type tableName = TableName.t;
type constraintName = Sql.ConstraintName.t;
type tableConstraint = Sql.CreateTable.tableConstraint;
type column = Sql.Column.t;
type typeName = Sql.TypeName.t;
type target = Sql.Select.New.target;
type select = Sql.Select.New.select;
type expr = Sql.Expression.t;
type aliasedExpr = S.Aliased.t(expr);
type direction = Sql.Select.direction;
type insert('r) = Sql.Insert.t('r);
type statement = Sql.CreateTable.statement;
type createTable = Sql.CreateTable.t;
type createView = Sql.CreateView.t;
type whereClause = S.whereClause;

open S;

let select =
    (
      ~from: option(target)=?,
      ~groupBy: option((list(expr), option(expr)))=?,
      ~where: option(whereClause)=?,
      selections: list(aliasedExpr),
    )
    : S.selectInUnion => {
  selections: L.toArray(selections),
  from,
  groupBy: O.map(groupBy, ((exprs, having)) => (L.toArray(exprs), having)),
  where,
};
let from: (target, list(aliasedExpr)) => selectInUnion =
  (target, exprs) => select(exprs, ~from=target);
let where: (expr, selectInUnion) => selectInUnion =
  (expr, sel) => {...sel, where: Some(Where(expr))};

let select: selectInUnion => select =
  s => {with_: None, select: Select(s), orderBy: None, limit: None};

let union: (selectVariant, select) => select =
  (s, sel) => {...sel, select: Union(s, sel.select)};

let unionAll: (selectVariant, select) => select =
  (s, sel) => {...sel, select: UnionAll(s, sel.select)};

let with_: (TableName.t, list(ColumnName.t), select, select) => select =
  (alias, colNames, aliasedSel, sel) => {
    ...sel,
    with_: Some((alias, L.toArray(colNames), aliasedSel)),
  };
