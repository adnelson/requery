module A = Utils.Array;
module L = Utils.List;
module O = Utils.Option;
module ISet = Belt.Set.Int;
module J = Utils.Json;
module S = Utils.String;

module type SqlRenderingRules = {
  let _TRUE: string;
  let _FALSE: string;
  // What character to use to wrap a column name on the left/right
  let _NAME_WRAP_LEFT: string;
  let _NAME_WRAP_RIGHT: string;
  // TODO: text escaping function for column names
};

module DefaultRules: SqlRenderingRules = {
  let _TRUE = "TRUE";
  let _FALSE = "FALSE";
  let _NAME_WRAP_LEFT = "\"";
  let _NAME_WRAP_RIGHT = "\"";
};

module WithRenderingRules = (S: SqlRenderingRules) => {
  // Wrap a table/column/etc name in quotes
  module RenderWrapped = (String: Sql.OpaqueString) => {
    type t = String.t;
    let wrapString = s => S._NAME_WRAP_LEFT ++ s ++ S._NAME_WRAP_RIGHT;
    let render = s => wrapString(String.toString(s));
  };

  module TableName = RenderWrapped(Sql.TableName);
  module ColumnName = RenderWrapped(Sql.ColumnName);
  module ConstraintName = RenderWrapped(Sql.ConstraintName);

  module RenderString = (String: Sql.OpaqueString) => {
    type t = String.t;
    let render = s => String.toString(s);
  };

  module TypeName = RenderString(Sql.TypeName);

  module Column = {
    open Sql.Column;
    let render = c =>
      switch (toTuple(c)) {
      | (None, Named(c)) => ColumnName.render(c)
      | (None, All) => "*"
      | (Some(t), Named(c)) => TableName.render(t) ++ "." ++ ColumnName.render(c)
      | (Some(t), All) => TableName.render(t) ++ ".*"
      };
  };

  module Aliased = {
    open Sql.Aliased;
    let render: ('a => string, t('a)) => string =
      (renderInner, aliased) =>
        switch (toTuple(aliased)) {
        | (x, None) => renderInner(x)
        | (x, Some(alias)) => renderInner(x) ++ " AS " ++ alias
        };
  };

  module Expression = {
    open Sql.Expression;
    // Escape single quotes by replacing them with single quote pairs.
    let escape = Js.String.replaceByRe(Js.Re.fromStringWithFlags("'", ~flags="g"), "''");
    let renderAtom: atom => string =
      fun
      | Null => "NULL"
      | Column(col) => Column.render(col)
      | Int(i) => string_of_int(i)
      | Float(f) => Js.Float.toString(f)
      | String(s) => "'" ++ escape(s) ++ "'"
      | Bool(b) => b ? S._TRUE : S._FALSE;

    // TODO handle precedence here, wrap with parentheses where needed
    let rec render: t => string =
      fun
      | Atom(atom) => renderAtom(atom)
      | Typed(e, t) => render(e) ++ "::" ++ TypeName.render(t)
      | Concat(ex1, ex2) => render(ex1) ++ " || " ++ render(ex2)
      | Add(ex1, ex2) => render(ex1) ++ " + " ++ render(ex2)
      | Subtract(ex1, ex2) => render(ex1) ++ " - " ++ render(ex2)
      | Multiply(ex1, ex2) => render(ex1) ++ " * " ++ render(ex2)
      | Divide(ex1, ex2) => render(ex1) ++ " / " ++ render(ex2)
      | Eq(ex1, ex2) => render(ex1) ++ " = " ++ render(ex2)
      | Neq(ex1, ex2) => render(ex1) ++ " <> " ++ render(ex2)
      | Lt(ex1, ex2) => render(ex1) ++ " < " ++ render(ex2)
      | Leq(ex1, ex2) => render(ex1) ++ " <= " ++ render(ex2)
      | Gt(ex1, ex2) => render(ex1) ++ " > " ++ render(ex2)
      | Geq(ex1, ex2) => render(ex1) ++ " >= " ++ render(ex2)
      | Like(ex1, ex2) => render(ex1) ++ " LIKE " ++ render(ex2)
      | And(ex1, ex2) => render(ex1) ++ " AND " ++ render(ex2)
      | Or(ex1, ex2) => render(ex1) ++ " OR " ++ render(ex2)
      | IsNull(e) => render(e) ++ " IS NULL"
      | IsNotNull(e) => render(e) ++ " IS NOT NULL"
      | Call(fnName, args) => fnName ++ A.mapJoinCommasParens(args, render)
      | Tuple(exprs) => A.mapJoinCommasParens(exprs, render);
  };

  module Select = {
    open Sql.Select;
    let renderJoinType: joinType => (string, option(string)) =
      fun
      | Inner(on) => ("INNER JOIN", Some(Expression.render(on)))
      | Left(on) => ("LEFT OUTER JOIN", Some(Expression.render(on)))
      | Right(on) => ("RIGHT OUTER JOIN", Some(Expression.render(on)))
      | Full(on) => ("FULL JOIN", Some(Expression.render(on)))
      | Cross => ("CROSS JOIN", None);

    let renderDirection: direction => string =
      fun
      | ASC => "ASC"
      | DESC => "DESC";

    let rec renderTarget: target => string =
      fun
      | Table(tname) => Aliased.render(Sql.TableName.toString, tname)
      | SubSelect(q, alias) => "(" ++ render(q) ++ ") AS " ++ alias
      | Join(join, t1, t2) =>
        switch (renderJoinType(join)) {
        | (keyword, None) => renderTarget(t1) ++ " " ++ keyword ++ " " ++ renderTarget(t2)
        | (keyword, Some(on)) =>
          renderTarget(t1) ++ " " ++ keyword ++ " " ++ renderTarget(t2) ++ " ON " ++ on
        }

    and render: t => string =
      ({selections, from, orderBy, groupBy, limit, where}) => {
        let selections = A.mapJoinCommas(selections, Aliased.render(Expression.render));
        let from = O.mapString(from, t => " FROM " ++ renderTarget(t));
        let orderBy =
          A.mapJoinIfNonEmpty(orderBy, ~prefix=" ORDER BY ", ", ", ((c, optDir)) =>
            Expression.render(c) ++ O.mapString(optDir, dir => " " ++ renderDirection(dir))
          );
        let groupBy = A.mapJoinIfNonEmpty(groupBy, ~prefix=" GROUP BY ", ", ", Expression.render);
        let limit = O.mapString(limit, n => " LIMIT " ++ Expression.render(n));
        let where = O.mapString(where, e => " WHERE " ++ Expression.render(e));
        "SELECT " ++ selections ++ from ++ where ++ groupBy ++ orderBy ++ limit;
      };
  };

  module Insert = {
    open Sql.Insert;
    exception UnequalNumberOfExpressions(list(int));

    let render: t => string =
      ({data, into, returning}) => {
        "INSERT INTO "
        ++ TableName.render(into)
        ++ " "
        ++ (
          switch (data) {
          | Values(values) =>
            let cols = A.mapJoinCommasParens(values, v => Column.render(fst(v)));
            let numsOfExprs = ISet.fromArray(A.map(values, ((_, exprs)) => A.length(exprs)));
            switch (ISet.toList(numsOfExprs)) {
            // They must all have the same number of expressions.
            | [count] =>
              // Convert expressions to comma-separated tuples
              let tuples = A.makeBy(count, n => A.map(values, ((_, exprs)) => exprs[n]));
              let valuesStr =
                A.mapJoinCommas(tuples, exprs => A.mapJoinCommasParens(exprs, Expression.render));
              cols ++ " VALUES " ++ valuesStr;
            | counts => raise(UnequalNumberOfExpressions(counts))
            };
          | Select(sel) => Select.render(sel)
          }
        )
        ++ O.mapString(
             returning,
             fun
             | Columns(columns) =>
               " RETURNING " ++ A.mapJoinCommasParens(columns, Column.render),
           );
      };
  };

  module CreateTable = {
    open Sql.CreateTable;
    let renderColumnConstraint = c => {
      let {primaryKey, notNull, unique, check, default} = c;
      Utils.String.(
        joinSpaces(
          A.keepSome(
            O.(
              [|
                someIf(primaryKey, "PRIMARY KEY"),
                someIf(notNull, "NOT NULL"),
                someIf(unique, "UNIQUE"),
                map(check, e => "CHECK " ++ Expression.render(e)),
                map(default, e => "DEFAULT " ++ Expression.render(e)),
              |]
            ),
          ),
        )
      );
    };

    let renderColumnDef = ({name, type_, constraints}) =>
      [|ColumnName.render(name), TypeName.render(type_), constraints |> renderColumnConstraint|]
      |> Utils.String.joinSpaces;

    let renderConstraint: tableConstraint => string =
      fun
      | PrimaryKey(columns) =>
        "PRIMARY KEY " ++ A.mapJoinCommasParens(columns, ColumnName.render)
      | ForeignKey(col, (refTable, refCol)) =>
        "FOREIGN KEY ("
        ++ ColumnName.render(col)
        ++ ") REFERENCES "
        ++ TableName.render(refTable)
        ++ "("
        ++ ColumnName.render(refCol)
        ++ ")"
      | Unique(columns) => "UNIQUE " ++ A.mapJoinCommasParens(columns, ColumnName.render)
      | Check(expr) => "CHECK (" ++ Expression.render(expr) ++ ")";

    let renderStatement: statement => string =
      fun
      | ColumnDef(cdef) => renderColumnDef(cdef)
      | Constraint(None, constraint_) => renderConstraint(constraint_)
      | Constraint(Some(n), constraint_) =>
        "CONSTRAINT " ++ ConstraintName.render(n) ++ " " ++ renderConstraint(constraint_);
    let render: t => string =
      ({name, statements, ifNotExists}) =>
        "CREATE TABLE "
        ++ (ifNotExists ? "IF NOT EXISTS " : "")
        ++ TableName.render(name)
        ++ " "
        ++ A.mapJoinCommasParens(statements, renderStatement);
  };

  module CreateView = {
    open Sql.CreateView;
    let render = ({name, query, ifNotExists}) =>
      "CREATE VIEW "
      ++ (ifNotExists ? "IF NOT EXISTS " : "")
      ++ TableName.render(name)
      ++ " AS "
      ++ Select.render(query);
  };

  let select: Sql.Select.t => string = Select.render;
  let insert: Sql.Insert.t => string = Insert.render;
  let createTable: Sql.CreateTable.t => string = CreateTable.render;
  let createView: Sql.CreateView.t => string = CreateView.render;
  let render: Sql.query => string =
    fun
    | Select(s) => select(s)
    | Insert(i) => insert(i)
    | CreateTable(ct) => createTable(ct)
    | CreateView(cv) => createView(cv);
};

module Default = WithRenderingRules(DefaultRules);
