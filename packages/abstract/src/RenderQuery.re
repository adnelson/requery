module A = Utils.Array;
module L = Utils.List;
module O = Utils.Option;
module ISet = Belt.Set.Int;
module J = Utils.Json;
module S = Utils.String;

module type SqlRenderingRules = {
  // How to render TRUE and FALSE constants
  let _TRUE: string;
  let _FALSE: string;
  // How to escape a column/table/constraint/etc name to ensure it renders correctly
  let escapeName: string => string;
};

module DefaultRules: SqlRenderingRules = {
  let _TRUE = "TRUE";
  let _FALSE = "FALSE";
  let validReg = {
    let first = "[a-zA-Z_#@]";
    let rest = "[a-zA-Z0-9_$#]*";
    Js.Re.fromString("^" ++ first ++ rest ++ "$");
  };
  let requiresEscape = n => !S.isMatch(n, validReg);
  // Escape double quotes by turning them into double-double quotes.
  let escapeName = n =>
    !requiresEscape(n) ? n : "\"" ++ S.replace(~old="\"", ~new_="\"\"", n) ++ "\"";
};

module WithRenderingRules = (S: SqlRenderingRules) => {
  // Wrap a table/column/etc name in quotes
  module RenderWrapped = (String: Sql.OpaqueString) => {
    type t = String.t;
    let render = s => S.escapeName(String.toString(s));
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
    let renderAtom: atom => string =
      fun
      | Null => "NULL"
      | Column(col) => Column.render(col)
      | Int(i) => string_of_int(i)
      | Float(f) => Js.Float.toString(f)
      // Escape single quotes by replacing them with single quote pairs.
      | String(s) => "'" ++ Utils.String.replace(~old="'", ~new_="''", s) ++ "'"
      | Bool(b) => b ? S._TRUE : S._FALSE;

    // TODO right now we're parenthesizing more than we need to. We could be
    // smarter about this
    let rec render: t => string =
      fun
      | Atom(atom) => renderAtom(atom)
      | Typed(e, t) => renderP(e) ++ "::" ++ TypeName.render(t)
      | Concat(ex1, ex2) => renderP(ex1) ++ " || " ++ renderP(ex2)
      | Between(e, lo, hi) =>
        renderP(e) ++ " BETWEEN " ++ renderP(lo) ++ " AND " ++ renderP(hi)
      | In(ex1, ex2) => renderP(ex1) ++ " IN " ++ renderP(ex2)
      | Add(ex1, ex2) => renderP(ex1) ++ " + " ++ renderP(ex2)
      | Subtract(ex1, ex2) => renderP(ex1) ++ " - " ++ renderP(ex2)
      | Multiply(ex1, ex2) => renderP(ex1) ++ " * " ++ renderP(ex2)
      | Divide(ex1, ex2) => renderP(ex1) ++ " / " ++ renderP(ex2)
      | Eq(ex1, ex2) => renderP(ex1) ++ " = " ++ renderP(ex2)
      | Neq(ex1, ex2) => renderP(ex1) ++ " <> " ++ renderP(ex2)
      | Lt(ex1, ex2) => renderP(ex1) ++ " < " ++ renderP(ex2)
      | Leq(ex1, ex2) => renderP(ex1) ++ " <= " ++ renderP(ex2)
      | Gt(ex1, ex2) => renderP(ex1) ++ " > " ++ renderP(ex2)
      | Geq(ex1, ex2) => renderP(ex1) ++ " >= " ++ renderP(ex2)
      | Like(ex1, ex2) => renderP(ex1) ++ " LIKE " ++ renderP(ex2)
      | And(ex1, ex2) => renderP(ex1) ++ " AND " ++ renderP(ex2)
      | Or(ex1, ex2) => renderP(ex1) ++ " OR " ++ renderP(ex2)
      | IsNull(e) => renderP(e) ++ " IS NULL"
      | IsNotNull(e) => renderP(e) ++ " IS NOT NULL"
      | Not(e) => "NOT " ++ renderP(e)
      | Call(fnName, args) => fnName ++ A.mapJoinCommasParens(args, render)
      | Tuple(exprs) => A.mapJoinCommasParens(exprs, render)
    and renderP =
      fun
      | Atom(_) as e
      | Tuple(_) as e
      | Call(_) as e => render(e)
      | e => "(" ++ render(e) ++ ")";
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

    and renderSelectInUnion: selectInUnion => string =
      ({selections, from, groupBy, where}) => {
        let selections' = A.mapJoinCommas(selections, Aliased.render(Expression.render));
        let from' = O.mapString(from, t => " FROM " ++ renderTarget(t));
        let groupBy' =
          switch (groupBy) {
          | Some((exprs, having)) when A.length(exprs) > 0 =>
            let gb = " GROUP BY " ++ A.mapJoinCommas(exprs, Expression.render);
            gb ++ O.mapWithDefault(having, "", h => " HAVING " ++ Expression.render(h));
          | _ => ""
          };
        let where' =
          O.mapString(
            where,
            fun
            | Where(e) => " WHERE " ++ Expression.render(e)
            | WhereExists(select) => " WHERE EXISTS (" ++ render(select) ++ ")",
          );
        "SELECT " ++ selections' ++ from' ++ where' ++ groupBy';
      }

    and renderSelectVariant =
      fun
      | Select(siu) => renderSelectInUnion(siu)
      | Union(s1, s2) => renderSelectVariant(s1) ++ " UNION " ++ renderSelectVariant(s2)
      | UnionAll(s1, s2) => renderSelectVariant(s1) ++ " UNION ALL " ++ renderSelectVariant(s2)

    and render: t => string =
      ({select, orderBy, limit}) => {
        let orderBy' =
          switch (orderBy) {
          | Some(cols) =>
            A.mapJoinIfNonEmpty(cols, ~prefix=" ORDER BY ", ", ", ((c, optDir)) =>
              Expression.render(c) ++ O.mapString(optDir, dir => " " ++ renderDirection(dir))
            )
          | _ => ""
          };
        let limit' = O.mapString(limit, n => " LIMIT " ++ Expression.render(n));

        "";
      };
  };

  module Insert = {
    open Sql.Insert;
    exception UnequalNumberOfExpressions(list(int));

    let render: ('returning => string, t('returning)) => string =
      (renderReturning, {data, into, returning}) => {
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
        ++ O.mapString(returning, renderReturning);
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
  let insert: ('r => string, Sql.Insert.t('r)) => string = r => Insert.render(r);
  let createTable: Sql.CreateTable.t => string = CreateTable.render;
  let createView: Sql.CreateView.t => string = CreateView.render;
  let render: ('r => string, Sql.query('r)) => string =
    r =>
      fun
      | Select(s) => select(s)
      | Insert(i) => insert(r, i)
      | CreateTable(ct) => createTable(ct)
      | CreateView(cv) => createView(cv);
};

module Default = WithRenderingRules(DefaultRules);
