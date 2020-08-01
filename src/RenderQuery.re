module A = ArrayUtils;
module L = Utils.List;
module O = Utils.Option;
module ISet = Belt.Set.Int;
module J = JsonUtils;
module S = Utils.String;

let map: 'a 'b. (array('a), 'a => 'b) => array('b) = Belt.Array.map;
let join: array(string) => string = Js.Array.joinWith("");
let rmEmpty: array(string) => array(string) = strs => strs->A.keep(s => s->S.length > 0);
let spaces: array(string) => string = strs => strs->rmEmpty |> Js.Array.joinWith(" ");
let commas: array(string) => string = Js.Array.joinWith(",");
let semis: array(string) => string = Js.Array.joinWith(";");
let parens: string => string = s => "(" ++ s ++ ")";
let brackets: string => string = s => "[" ++ s ++ "]";
let curlies: string => string = s => "{" ++ s ++ "}";

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
  module RenderWrapped = (String: Opaque.String.StringType) => {
    include String;
    let render = s => S.escapeName(String.toString(s));
  };

  module FunctionName = RenderWrapped(Sql.FunctionName);
  module TableName = RenderWrapped(Sql.TableName);
  module ColumnName = RenderWrapped(Sql.ColumnName);
  module ConstraintName = RenderWrapped(Sql.ConstraintName);

  module RenderString = (String: Opaque.String.StringType) => {
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
        // TODO eventually aliases should be typed. For now just wrap
        // them as if they were column names
        | (x, Some(alias)) =>
          renderInner(x) ++ " AS " ++ ColumnName.(render(fromString(alias)))
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
      // A few tricks to simplify generated output
      | Not(IsNotNull(e)) => render(IsNull(e))
      | Not(IsNull(e)) => render(IsNotNull(e))
      | Not(e) => "NOT " ++ renderP(e)
      | Call(fnName, args) =>
        fnName->FunctionName.render ++ A.mapJoinCommas(args, render)->parens
      | Tuple(exprs) => A.mapJoinCommas(exprs, render)->parens
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
      | SubSelect(q, alias) => render(q)->parens ++ " AS " ++ alias
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
            gb ++ having->O.mapString(h => " HAVING " ++ Expression.render(h));
          | _ => ""
          };
        let where' =
          O.mapString(
            where,
            fun
            | Where(e) => " WHERE " ++ Expression.render(e)
            | WhereExists(select) => " WHERE EXISTS " ++ render(select)->parens,
          );
        "SELECT " ++ selections' ++ from' ++ where' ++ groupBy';
      }

    and renderSelectVariant =
      fun
      | Select(siu) => renderSelectInUnion(siu)
      | Union(s1, s2) => renderSelectVariant(s1) ++ " UNION " ++ renderSelectVariant(s2)
      | UnionAll(s1, s2) => renderSelectVariant(s1) ++ " UNION ALL " ++ renderSelectVariant(s2)

    and render: t => string =
      ({with_, select, orderBy, limit}) => {
        let with_' =
          O.mapString(with_, ((table_, columns_, innerSelect)) =>
            " WITH "
            ++ TableName.render(table_)
            ++ A.mapJoinCommasParens(columns_, ColumnName.render)
            ++ " AS "
            ++ render(innerSelect)->parens
          );
        let orderBy' =
          switch (orderBy) {
          | Some(cols) =>
            A.mapJoinIfNonEmpty(cols, ~prefix=" ORDER BY ", ", ", ((c, optDir)) =>
              Expression.render(c) ++ O.mapString(optDir, dir => " " ++ renderDirection(dir))
            )
          | _ => ""
          };
        let limit' = O.mapString(limit, n => " LIMIT " ++ Expression.render(n));

        with_' ++ renderSelectVariant(select) ++ orderBy' ++ limit';
      };
  };

  module Insert = {
    open Sql.Insert;
    exception UnequalNumberOfExpressions(list(int));

    let render =
        (
          ~returning as renderReturning: 'returning => string=_ => "",
          ~onConflict as renderOnConflict: 'onConflict => string=_ => "",
          {data, into, returning, onConflict},
        )
        : string =>
      [|
        "INSERT INTO",
        TableName.render(into),
        switch (data) {
        | Values(values) =>
          let cols = A.mapJoinCommasParens(values, v => ColumnName.render(fst(v)));
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
        },
        onConflict->O.mapString(renderOnConflict),
        returning->O.mapString(renderReturning),
      |]
      ->spaces;
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

    let renderOnDelete =
      fun
      | Cascade => "CASCADE"
      | SetNull => "SET NULL";

    let renderConstraint: ('tr => string, tableConstraint('tr)) => string =
      renderTableRef =>
        fun
        | PrimaryKey(columns) =>
          "PRIMARY KEY " ++ A.mapJoinCommasParens(columns, ColumnName.render)
        | ForeignKey(col, (refTable, refCol), onDelete) =>
          [|
            "FOREIGN KEY",
            ColumnName.render(col)->parens,
            "REFERENCES",
            refTable->renderTableRef,
            ColumnName.render(refCol)->parens,
            onDelete->O.mapString(od => "ON DELETE " ++ od->renderOnDelete),
          |]
          ->spaces
        | Unique(columns) => "UNIQUE " ++ columns->A.map(ColumnName.render)->commas->parens
        | Check(expr) => "CHECK " ++ expr->Expression.render->parens;

    let renderStatement: ('tr => string, statement('tr)) => string =
      renderTableRef =>
        fun
        | ColumnDef(cdef) => renderColumnDef(cdef)
        | Constraint(None, constraint_) => constraint_ |> renderConstraint(renderTableRef)
        | Constraint(Some(n), constraint_) =>
          [|
            "CONSTRAINT",
            ConstraintName.render(n),
            constraint_ |> renderConstraint(renderTableRef),
          |]
          ->spaces;

    let renderWith: ('tr => string, t_('tr)) => string =
      (renderTableRef, {name, statements, ifNotExists}) =>
        [|
          "CREATE TABLE",
          ifNotExists ? "IF NOT EXISTS" : "",
          TableName.render(name),
          " ",
          A.mapJoinCommasParens(statements, renderStatement(renderTableRef)),
        |]
        ->spaces;

    // Common case: referencing tables by name
    let render: t => string = renderWith(TableName.toString);
  };

  module CreateView = {
    open Sql.CreateView;
    let render = ({name, query}) =>
      "CREATE VIEW "
      // TODO sqlite and postgres have different ways of rendering this.
      // SQLite uses `IF NOT EXISTS` while postgres uses `OR REPLACE`
      // ++ (ifNotExists ? "IF NOT EXISTS " : "")
      ++ TableName.render(name)
      ++ " AS "
      ++ Select.render(query);
  };

  let select: Sql.Select.t => string = Select.render;
  let insert:
    (~returning: 'r => string=?, ~onConflict: 'oc => string=?, Sql.Insert.t('r, 'oc)) => string = Insert.render;
  // Supply a custom renderer to
  let createTableWith: 'tr. (~tableRef: 'tr => string, Sql.CreateTable.t_('tr)) => string =
    (~tableRef) => CreateTable.renderWith(tableRef);
  let createTable: Sql.CreateTable.t => string = createTableWith(~tableRef=TableName.render);
  let createView: Sql.CreateView.t => string = CreateView.render;
  let renderGeneric:
    'r 'c 'c 'tr.
    (
      ~returning: 'r => string,
      ~onConflict: 'oc => string,
      ~createCustom: 'c => string,
      ~tableRef: 'tr => string,
      Sql.query('r, 'oc, 'c, 'tr)
    ) =>
    string
   =
    (~returning, ~onConflict, ~createCustom, ~tableRef) =>
      fun
      | Select(s) => s |> select
      | Insert(i) => i |> insert(~returning, ~onConflict)
      | CreateTable(ct) => ct |> createTableWith(~tableRef)
      | CreateView(cv) => cv |> createView
      | CreateCustom(c) => c |> createCustom;
};

module Default = WithRenderingRules(DefaultRules);

let renderDefault: Sql.queryRenderer(Sql.defaultQuery) =
  Default.renderGeneric(
    ~returning=_ => "",
    ~onConflict=_ => "",
    ~createCustom=_ => "",
    ~tableRef=_ => "",
  );
