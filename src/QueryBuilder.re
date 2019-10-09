open Sql;
module E = Expression;
module L = Utils.List;

type column = Sql.Column.t;
type table = Sql.Table.t;
type target = Sql.Select.target;
type expr = Sql.Expression.t;
type aliasedExpr = Sql.Aliased.t(expr);
type direction = Sql.Select.direction;
type select = Sql.Select.t;
type insert = Sql.Insert.t;
type row = list((column, expr));
type toSelect('t) = 't => select;
type toInsert('t) = ('t, table) => insert;
type toColumn('t) = 't => column;
type toExpr('t) = 't => expr;
type toRow('t) = 't => row;

let typed = (e, t) => E.Typed(e, t);

let null = E.Atom(E.Null);
let nullable = (toExpr, opt) => O.mapWithDefault(opt, null, toExpr);
let int = i => E.Atom(E.Int(i));
let bool = b => E.Atom(E.Bool(b));
let float = f => E.Atom(E.Float(f));
let string = s => E.Atom(E.String(s));
let bigint = i => typed(int(i), "BigInt");
let tuple = exprs => E.Tuple(L.toArray(exprs));

let tbl = Sql.Table.fromString;
let column = Sql.Column.fromString;
let tcolumn = (t, c) => Sql.Column.fromStringWithTable(tbl(t), c);
let columns = Sql.Column.fromStringList;
let tcolumns = l => Sql.Column.fromTupleList(L.map(l, ((t, c)) => (tbl(t), c)));
let col_ = c => E.Atom(E.Column(c));
let col = c => E.Atom(E.Column(column(c)));
let all = col("*");
let allFrom = t => col(t ++ ".*");

let eq = (e1, e2) => E.Eq(e1, e2);
let (==) = eq;
let neq = (e1, e2) => E.Neq(e1, e2);
let (!=) = neq;
let lt = (e1, e2) => E.Lt(e1, e2);
let (<) = lt;
let leq = (e1, e2) => E.Leq(e1, e2);
let (<=) = leq;
let gt = (e1, e2) => E.Gt(e1, e2);
let (>) = gt;
let geq = (e1, e2) => E.Geq(e1, e2);
let (>=) = geq;
let like = (e1, e2) => E.Like(e1, e2);
let and_ = (e1, e2) => E.And(e1, e2);
let (&&) = and_;
let or_ = (e1, e2) => E.Or(e1, e2);
let (||) = or_;
let ands =
  fun
  | [] => bool(true)
  | [expr, ...exprs] => L.reduce(exprs, expr, and_);
let ors =
  fun
  | [] => bool(false)
  | [expr, ...exprs] => L.reduce(exprs, expr, or_);
let isNotNull = e => E.IsNotNull(e);
let isNull = e => E.IsNull(e);

let count = e => E.Call("COUNT", [|e|]);
let distinct = e => E.Call("DISTINCT", [|e|]);
let max = e => E.Call("MAX", [|e|]);
let min = e => E.Call("MIN", [|e|]);
let avg = e => E.Call("AVG", [|e|]);
let sum = e => E.Call("SUM", [|e|]);
let coalesce = (e1, e2) => E.Call("COALESCE", [|e1, e2|]);
let call = (name, args) => E.Call(name, L.toArray(args));

let e = (~a=?, expr): aliasedExpr => Aliased.make(expr, ~a?);

let table = (~a=?, name) => Select.Table(Aliased.make(name, ~a?));
let innerJoin = (t1, on, t2) => Select.(Join(Inner(on), t2, t1));
let leftJoin = (t1, on, t2) => Select.(Join(Left(on), t2, t1));
let rightJoin = (t1, on, t2) => Select.(Join(Right(on), t2, t1));
let fullJoin = (t1, on, t2) => Select.(Join(Full(on), t2, t1));
let crossJoin = (t1, t2) => Select.(Join(Cross, t2, t1));
// TODO this can inspect the type of the select to collapse unnecessary aliases
let selectAs = (alias, select) => Select.SubSelect(select, alias);

let (asc, desc) = Select.(ASC, DESC);

let select = (~from=?, ~groupBy=[], ~orderBy=[], ~limit=?, ~where=?, selections) =>
  Select.{
    selections: L.toArray(selections),
    from,
    limit,
    groupBy: L.toArray(groupBy),
    orderBy: L.toArray(orderBy),
    where,
  };

let as_ = alias =>
  Select.(
    fun
    | Table(tname) => Table(Aliased.as_(tname, alias))
    | SubSelect(q, _) => SubSelect(q, alias)
    | target => SubSelect(select([e(all)], ~from=target), alias)
  );

let selecting = (sels, s) => Select.{...s, selections: L.toArray(sels)};
let from = (t, s) => Select.{...s, from: Some(t)};
let limit = (n, s) => Select.{...s, limit: Some(n)};
let where = (cond, s) => Select.{...s, where: Some(cond)};
let orderBy = (cols, s) =>
  Select.{...s, orderBy: L.amap(cols, ((c, dir)) => (c, Some(dir)))};
let orderBy_ = (cols, s) => Select.{...s, orderBy: L.amap(cols, c => (c, None))};
let orderBy1 = (col, dir, s) => Select.{...s, orderBy: [|(col, Some(dir))|]};
let orderBy1_ = (col, s) => Select.{...s, orderBy: [|(col, None)|]};
let orderBy2 = (col1, dir1, col2, dir2, s) =>
  Select.{...s, orderBy: [|(col1, Some(dir1)), (col2, Some(dir2))|]};
let orderBy2_ = (col1, col2, s) => Select.{...s, orderBy: [|(col1, None), (col2, None)|]};
let groupBy = (cols, s) => Select.{...s, groupBy: L.toArray(cols)};

let convertRow = (toC, toE, (k, v)) => (toC(k), toE(v));
let convertColumn = (toC, toE, (k, vs)) => (toC(k), A.map(L.toArray(vs), toE));
let stringRowWith = (toExpr, row) => L.map(row, convertRow(column, toExpr));
let stringRow = stringRowWith(Utils.id);
let rowFromFields = (fields, obj) =>
  stringRow(L.map(fields, ((field, fn)) => (field, fn(obj))));

let insertColumns = cols =>
  Insert.make(Values(L.toArray(L.map(cols, ((c, exprs)) => (c, L.toArray(exprs))))));

let insertColumnsWith = (toColumn, toExpr, cols) =>
  Insert.make(Values(L.toArray(L.map(cols, convertColumn(toColumn, toExpr)))));

let insertRows = rows =>
  Insert.(make(Values(rowsToColumns(L.toArray(L.map(rows, L.toArray))))));

let insertRowsWith = (toColumn, toExpr, rows) =>
  Insert.(
    make(
      Values(
        rowsToColumns(
          A.map(L.toArray(rows), row => A.map(L.toArray(row), convertRow(toColumn, toExpr))),
        ),
      ),
    )
  );

let insertRow = row => insertRows([row]);
let insertRowWith = (toC, toE, row) => insertRow(L.map(row, convertRow(toC, toE)));
let insertStringRow = insertRowWith(column, Utils.id);
let insertOne = (toRow, obj) => insertRow(toRow(obj));
let insertMany = (toRow, objects) => insertRows(L.map(objects, toRow));
let insertSelect = select => Insert.make(Insert.Select(select));

let returningColumns = (columns, insert) =>
  Insert.{...insert, returning: Some(Columns(columns))};
let returningColumn = (column, insert) =>
  Insert.{...insert, returning: Some(Columns([column]))};
