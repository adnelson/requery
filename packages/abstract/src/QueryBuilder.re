open Sql;
module E = Expression;
module L = Utils.List;

type columnName = Sql.ColumnName.t;
type column = Sql.Column.t;
type tableName = Sql.TableName.t;
type constraintName = Sql.ConstraintName.t;
type tableConstraint = Sql.CreateTable.tableConstraint;
type typeName = Sql.TypeName.t;
type target = Sql.Select.target;
type expr = Sql.Expression.t;
type aliasedExpr = Sql.Aliased.t(expr);
type direction = Sql.Select.direction;
type select = Sql.Select.t;
type insert('r) = Sql.Insert.t('r);
type statement = Sql.CreateTable.statement;
type createTable = Sql.CreateTable.t;
type createView = Sql.CreateView.t;
type whereClause = Sql.Select.whereClause;
type row = list((column, expr));
type toSelect('t) = 't => select;
type toInsert('r, 't) = ('t, tableName) => insert('r);
type toColumn('t) = 't => column;
type toExpr('t) = 't => expr;
type toRow('t) = 't => row;

let typeName = Sql.TypeName.fromString;
let typed = (e, t) => E.Typed(e, t);

let null = E.Atom(E.Null);
let nullable = (toExpr, opt) => O.mapWithDefault(opt, null, toExpr);
let int = i => E.Atom(E.Int(i));
let bool = b => E.Atom(E.Bool(b));
let float = f => E.Atom(E.Float(f));
let string = s => E.Atom(E.String(s));
let bigint = i => typed(int(i), typeName("BigInt"));
let tuple = exprs => E.Tuple(L.toArray(exprs));
let tuple2 = (f, g, (a, b)) => tuple([f(a), g(b)]);

let tname = Sql.TableName.fromString;
let cname = Sql.ColumnName.fromString;
let column = Sql.Column.fromString;
let tcolumn = (t, c) => Sql.Column.fromStringWithTable(tname(t), c);
let columns = Sql.Column.fromStringList;
let tcolumns = l => Sql.Column.fromTupleList(L.map(l, ((t, c)) => (tname(t), c)));
let col_ = c => E.Atom(E.Column(c));
let col = c => E.Atom(E.Column(column(c)));
let cols = cs => L.map(cs, col);
let tcol = (t, c) => E.Atom(E.Column(tcolumn(t, c)));
let all = E.(Atom(Column(Sql.Column.all)));
let allFrom = t => E.Atom(Column(Sql.Column.allFrom(tname(t))));

let between = (e, lo, hi) => E.Between(e, lo, hi);
let in_ = (e1, e2) => E.In(e1, e2);
let concat = (e1, e2) => E.Concat(e1, e2);
let (++) = concat;
let add = (e1, e2) => E.Add(e1, e2);
let (+) = add;
let subtract = (e1, e2) => E.Subtract(e1, e2);
let (-) = subtract;
let multiply = (e1, e2) => E.Multiply(e1, e2);
let ( * ) = multiply;
let divide = (e1, e2) => E.Divide(e1, e2);
let (/) = divide;

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
let not = e => E.Not(e);
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

let table = (~a=?, t) => Select.Table(Aliased.make(t, ~a?));
let tableNamed = (~a=?, name) => Select.Table(Aliased.make(tname(name), ~a?));
let innerJoin = (t1, on, t2) => Select.(Join(Inner(on), t2, t1));
let leftJoin = (t1, on, t2) => Select.(Join(Left(on), t2, t1));
let rightJoin = (t1, on, t2) => Select.(Join(Right(on), t2, t1));
let fullJoin = (t1, on, t2) => Select.(Join(Full(on), t2, t1));
let crossJoin = (t1, t2) => Select.(Join(Cross, t2, t1));
// TODO this can inspect the type of the select to collapse unnecessary aliases
let selectAs = (alias, select) => Select.SubSelect(select, alias);

let (asc, desc) = Select.(ASC, DESC);

let select = (~from=?, ~groupBy=([], None), ~orderBy=[], ~limit=?, ~where=?, selections) =>
  Select.{
    selections: L.toArray(selections),
    from,
    limit,
    groupBy:
      switch (groupBy) {
      | ([], _) => ([||], None)
      | (es, h) => (L.toArray(es), h)
      },
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
let selectFrom = (target, exprs) => select(exprs) |> from(target);
let limit = (n, s) => Select.{...s, limit: Some(n)};
let where = (cond, s) => Select.{...s, where: Some(Sql.Select.Where(cond))};
let whereExists = (sel, s) => Select.{...s, where: Some(Sql.Select.WhereExists(sel))};
let orderBy = (exs, s) => Select.{...s, orderBy: L.amap(exs, ((c, dir)) => (c, Some(dir)))};
let orderBy_ = (exs, s) => Select.{...s, orderBy: L.amap(exs, c => (c, None))};
let orderBy1 = (ex, dir, s) => Select.{...s, orderBy: [|(ex, Some(dir))|]};
let orderBy1_ = (ex, s) => Select.{...s, orderBy: [|(ex, None)|]};
let orderBy2 = (ex1, dir1, ex2, dir2, s) =>
  Select.{...s, orderBy: [|(ex1, Some(dir1)), (ex2, Some(dir2))|]};
let orderBy2_ = (ex1, ex2, s) => Select.{...s, orderBy: [|(ex1, None), (ex2, None)|]};
//let orderByCols = orderBy_(column);
let groupBy = (~having=?, cols, s) => Select.{...s, groupBy: (L.toArray(cols), having)};
let groupBy1 = (~having=?, col, s) => Select.{...s, groupBy: ([|col|], having)};
let groupByColumn = (~having=?, c, s) => Select.{...s, groupBy: ([|col(c)|], having)};
let groupByCol = groupByColumn;
let groupByColumns = (~having=?, cols, s) =>
  Select.{...s, groupBy: (L.amap(cols, col), having)};
let groupByCols = groupByColumns;

let convertRow = (toC, toE, (k, v)) => (toC(k), toE(v));
let convertColumn = (toC, toE, (k, vs)) => (toC(k), A.map(L.toArray(vs), toE));

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
let insertOne = (toRow, obj) => insertRow(toRow(obj));
let insertMany = (toRow, objects) => insertRows(L.map(objects, toRow));
let insertSelect = select => Insert.make(Insert.Select(select));

let returning = (returning, insert) => Insert.{...insert, returning: Some(returning)};

let into = (t, f) => f(t);

let cdef =
    (~primaryKey=false, ~notNull=true, ~unique=false, ~check=?, ~default=?, name, type_)
    : statement =>
  CreateTable.(
    ColumnDef({
      name: cname(name),
      type_,
      constraints: {
        primaryKey,
        notNull,
        unique,
        check,
        default,
      },
    })
  );

let constraintName = Sql.ConstraintName.fromString;
let (constraint_, primaryKey, foreignKey, unique, check) = {
  open Sql.CreateTable;
  let constraint_ = (~a=?, c) => Constraint(O.map(a, constraintName), c);
  let pk = cols => PrimaryKey(L.toArray(cols));
  let fk = (col, (refTbl, refCol)) => ForeignKey(col, (refTbl, refCol));
  let u = cols => Unique(L.toArray(cols));
  let c = expr => Check(expr);
  (constraint_, pk, fk, u, c);
};

let createTable = (~ifNotExists=true, name, statements) =>
  Sql.CreateTable.{name, statements: L.toArray(statements), ifNotExists};

let createView = (~ifNotExists=true, name, query) => Sql.CreateView.{name, query, ifNotExists};

module Types = {
  let int = typeName("INTEGER");
  let text = typeName("TEXT");
};

module New = {
  open Sql.Select.New;
  let select_ =
      (
        ~from: option(target)=?,
        ~groupBy: (list(expr), option(expr))=([], None),
        ~where: option(whereClause)=?,
        selections: list(aliasedExpr),
      )
      : selectVariant =>
    Select({
      selections: L.toArray(selections),
      from,
      groupBy: (L.toArray(fst(groupBy)), snd(groupBy)),
      where,
    });
  /*

   let s: selectVariant = select(
     [x,y,z]
     |> from(table("foo"))
     |> where(bar)
   )
   |> union(
     select(
     [x,y,z]
     |> from(table("foobar"))
     |> where(baz)
   ))

   with(tbl("floop"), [a, b, c], s, )

     let rec from = target => fun
       | Select(sel) => Select({...sel, from: Some(target)})
       | Union(s1, s2) => Union(s1 |> from(target), s2 |> from(target))
       | UnionAll(s1, s2) => UnionAll(s1 |> from(target), s2 |> from(target))
   //  let select:
   */
};
