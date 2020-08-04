open Sql;
module E = Expression;
module L = ListUtils;

type columnName = ColumnName.t;
type functionName = FunctionName.t;
type column = Column.t;
type tableName = TableName.t;
type aliased('t) = Aliased.t('t);
type constraintName = ConstraintName.t;
type databaseName = DatabaseName.t;
type tableConstraint('tr) = CreateTable.tableConstraint('tr);
type typeName = TypeName.t;
type target = Select.target;
type selectInUnion = Select.selectInUnion;
type selectVariant = Select.selectVariant;
type select = Select.select;

type expr = Expression.t;
type direction = Select.direction;
type insert('r, 'oc) = Insert.t('r, 'oc);
type tableStatement('tr) = CreateTable.statement('tr);
type onDelete = CreateTable.onDelete;
type createTable('tr) = CreateTable.t('tr);
type createView = CreateView.t;
type whereClause = Select.whereClause;
type row = list((columnName, expr));
type toSelect('t) = 't => select;
type toInsert('r, 'oc, 't) = ('t, tableName) => insert('r, 'oc);
type toColumnName('t) = 't => columnName;
type columnDef = CreateTable.columnDef;
type toColumnDef('t) = 't => columnDef;
type toColumnDefs('a) = 'a => list(columnDef);
type toTableStatement('tr, 't) = 't => tableStatement('tr);
type toExpr('t) = 't => expr;
type toRow('t) = 't => row;

let typeName = TypeName.fromString;
let typed = (e, t) => E.Typed(e, t);
type toTypeName('a) = 'a => typeName;

let null = E.Atom(E.Null);
let nullable = (toExpr, opt) => O.mapWithDefault(opt, null, toExpr);
let int = i => E.Atom(E.Int(i));
let bool = b => E.Atom(E.Bool(b));
let float = f => E.Atom(E.Float(f));
let string = s => E.Atom(E.String(s));
let bigint = i => typed(int(i), typeName("BigInt"));
let tuple = exprs => E.Tuple(L.toArray(exprs));
let tuple2 = (f, g, (a, b)) => tuple([f(a), g(b)]);
let tupleOf = (toExpr: toExpr('a), xs) => tuple(L.map(xs, toExpr));

let tname = TableName.fromString;
let cname = ColumnName.fromString;
let cnames = l => L.map(l, ColumnName.fromString);
let fname = FunctionName.fromString;
let column = Column.fromString;
let columnFromName: columnName => column = cn => cn->ColumnName.toString->column;
let tcolumn = (t, c) => Column.fromColumnNameWithTable(t, c);
let columns = Column.fromStringList;
let tcolumns = l => Column.fromTupleList(L.map(l, ((t, c)) => (tname(t), c)));
let col_ = c => E.Atom(E.Column(c));
let col = c => E.Atom(E.Column(columnFromName(c)));
let cols = cs => L.map(cs, col);
let tcol = (t, c) => E.Atom(E.Column(tcolumn(t, c)));
let all = E.(Atom(Column(Column.all)));
let allFrom = t => E.Atom(Column(Column.allFrom(tname(t))));

let between = (e, lo, hi) => E.Between(e, lo, hi);
let in_ = (e1, e2) => E.In(e1, e2);
let concat = (e1, e2) => E.Concat(e1, e2);
let add = (e1, e2) => E.Add(e1, e2);
let subtract = (e1, e2) => E.Subtract(e1, e2);
let multiply = (e1, e2) => E.Multiply(e1, e2);
let divide = (e1, e2) => E.Divide(e1, e2);

let eq = (e1, e2) => E.Eq(e1, e2);
let neq = (e1, e2) => E.Neq(e1, e2);
let lt = (e1, e2) => E.Lt(e1, e2);
let leq = (e1, e2) => E.Leq(e1, e2);
let gt = (e1, e2) => E.Gt(e1, e2);
let geq = (e1, e2) => E.Geq(e1, e2);
let like = (e1, e2) => E.Like(e1, e2);
let and_ = (e1, e2) => E.And(e1, e2);
let or_ = (e1, e2) => E.Or(e1, e2);
let not = e => E.Not(e);
let xor = (e1, e2) => e1 |> and_(not(e2)) |> or_(e2 |> and_(not(e1)));
let ands =
  fun
  | [] => bool(true)
  | [expr, ...exprs] => L.reduce(exprs, expr, and_);
let ors =
  fun
  | [] => bool(false)
  | [expr, ...exprs] => L.reduce(exprs, expr, or_);
let xors =
  fun
  | [] => bool(false)
  | [expr, ...exprs] => L.reduce(exprs, expr, xor);
let isNotNull = e => E.IsNotNull(e);
let isNull = e => E.IsNull(e);

module Op = {
  let (++) = concat;
  let (+) = add;
  let (-) = subtract;
  let ( * ) = multiply;
  let (/) = divide;
  let (==) = eq;
  let (!=) = neq;
  let (<) = lt;
  let (<=) = leq;
  let (>) = gt;
  let (>=) = geq;
  let (&&) = and_;
  let (||) = or_;
};

let count = e => E.Call(fname("COUNT"), [|e|]);
let distinct = e => E.Call(fname("DISTINCT"), [|e|]);
let max = e => E.Call(fname("MAX"), [|e|]);
let min = e => E.Call(fname("MIN"), [|e|]);
let avg = e => E.Call(fname("AVG"), [|e|]);
let sum = e => E.Call(fname("SUM"), [|e|]);
let coalesce = (e1, e2) => E.Call(fname("COALESCE"), [|e1, e2|]);
let call = (name, args) => E.Call(name, L.toArray(args));
let inTuple = (e, es) => in_(e, tuple(es));
let inTupleOf = (e, toExpr, items) => inTuple(e, L.map(items, toExpr));

let e = (~a=?, expr): aliased(expr) => Aliased.make(expr, ~a?);

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

let from: (target, list(aliased(expr))) => selectInUnion =
  (target, exprs) => {
    selections: L.toArray(exprs),
    into: None,
    from: Some(target),
    groupBy: None,
    where: None,
  };

let fromNone = exprs => {
  Select.selections: L.toArray(exprs),
  into: None,
  from: None,
  groupBy: None,
  where: None,
};

let selectInto: (tableName, selectInUnion) => selectInUnion =
  (t, sel) => {...sel, into: Some((t, None))};

let selectIntoIn: (tableName, databaseName, selectInUnion) => selectInUnion =
  (t, db, sel) => {...sel, into: Some((t, Some(db)))};

let where: (expr, selectInUnion) => selectInUnion =
  (expr, sel) => {...sel, where: Some(Where(expr))};

let andWhere: (expr, selectInUnion) => selectInUnion =
  (expr, sel) =>
    sel
    |> (
      switch (sel.where) {
      | Some(Where(cond)) => where(Op.(expr && cond))
      | _ => where(expr)
      }
    );

let orWhere: (expr, selectInUnion) => selectInUnion =
  (expr, sel) =>
    sel
    |> (
      switch (sel.where) {
      | Some(Where(cond)) => where(Op.(expr || cond))
      | _ => where(expr)
      }
    );

let whereExists: (select, selectInUnion) => selectInUnion =
  (exists, sel) => {...sel, where: Some(WhereExists(exists))};

let select = s => {Select.with_: None, select: Select(s), orderBy: None, limit: None};

let selectN = (n, s) => {
  Select.with_: None,
  select: Select(s),
  orderBy: None,
  limit: Some(int(n)),
};

let select1 = selectN(1);

//let union_: (selectInUnion, selectInUnion) => select =
//  (s1, s2) => {with_: None, select: Union(s1, s2)
//
let as_ = alias =>
  Select.(
    fun
    | Table(tname) => Table(Aliased.as_(tname, alias))
    | SubSelect(q, _) => SubSelect(q, alias)
    | target => SubSelect(select([e(all)] |> from(target)), alias)
  );

// Still figuring out the ideal api for this...
let union1: (selectInUnion, select) => select =
  (siu, sel) => {...sel, select: Union(sel.select, Select(siu))};

let union: (selectVariant, select) => select =
  (s, sel) => {...sel, select: Union(sel.select, s)};

let unionAll: (selectVariant, select) => select =
  (s, sel) => {...sel, select: UnionAll(sel.select, s)};

let with_: (TableName.t, list(ColumnName.t), select, select) => select =
  (alias, colNames, aliasedSel, sel) => {
    ...sel,
    with_: Some((alias, L.toArray(colNames), aliasedSel)),
  };

// TODO rewrite tail-recursive
let rec withs = defs =>
  switch (defs) {
  | [] => (select => select)
  | [(t, cols, sel), ...defs] => (select => with_(t, cols, sel, withs(defs, select)))
  };

let orderBy = (exs, s) =>
  Select.{...s, orderBy: Some(L.amap(exs, ((c, dir)) => (c, Some(dir))))};
let orderBy_ = (exs, s) => Select.{...s, orderBy: Some(L.amap(exs, c => (c, None)))};
let orderBy1 = (ex, dir, s) => Select.{...s, orderBy: Some([|(ex, Some(dir))|])};
let orderBy1_ = (ex, s) => Select.{...s, orderBy: Some([|(ex, None)|])};
let orderBy2 = (ex1, dir1, ex2, dir2, s) =>
  Select.{...s, orderBy: Some([|(ex1, Some(dir1)), (ex2, Some(dir2))|])};
let orderBy2_ = (ex1, ex2, s) => Select.{...s, orderBy: Some([|(ex1, None), (ex2, None)|])};
let limit = (n, s) => Select.{...s, limit: Some(n)};
let limit1 = s => Select.{...s, limit: Some(int(1))};

//let orderByCols = orderBy_(column);
let groupBy = (~having=?, cols, s) => Select.{...s, groupBy: Some((L.toArray(cols), having))};
let groupBy1 = (~having=?, col, s) => Select.{...s, groupBy: Some(([|col|], having))};
let groupByColumn = (~having=?, c, s) => Select.{...s, groupBy: Some(([|col(c)|], having))};
let groupByCol = groupByColumn;
let groupByColumns = (~having=?, cols, s) =>
  Select.{...s, groupBy: Some((L.amap(cols, col), having))};
let groupByCols = groupByColumns;

let convertRow = (toC, toE, (k, v)) => (toC(k), toE(v));
let convertColumn = (toC, toE, (k, vs)) => (toC(k), A.map(L.toArray(vs), toE));

let insertColumns = cols =>
  Insert.make(Values(L.toArray(L.map(cols, ((c, exprs)) => (c, L.toArray(exprs))))));

let insertColumnsWith = (toColumnName, toExpr, cols) =>
  Insert.make(Values(L.toArray(L.map(cols, convertColumn(toColumnName, toExpr)))));

let insertRows = rows =>
  Insert.(make(Values(rowsToColumns(L.toArray(L.map(rows, L.toArray))))));

let insertRowsWith = (toColumnName, toExpr, rows) =>
  Insert.(
    make(
      Values(
        rowsToColumns(
          A.map(L.toArray(rows), row =>
            A.map(L.toArray(row), convertRow(toColumnName, toExpr))
          ),
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

let onConflict = (onConflict, insert) => Insert.{...insert, onConflict: Some(onConflict)};

let into = (t, f) => f(t);

let cdef =
    (~primaryKey=false, ~notNull=true, ~unique=false, ~check=?, ~default=?, name, type_)
    : tableStatement('tr) =>
  ColumnDef({
    CreateTable.name,
    type_,
    constraints: {
      primaryKey,
      notNull,
      unique,
      check,
      default,
    },
  });

let nullableCol = (~unique=?, ~check=?, ~default=?) =>
  cdef(~primaryKey=false, ~notNull=false, ~unique?, ~check?, ~default?);

let notNullCol = (~unique=?, ~check=?, ~default=?) =>
  cdef(~primaryKey=false, ~notNull=true, ~unique?, ~check?, ~default?);

let primaryKeyCol = (~check=?, ~default=?) =>
  cdef(~primaryKey=false, ~notNull=true, ~unique=false, ~check?, ~default?);

let constraintName = ConstraintName.fromString;

let constraint_ = (~a=?, c) => CreateTable.(Constraint(a, c));
let primaryKey = cols => CreateTable.PrimaryKey(L.toArray(cols));
let foreignKey = (~onDelete=?, col, (refTbl: 'tr, refCol)): tableConstraint('tr) =>
  CreateTable.ForeignKey(col, (refTbl, refCol), onDelete);
let unique = cols => CreateTable.Unique(L.toArray(cols));
let check = expr => CreateTable.Check(expr);

let primaryKey1 = name => primaryKey([name]);

let createTable =
    (~ifNotExists=true, name, statements: list(tableStatement(tableName)))
    : createTable(tableName) => {
  CreateTable.name,
  statements: L.toArray(statements),
  ifNotExists,
};

let createTableWith = (~ifNotExists=true, name, statements) => {
  CreateTable.name,
  statements: L.toArray(statements),
  ifNotExists,
};

let createView = (~ifNotExists=true, name, query) => CreateView.{name, query, ifNotExists};

module Types = {
  let int = typeName("INTEGER");
  let text = typeName("TEXT");
  let char = len => typeName("CHAR(" ++ string_of_int(len) ++ ")");
};
