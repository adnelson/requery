module A = ArrayUtils;
module M = MapUtils;
open Sql;
module QB = QueryBuilder;

exception NoSuchColumn(ColumnName.t);

// TODO any nontrivial function should probably be memoized
module TableTools = {
  include CreateTable;
  let name: t('a) => TableName.t = ({name}) => name;

  // Get the statements of a table
  let statements: CreateTable.t('a) => array(CreateTable.statement('a)) =
    ({statements}) => statements;

  // Get the constraints from a table
  let constraints:
    CreateTable.t('a) => array((option(ConstraintName.t), CreateTable.tableConstraint('a))) =
    tbl =>
      tbl
      ->statements
      ->A.keepMap(
          fun
          | Constraint(name, constraint_) => Some((name, constraint_))
          | _ => None,
        );

  // Get the column definitions from a table
  let columnDefs: t('a) => array(columnDef) =
    ({statements}) =>
      statements->A.keepMap(
        fun
        | ColumnDef(def) => Some(def)
        | _ => None,
      );

  // Get an array of all the column names
  let columnNames: t('a) => array(ColumnName.t) =
    tbl => tbl->columnDefs->A.map(({name}) => name);

  // Get column definitions as a map keyed on column name
  let columnDefMap: t('a) => M.t(ColumnName.t, columnDef) =
    tbl => tbl->columnDefs->A.map(cd => (cd.name, cd))->M.fromArray;

  // Resolve a column name on this table to its definition, returning option
  let getColDef: (t('a), ColumnName.t) => option(CreateTable.columnDef) =
    (t, colName) => t->columnDefMap->M.get(colName);

  // Look up a column on this table by string. If it exists, it's turned into
  // a column name. If not, an exception is raised. This allows a safer way to
  // refer to the column of a table.
  let getCol: (t('a), ColumnName.t) => ColumnName.t =
    (t, colName) => {
      t->columnDefMap->M.has(colName) ? colName : raise(NoSuchColumn(colName));
    };

  // Look up a column on this table by string. If it exists, it's turned into
  // a column name. If not, an exception is raised. This allows a safer way to
  // refer to the column of a table.
  let getColString: (t('a), string) => ColumnName.t =
    (t, name) => {
      let colName = name->ColumnName.fromString;
      t->columnDefMap->M.has(colName) ? colName : raise(NoSuchColumn(colName));
    };

  // Return definitions of the column which forms the primary key.
  // TODO for now, this only works if the table has a single column which has
  // `primaryKey: true`. It does not work with multiple primary keys, or ones
  // created by a standalone `PRIMARY KEY` constraint statement.
  let primaryKeyColumnDef: CreateTable.t(_) => option(CreateTable.columnDef) =
    tbl =>
      switch (tbl->columnDefs->A.keep(cdef => cdef.constraints.primaryKey)) {
      | [|def|] => Some(def)
      | _ => None
      };

  // If there's a single column which is a primary key, return it.
  // Doesn't handle compound keys.
  let primaryKeyColumnDef: t('a) => option(CreateTable.columnDef) =
    tbl =>
      switch (
        tbl->primaryKeyColumnDef,
        tbl
        ->constraints
        ->A.keepMap(
            fun
            | (_, PrimaryKey([|col|])) => tbl->getColDef(col)
            | _ => None,
          ),
      ) {
      | (Some(def), [||]) => Some(def)
      | (None, [|def|]) => Some(def)
      | _ => None
      };

  // Find all of the columns in the table which refer to a foreign key.
  let foreignKeyColumns: t('tref) => M.t(ColumnName.t, ('tref, ColumnName.t)) =
    ({statements}) =>
      statements
      ->A.keepMap(
          fun
          | Constraint(_, ForeignKey(cn, (tref, otherCol), _)) => Some((cn, (tref, otherCol)))
          | _ => None,
        )
      ->M.fromArray;
};

module StatementBuilders = {
  // Given a CreateTable object and a column name, creates a pair of statements,
  // one creating a column of the given name of the type which is the ID type of
  // the foreign table, and another which sets up a foreign key constraint with the
  // given onDelete behavior. The column can be nullable or not.
  let foreignKeyOn = (~notNull=true, ~onDelete=CreateTable.Cascade, ~localColumn, table) => {
    let tableName = table->TableTools.name;
    switch (table->TableTools.primaryKeyColumnDef) {
    | None =>
      Error(
        "Foreign ID column could not be determined on table " ++ tableName->TableName.toString,
      )
    | Some(foreignColumnDef) =>
      Ok(
        QB.[
          cdef(~notNull, localColumn, foreignColumnDef.type_),
          foreignKey(~onDelete, localColumn, (tableName, foreignColumnDef.name))->constraint_,
        ],
      )
    };
  };
};
