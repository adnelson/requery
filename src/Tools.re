module A = ArrayUtils;
module M = MapUtils;
open Sql;

exception NoSuchColumn(ColumnName.t);

// TODO any nontrivial function should probably be memoized
module TableTools = {
  include CreateTable;
  let name: t('a) => TableName.t = ({name}) => name;

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

  // Look up a column on this table by string. If it exists, it's turned into
  // a column name. If not, an exception is raised. This allows a safer way to
  // refer to the column of a table.
  let getCol: (t('a), string) => ColumnName.t =
    (t, name) => {
      let colName = name->ColumnName.fromString;
      t->columnDefMap->M.has(colName) ? colName : raise(NoSuchColumn(colName));
    };

  // If there's a single column which is a primary key, return it.
  // Doesn't handle compound keys.
  let primaryKeyColumn: t('a) => option(ColumnName.t) =
    tbl =>
      tbl
      ->columnDefs
      ->A.keepMap(cdef => cdef.constraints.primaryKey ? Some(cdef.name) : None)
      ->A.get(0);

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
