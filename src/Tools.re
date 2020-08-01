module A = ArrayUtils;
open Sql;

module TableTools = {
  let name: CreateTable.t => TableName.t = ({name}) => name;

  let columnDefs: CreateTable.t => array(CreateTable.columnDef) =
    ({statements}) =>
      statements->A.keepMap(
        fun
        | ColumnDef(def) => Some(def)
        | _ => None,
      );

  let columns: CreateTable.t => array(ColumnName.t) =
    tbl => tbl->columnDefs->A.map(({name}) => name);

  // If there's a single column which is a primary key, return it.
  // Doesn't handle compound keys.
  let primaryKeyColumn: CreateTable.t => option(ColumnName.t) =
    tbl =>
      tbl
      ->columnDefs
      ->A.keepMap(cdef => cdef.constraints.primaryKey ? Some(cdef.name) : None)
      ->A.get(0);
};
