// Tools for interacting with tables, abstracting data insertion,
// foreign key references, etc
module QB = QueryBuilder;

// Data to go into a table. A row to put in an INSERT, along with a
// unique string ID which is used to avoid inserting it twice.
module TableData = {
  [@bs.deriving accessors]
  type t = {
    row: QB.row,
    uniqueId: string,
  };

  let make = (uniqueId, row) => {uniqueId, row};

  let toJson = ({row, uniqueId}) =>
    (row, uniqueId) |> Utils.Json.Encode.(object2("row", ToJson.rowToJson, "uniqueId", string));
};

module InsertData = {
  // A grouping of a table along with data to insert into that table, and
  // optionally how to deal with insertion conflicts.
  type t('onConflict) = {
    table: QB.tableName,
    data: list(TableData.t),
    onConflict: option('onConflict),
  };

  let toJson: 'oc. Json.Encode.encoder('oc) => Json.Encode.encoder(t('oc)) =
    (onConflictToJson, {table, data, onConflict}) =>
      Utils.Json.Encode.(
        object_([
          ("table", table |> Sql.TableName.toJson),
          ("data", data |> list(TableData.toJson)),
          ("onConflict", onConflict |> nullable(onConflictToJson)),
        ])
      );

  // Generate an INSERT query which will add all the rows the object needs.
  let makeInsertQuery: 'r 'oc. t('oc) => QB.insert('r, 'oc) =
    ({table, data, onConflict}) =>
      data->Belt.List.map(TableData.row)
      |> QB.insertRows
      |> QB.into(table)
      |> onConflict->Belt.Option.mapWithDefault(i => i, QB.onConflict);

  // Type representing the complete set of inserts needed to record an object
  // and all of its dependencies in the database.
  type closure('oc) = list(t('oc));

  let closureToJson: 'oc. Json.Encode.encoder('oc) => Json.Encode.encoder(closure('oc)) =
    onConflictToJson => Json.Encode.list(toJson(onConflictToJson));

  type idsByTable = JsMap.t(QB.tableName, JsSet.t(string));

  // Generates a list of inserts as well as a map of every ID that was inserted
  // into every table, to avoid redundant insertions.
  let genInserts:
    'r 'oc.
    (~inserted: idsByTable=?, closure('oc)) => (idsByTable, list(QB.insert('r, 'oc)))
   =
    (~inserted=JsMap.empty(), closure) =>
      closure->Belt.List.reduce(
        (inserted, []),
        ((insertedIds, inserts), {table, data} as insertData) => {
          // Get the set of inserted IDs for this table
          let inserted = insertedIds->MapUtils.getOrSetDefaultMut(table, JsSet.empty());
          // Filter to table data which are not in this set
          let newData = data->Belt.List.keep(({uniqueId}) => !inserted->JsSet.has(uniqueId));
          (
            insertedIds,
            switch (newData) {
            | [] => inserts
            | _ =>
              let insert = makeInsertQuery({...insertData, data: newData});
              // Add all the newly inserted data to the cache
              newData->Belt.List.forEach(({uniqueId}) => inserted->JsSet.addMut(uniqueId));
              // return the data to insert
              [insert, ...inserts];
            },
          );
        },
      )
      |> Utils.mapSnd(Belt.List.reverse);
};

// Acts as an abstract interface to a table. The first type parameter is the
// object which can be inserted into the table, and the second is the type
// used for on optional onConflict clause (see the definitions in
// `Sql.CreateTable` and `QueryBuilder.onConflict`).
type t('item, 'onConflict) = {
  table: QB.createTable(QB.tableName),
  idColumn: QB.columnName,
  toUniqueId: 'item => string,
  toSelfDataWithoutId: 'item => list(QB.row),
  toDataBefore: 'item => InsertData.closure('onConflict),
  toDataAfter: 'item => InsertData.closure('onConflict),
};

let make =
    (
      ~table,
      ~idColumn="id"->QB.cname,
      ~toUniqueId,
      ~toSelfDataWithoutId,
      ~toDataBefore=_ => [],
      ~toDataAfter=_ => [],
      (),
    ) => {
  table,
  idColumn,
  toUniqueId,
  toSelfDataWithoutId,
  toDataBefore,
  toDataAfter,
};

// Common case, when there is only one row to insert for a given object
let make1 =
    (~table, ~idColumn=?, ~toUniqueId, ~toSelfRowWithoutId, ~toDataBefore=?, ~toDataAfter=?, ()) =>
  make(
    ~table,
    ~idColumn?,
    ~toUniqueId,
    ~toSelfDataWithoutId=obj => [obj->toSelfRowWithoutId],
    ~toDataBefore?,
    ~toDataAfter?,
    (),
  );

// Get the name of the underlying table
let tableName = ({table}) => table->Tools.TableTools.name;

// Convert to a select target
let toTarget = (interface): QB.target => QB.table(interface->tableName);

// Get the default column name to use for a column referencing this table
let foreignIdColumnName = interface =>
  (interface->tableName->Sql.TableName.toString ++ "_id")->QB.cname;

// Get the complete set of data for an object to be inserted into this table,
// including all of its dependent data.
let getDataClosure: 'a 'oc. (t('a, 'oc), ~onConflict: 'oc=?, 'a) => InsertData.closure('oc) =
  (
    {table, idColumn, toUniqueId, toSelfDataWithoutId, toDataBefore, toDataAfter},
    ~onConflict=?,
    obj,
  ) => {
    let uniqueId = obj->toUniqueId;
    let idField = (idColumn, uniqueId->RowEncode.string);
    obj
    ->toDataBefore
    ->Belt.List.concat([
        {
          table: table->Tools.TableTools.name,
          onConflict,
          data:
            obj
            ->toSelfDataWithoutId
            ->Belt.List.map(row => TableData.make(uniqueId, [idField, ...row])),
        },
      ])
    ->Belt.List.concat(obj->toDataAfter);
  };

let genInsertsForObject:
  'a 'r 'oc.
  (t('a, 'oc), ~inserted: InsertData.idsByTable=?, 'a) => list(QB.insert('r, 'oc))
 =
  (interface, ~inserted=?, obj) =>
    interface->getDataClosure(obj) |> InsertData.genInserts(~inserted?) |> snd;
