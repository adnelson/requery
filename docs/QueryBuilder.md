# QueryBuilder

The query builder is used to construct SQL queries programatically. This provides
a toolkit layer on top of the abstract types defined in the `Sql` module, making
it easy to generate queries that are readable and behave as you expect.

## INSERT Queries

These functions will build INSERT queries. An insert falls into one of two
categories: inserting items with a SELECT query, or inserting literal values.
In the first case it's just a matter of passing in a SELECT query:

```reason
let manySidedShapes = QB.(
  tbl("shapes_with_many_sides")
  |> insertSelect(
    select([e(col("name")), e((col("sides")))])
    |> from("shapes")
    |> where(col("sides") > int(5))
   )
);
```

In the second case there are several ways to approach it.
The simplest is to use literal values. For example, to insert a single row:

```reason
let insertSquare = QB.(
  tbl("polygons")
  |> insertRow([
    ("name", string("square")),
    ("sides", int(4)),
  ])
);
```

To insert multiple rows, specifying column names each time:

```reason
let insertSquareAndTriangle = QB.(
  tbl("polygons")
  |> insertRows([
    [
      ("name", string("square")),
      ("sides", int(4)),
    ],
    [
      ("name", string("triangle")),
      ("sides", int(3)),
    ]
  ])
);
```

To insert multiple columns, with a list of values for each (which
is rendered to exactly the same query as the previous example):

```reason
let insertSquareAndTriangle = QB.(
  tbl("polygons")
  |> insertRows([
    ("name", [string("square"), string("triangle")]),
    ("sides", [int(4), int(3)]),
  ])
);
```

### Writing generic INSERT queries with encoders

Since of course in most cases you'll need to choose which values are
being inserted programmatically. A straightforward, but somewhat limited,
way to accomplish this is by turning the above literals into functions:

```reason
let insertShape = (name: string, sides: int) => QB.(
  tbl("polygons")
  |> insertRow([
    ("name", string(name)),
    ("sides", int(sides)),
  ])
);

let insertShapes = (shapes: list(string, int)) => QB.(
  tbl("polygons")
  |> insertColumns([
    ("name", Belt.List.map(shapes, fst)),
    ("sides", Belt.List.map(shapes, snd)),
  ])
);
```

This will work OK, but it starts to get verbose and unweildy. A more robust
solution is to think in terms of *encoding* an object into a row.

```reason
type shape = {
  name: string,
  sides: int
};

let toRow: QB.toRow(shape) = shape => QB.([
  ("name", shape.name |> string),
  ("sides", shape.sides |> string),
]);
```

Using this, we can write code using clear, domain-specific objects.

```reason
// Use `insertOne` to insert a single `shape` object
let insertTriangle = QB.(
  tbl("shapes")
  |> insertOne(toRow, { name: "triangle", sides: 3 })
);

// Use `insertMany` to insert a list of `shape` objects
let insertSquareAndPentagon = QB.(
  tbl("shapes")
  |> insertMany(toRow, [
      { name: "square", sides: 4 },
      { name: "pentagon", sides: 5 },
  ])
);
```

One advantage of this is that it allows you to whatever type makes sense for your program, or even change your the type without needing to change your database tables.


```reason
// We decided we only wanted to support these three shapes in our app.
type shape =
  | Triangle
  | Square
  | Pentagon;

let name = fun
  | Triangle => "triangle"
  | Square => "square"
  | Pentagon => "pentagon";

let sides = fun
  | Triangle => 3
  | Square => 4
  | Pentagon => 5;

// Different implementation, but it renders to exactly the same row as before!
let toRow: QB.toRow(shape) = shape => QB.([
  ("name", shape |> name |> string),
  ("sides", shape |> sides |> string),
]);
```

In the above example, the way `shape` is defined is totally different,
but since the insertion code we has just calls `toRow`, it will
continue to work as it always did.

### Returning values from an INSERT

An insert with a `returning` clause can be paired with a *decoder* to retrieve
information back from the insertion result.
