module Queries = {
  open Requery.QueryBuilder;
  let booksByAuthor = (authorId: int): select =>
    select([
      e(
        tcol("authors", "first_name") ++ string(" ") ++ tcol("authors", "last_name"),
        ~a="name",
      ),
      e(tcol("books", "title")),
    ])
    |> from(
         tableNamed("authors")
         |> innerJoin(tableNamed("books"), tcol("authors", "id") == tcol("books", "author_id")),
       )
    |> where(tcol("authors", "id") == int(authorId));

  let bookCountByAuthor = (authorId: int): select =>
    select([e(col("name")), e(count(all))])
    |> from(booksByAuthor(authorId) |> selectAs("t"))
    |> groupBy1(column("name"));
};

let argv = Node.Process.argv;
if (Belt.Array.some(argv, s => s == "--counts")) {
  Js.log(Postgres.Render.select(Queries.bookCountByAuthor(2)));
} else {
  Js.log(Postgres.Render.select(Queries.booksByAuthor(2)));
};
