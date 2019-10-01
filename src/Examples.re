open SqlQuery;
module C = Column;
module E = Expression;
module J = Join;
module Q = Query;

/*
 SELECT q.id AS question_id, ro.label, count FROM question AS q
 INNER JOIN response_option AS ro ON q.option_set_id = ro.option_set_id
 LEFT OUTER JOIN (
   SELECT count(*), question_id, response_option_id
   FROM single_choice_response
   GROUP BY question_id, response_option_id
 ) AS counts ON counts.question_id = q.question_id and
    counts.response_option_id = ro.id
 WHERE q.id = 1;
 */
let countsQuery: Query.t =
  Query.(
    make(
      A.map(
        [|
          E.Call("COUNT", [|E.col("*")|]),
          E.col("question_id"),
          E.col("response_option_id"),
        |],
        select,
      ),
      ~from=table("single_choice_response"),
      ~groupBy=A.map([|"question_id", "response_option_id"|], Column.make),
    )
  );

let mainQuery: Query.t =
  Query.(
    make(
      A.map([|E.col("id", ~t="q"), E.col("label", ~t="ro"), E.col("count")|], select),
      ~from=
        leftJoin(
          innerJoin(
            table("question", ~a="q"),
            table("response_option", ~a="ro"),
            E.(Eq(col(~t="q", "option_set_id"), col(~t="ro", "option_set_id"))),
          ),
          SubQuery(countsQuery, "counts"),
          E.(
            And(
              Eq(col(~t="counts", "question_id"), col(~t="q", "id")),
              Eq(col(~t="counts", "response_option_id"), col(~t="ro", "id")),
            )
          ),
        ),
    )
  );

 // Would be best to have a ppx!

let questionResponses = QueryBuilder.(
   select(
     [e(col("q.id"), ~a="question_id"), e(col("ro.label"))],
     ~from = table("question", ~a="q")
       |> innerJoin(table("response_option", ~a="ro"),
                    eq(col("q.response_option_id"), col("ro.id")))
   )
 );

 let questionCounts = tbl => QueryBuilder.(
   select(
     [e(col("question")), e(col("response_option_id")), e(count(all))],
     ~from=tbl,
     ~groupBy=["question", "response_option_id"]
 ));

 let questionHistogram = (id, tbl) => QueryBuilder.(
   select(
     [
       e(col("r.question_id")),
       e(col("ro.label")),
       e(coalesce(col("count"), bigint(0)), ~a="count")
     ],
     ~from =
       subQuery(questionResponses, "r")
       |> leftJoin(
         subQuery(questionCounts(tbl), "counts"),
         and_(eq(col("r.question_id"), col("counts.question_id")),
             eq(col("r.response_option_id"), col("counts.response_option_id")))),
     ~where = eq(col("question_id"), int(id))
   )
 );

let main = () => {
  Js.log(Q.renderQuery(questionHistogram(123, QueryBuilder.table("single_choice_response"))));
};

let _ = main();

/*

 let main = () => {
   getConnection()
   |> then_(conn => {
    execute(conn, questionHistogram(123, QueryBuilder.table("single_choice_response")))
    |> then_(decode(decodeHistogram))
    |> then_(histogram => Js.log(histogram))
    |> then_(_ => close(conn))
    |> catch(_ => close(conn))
 */
