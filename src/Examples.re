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

let questionResponses = QueryBuilder.(
   select(
     [e(col("q.id"), ~a="question_id"), e(col("ro.id"), ~a="response_option_id"), e(col("ro.label"))],
     ~from = table("question", ~a="q")
       |> innerJoin(table("response_option", ~a="ro"),
                    eq(col("q.option_set_id"), col("ro.option_set_id")))
   )
 );

 let questionCounts = tbl => QueryBuilder.(
   select(
     [e(col("question_id")), e(col("response_option_id")), e(count(all))],
     ~from=tbl,
     ~groupBy=["question_id", "response_option_id"]
 ));

 let questionHistogram = (id, tbl) => QueryBuilder.(
   select(
     [
       e(col("responses.question_id")),
       e(col("responses.label")),
       e(coalesce(col("count"), bigint(0)), ~a="count")
     ],
     ~from =
       subQuery(questionResponses, "responses")
       |> leftJoin(
         subQuery(questionCounts(tbl), "counts"),
         and_(eq(col("responses.question_id"), col("counts.question_id")),
             eq(col("responses.response_option_id"), col("counts.response_option_id")))),
     ~where = eq(col("responses.question_id"), int(id))
   )
 );

let main = () => {
  Js.log(Q.renderQuery(questionHistogram(1, QueryBuilder.table("single_choice_response"))));
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
