open SqlQuery;
module Render = RenderQuery;
module C = Column;
module E = Expression;
module S = Select;
module Pool = BsPostgres.Pool;
module Result = BsPostgres.Result;
module Query = BsPostgres.Query;
let (query', connect) = BsPostgres.Client.Promise.(query', connect);
let (then_, resolve) = Js.Promise.(then_, resolve);

let exit: int => 'a = [%bs.raw {|code => process.exit(code)|}];

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

let questionResponses =
  QueryBuilder.(
    select(
      [
        e(col("q.id"), ~a="question_id"),
        e(col("ro.id"), ~a="response_option_id"),
        e(col("ro.label")),
      ],
      ~from=
        table("question", ~a="q")
        |> innerJoin(
             table("response_option", ~a="ro"),
             eq(col("q.option_set_id"), col("ro.option_set_id")),
           ),
    )
  );

let questionCounts = tbl =>
  QueryBuilder.(
    select(
      [e(col("question_id")), e(col("response_option_id")), e(count(all))],
      ~from=tbl,
      ~groupBy=["question_id", "response_option_id"],
    )
  );

let questionHistogram = (id, tbl) =>
  QueryBuilder.(
    select(
      [
        e(col("responses.question_id")),
        e(col("responses.label")),
        e(coalesce(col("count"), bigint(0)), ~a="count"),
      ],
      ~from=
        sub(questionResponses, "responses")
        |> leftJoin(
             sub(questionCounts(tbl), "counts"),
             and_(
               eq(col("responses.question_id"), col("counts.question_id")),
               eq(col("responses.response_option_id"), col("counts.response_option_id")),
             ),
           ),
      ~where=eq(col("responses.question_id"), int(id)),
    )
  );

let main = () => {
  let pool = Pool.make(~host="localhost", ~database="sheltie-test", ~port=5999, ());

  let select = questionHistogram(1, QueryBuilder.table("single_choice_response"));
  let rendered = Render.Select.render(select);
  Js.log(rendered ++ ";");

  Pool.Promise.connect(pool)
  |> then_(client =>
      query'(Query.make(~text=rendered, ()), client)
      |> then_((result: Result.t(Js.Json.t)) => {
           let rows = result##rows;
           Js.log(Utils.Json.pretty(rows |> Utils.Encode.(array(json))));
           resolve();
         })
      |> then_(() => Pool.Pool_Client.release(client)))
};

let _ = main() |> then_(_ => exit(0));
