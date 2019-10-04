open BsMocha.Promise;
open Test_Helpers;

let (resolve, then_) = Js.Promise.(resolve, then_);
let (describe, describe_skip) = BsMocha.Mocha.(describe, describe_skip);

describe("Postgres", () => {
  let client =
    Client.make(
      ~host="localhost",
      ~user="postgres",
      ~database="test_bs_postgres",
      ~port=5432,
      (),
    );

  before(() => client |> setup);
  after(() => client |> teardown);

  describe("Client", () => {
    let _ = it("should perform a query", () =>
              client |> test_query
            );

    it("should perform a query using a query object") @@
    (() => client |> test_query_object);
  });
  describe("Pool", () => {
    let pool =
      Pool.make(
        ~host="localhost",
        ~user="postgres",
        ~database="test_bs_postgres",
        ~port=5432,
        (),
      );

    after(() => pool |> Pool.Promise.end_);

    it("should return an object that can successfully perform queries", () =>
      pool
      |> Pool.Promise.connect
      |> then_(test_query)
      |> then_(test_query_object)
      |> then_(Pool.Pool_Client.release)
    );
  });
});
