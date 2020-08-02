open QueryBuilder;
open Jest;
open Expect;

module OperatorTests = {
  describe("boolean operator tests", () => {
    test("xor", () => {
      expect(bool(true)->xor(bool(false))->RenderQuery.Default.Expression.render)
      ->toMatchSnapshot();
      expect(col(cname("foobar"))->xor(bool(false))->RenderQuery.Default.Expression.render)
      ->toMatchSnapshot();
    })
  });
};
