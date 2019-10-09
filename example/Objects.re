module A = Utils.Array;
module J = Utils.Json.Decode;
module RD = RowDecode;
module L = Belt.List;
module QB = QueryBuilder;

module Customer = {
  let table = QB.tbl("customer");
  type t = {
    name: string,
    email: string,
    phoneNumber: option(string),
  };

  let make = (~phoneNumber=?, name, email) => {name, email, phoneNumber};

  module Encode = {
    module J = Utils.Json.Encode;

    let toJson: J.encoder(t) =
      customer =>
        J.(
          object_([
            ("name", customer.name |> string),
            ("email", customer.email |> string),
            ("phoneNumber", customer.phoneNumber |> nullable(string)),
          ])
        );

    let toRow: QB.toRow(t) =
      customer =>
        QB.(
          stringRow([
            ("name", customer.name |> string),
            ("email", customer.email |> string),
            ("phone_number", customer.phoneNumber |> nullable(string)),
          ])
        );

    let insert: QB.toInsert(t) = customer => QB.(insertRow(customer |> toRow));
  };
};

module Address = {
  let table = QB.tbl("address");
  type t = string;

  module Encode = {
    let toRow: QB.toRow(t) = address => QB.(stringRow([("contents", address |> string)]));
  };
};
