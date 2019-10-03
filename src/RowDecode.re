module A = Utils.Array;
module J = Utils.Json.Decode;
type decoder('t) = array(Js.Json.t) => 't;

let dict = (
  ~keyField: string,
  ~keyDecode: J.decoder(string) = J.string,
  ~valueField: string,
  ~valueDecode: J.decoder('a),
): decoder(Js.Dict.t('a)) => rows => {
  let decodeRow: J.decoder((string, 'a)) =
        obj => (obj |> J.field(keyField, keyDecode), obj |> J.field(valueField, valueDecode));
  Js.Dict.fromArray(A.map(rows, decodeRow));
};

let nestedDict = (
  ~outerKeyField: string,
  ~outerKeyDecode: J.decoder(string) = J.string,
  ~innerKeyField: string,
  ~innerKeyDecode: J.decoder(string) = J.string,
  ~valueField: string,
  ~valueDecode: J.decoder('a),
): decoder(Js.Dict.t(Js.Dict.t('a))) => rows => {
     let decodeRow: J.decoder((string, string, 'a)) =
        obj => (
          obj |> J.field(innerKeyField, innerKeyDecode),
          obj |> J.field(outerKeyField, outerKeyDecode),
          obj |> J.field(valueField, valueDecode),
        );
      let result = Js.Dict.empty();
      A.forEach(
        rows,
        row => {
          let (inner, outer, value) = decodeRow(row);
          switch (Js.Dict.get(result, outer)) {
          | None => Js.Dict.set(result, outer, Js.Dict.fromArray([|(inner, value)|]))
          | Some(values) => Js.Dict.set(values, inner, value)
          };
        },
      );
      result;
 }
