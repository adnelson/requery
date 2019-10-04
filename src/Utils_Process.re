module O = Belt.Option;

let exit_: int => 'a = [%bs.raw {|code => process.exit(code)|}];

let exit = (~code=?, ~message=?, ()) => {
  let code =
    switch (code, message) {
    | (Some(c), _) => c
    | (None, None) => 0
    | (None, _) => 1
    };
  O.map(message, Js.log) |> ignore;
  exit_(code);
};
