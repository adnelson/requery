include Belt.Option;

let someIf: (bool, 'a) => option('a) = (cond, x) => cond ? Some(x) : None;

// like getWithDefault, but obtains its default thru a lazily-evaluated function.
let getWithDefaultLazy: (option('a), unit => 'a) => 'a =
  (opt, make) =>
    switch (opt) {
    | Some(x) => x
    | None => make()
    };

let mapString: (option('a), 'a => string) => string =
  (opt, f) =>
    switch (opt) {
    | None => ""
    | Some(x) => f(x)
    };
