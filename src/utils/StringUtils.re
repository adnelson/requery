include UtilsPrelude;
include Js.String;
module O = Belt.Option;
module A = ArrayUtils;

let length = Js.String.length;
let contains = (~substring, str) => Js.String.indexOf(substring, str) >= 0;
let replace = (~old, ~new_, str) =>
  Js.String.replaceByRe(Js.Re.fromStringWithFlags(old, ~flags="g"), new_, str);

let isMatch: (string, Js.Re.t) => bool = (s, r) => O.isSome(Js.String.match(r, s));

// Return the string if condition is true, else empty string.
let strIf = (cond: bool, s: string) => cond ? s : "";
let strFrom = (toString: 'a => string, opt: option('a)) => O.mapWithDefault(opt, "", toString);

// Convert a string to an int, handling failure with an option type.
let parseInt = str =>
  switch (int_of_string(str)) {
  | i => Some(i)
  | exception _ => None
  };

// Convert a string to an float, handling failure with an option type.
let parseFloat = str =>
  switch (float_of_string(str)) {
  | i => Some(i)
  | exception _ => None
  };

// Note: these are duplicates of corresponding functions in the Array module. Can remove those later
let joinWith: (array(string), string) => string = (arr, sep) => Js.Array.joinWith(sep, arr);
let joinSpaces: array(string) => string = arr => joinWith(arr, " ");
let mapJoin: (array('a), ~prefix: string=?, ~suffix: string=?, string, 'a => string) => string =
  (arr, ~prefix="", ~suffix="", sep, f) => prefix ++ joinWith(A.map(arr, f), sep) ++ suffix;
let mapJoinWith: (array('a), string, 'a => string) => string =
  (arr, sep, f) => joinWith(A.map(arr, f), sep);
let mapJoinCommas = (arr, ~prefix=?, ~suffix=?, f) => mapJoin(arr, ~prefix?, ~suffix?, ", ", f);
let mapJoinSpaces = (arr, ~prefix=?, ~suffix=?, f) => mapJoin(arr, ~prefix?, ~suffix?, " ", f);
let mapJoinCommasParens = (arr, f) => mapJoin(arr, ~prefix="(", ~suffix=")", ", ", f);
let mapJoinIfNonEmpty:
  (array('a), ~onEmpty: string=?, ~prefix: string=?, ~suffix: string=?, string, 'a => string) =>
  string =
  (arr, ~onEmpty="", ~prefix="", ~suffix="", sep, f) =>
    switch (arr) {
    | [||] => onEmpty
    | _ => mapJoin(arr, ~prefix, ~suffix, sep, f)
    };

// Deduplicate strings in an array, preserving order.
let dedupeArray = (strings: array(string)): array(string) => {
  // Doing this super imperative style cuz why not
  let seen = Js.Dict.empty();
  let uniques = [||];
  A.forEach(strings, s =>
    if (Js.Dict.get(seen, s)->Belt.Option.isNone) {
      Js.Array.push(s, uniques) |> ignore;
      Js.Dict.set(seen, s, true) |> ignore;
    }
  );
  uniques;
};

// Deduplicate strings in a list, preserving order.
let dedupeList = (strings: list(string)): list(string) => {
  let (_, reversed) =
    Belt.List.reduce(strings, (SSet.empty, []), ((seen, uniques), s) =>
      SSet.has(seen, s) ? (seen, uniques) : (SSet.add(seen, s), Belt.List.add(uniques, s))
    );
  // Since we're pushing to the front of the list, the order will be reversed, so
  // reverse it before returning
  Belt.List.reverse(reversed);
};
