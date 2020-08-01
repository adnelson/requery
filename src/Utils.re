// This is a kitchen sink for various functions that I've written
// and used in multiple places.
include UtilsPrelude;

module O_ = Belt.Option;
module A = ArrayUtils;

module Dict = {
  include Js.Dict;

  // Map a function over the values in a dict.
  let map: (t('a), 'a => 'b) => t('b) =
    (dict, f) => {
      let entries = entries(dict);
      fromArray(A.map(entries, ((k, v)) => (k, f(v))));
    };

  // Map a function over the key/value pairs in a dict.
  let mapWithKeys: (t('a), (string, 'a) => 'b) => t('b) =
    (dict, f) => {
      let entries = entries(dict);
      fromArray(A.map(entries, ((k, v)) => (k, f(k, v))));
    };

  let fromMap: SMap.t('a) => t('a) = map => fromArray(SMap.toArray(map));
  let toMap: t('a) => SMap.t('a) = dict => SMap.fromArray(entries(dict));

  // Set a key in a dictionary, producing a new dictionary.
  let setPure: (t('a), string, 'a) => t('a) =
    (dict, k, v) => {
      fromArray(A.concat(entries(dict), [|(k, v)|]));
    };

  let singleton: (string, 'a) => t('a) = (k, v) => fromArray([|(k, v)|]);

  let getExn = (d, k) =>
    switch (Js.Dict.get(d, k)) {
    | None => throw("No such key '" ++ k ++ "' in ")
    | Some(v) => v
    };

  // Construct from an array of keys, applying a function to each key.
  let fromKeys = (ks: array(string), f: string => 'a): Js.Dict.t('a) =>
    Js.Dict.fromArray(A.map(ks, k => (k, f(k))));

  // I really shouldn't have to be implementing this myself but ohhhh wellll
  let has = (dict, key) => Belt.Option.isSome(get(dict, key));
};

module String = {
  let length = Js.String.length;
  let contains = (~substring, str) => Js.String.indexOf(substring, str) >= 0;
  let replace = (~old, ~new_, str) =>
    Js.String.replaceByRe(Js.Re.fromStringWithFlags(old, ~flags="g"), new_, str);

  let isMatch: (string, Js.Re.t) => bool = (s, r) => O_.isSome(Js.String.match(r, s));

  // Return the string if condition is true, else empty string.
  let strIf = (cond: bool, s: string) => cond ? s : "";
  let strFrom = (toString: 'a => string, opt: option('a)) =>
    O_.mapWithDefault(opt, "", toString);

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
  let mapJoinCommas = (arr, ~prefix=?, ~suffix=?, f) =>
    mapJoin(arr, ~prefix?, ~suffix?, ", ", f);
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
    let seen = Dict.empty();
    let uniques = [||];
    A.forEach(strings, s =>
      if (!Dict.has(seen, s)) {
        Js.Array.push(s, uniques) |> ignore;
        Dict.set(seen, s, true) |> ignore;
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
};

module Result = {
  include Belt.Result;

  // Map a function over a result, if it's a success.
  let map: ('a => 'b, t('a, 'err)) => t('b, 'err) =
    f =>
      fun
      | Ok(x) => Ok(f(x))
      | Error(e) => Error(e);

  let unwrap: t('a, exn) => 'a =
    fun
    | Ok(x) => x
    | Error(err) => raise(err);

  let unwrapPromise: t('a, exn) => Js.Promise.t('a) =
    fun
    | Ok(x) => Js.Promise.resolve(x)
    | Error(err) => Js.Promise.reject(err);

  let unwrapPromise2: ((t('a, exn), t('b, exn))) => Js.Promise.t(('a, 'b)) =
    fun
    | (Ok(x), Ok(y)) => Js.Promise.resolve((x, y))
    | (Error(err), _) => Js.Promise.reject(err)
    | (_, Error(err)) => Js.Promise.reject(err);

  let mapError = f =>
    fun
    | Ok(_) as result => result
    | Error(e) => Error(f(e));
};

module Log = {
  [@bs.val] external error: 'a => unit = "console.error";
  [@bs.val] external error2: ('a, 'b) => unit = "console.error";
  [@bs.val] external error3: ('a, 'b, 'c) => unit = "console.error";
  [@bs.val] external error4: ('a, 'b, 'c, 'd) => unit = "console.error";
};

module Option = {
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
};

module O = Option;

// Extra functions on lists
module List = {
  include Belt.List;
  let amap: (list('a), 'a => 'b) => array('b) = (l, f) => toArray(map(l, f));
};
