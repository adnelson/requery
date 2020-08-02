// A bunch of aliases to commonly used modules. This can be `open`ed
include UtilsPrelude;

module Array = ArrayUtils;
module Dict = DictUtils;
module Json = JsonUtils;
module List = ListUtils;
module Map = MapUtils;
module Option = OptionUtils;
module Promise = PromiseUtils;
module Result = ResultUtils;
module String = StringUtils;

module Abbreviations = {
  module A = ArrayUtils;
  module D = DictUtils;
  module J = JsonUtils;
  module JD = J.Decode;
  module JE = J.Encode;
  module L = ListUtils;
  module M = MapUtils;
  module O = OptionUtils;
  module P = PromiseUtils;
  module R = ResultUtils;
  module S = StringUtils;
};
