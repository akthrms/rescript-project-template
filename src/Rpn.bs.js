// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Curry = require("rescript/lib/js/curry.js");
var Belt_Int = require("rescript/lib/js/belt_Int.js");
var Belt_List = require("rescript/lib/js/belt_List.js");
var Belt_Array = require("rescript/lib/js/belt_Array.js");
var Caml_int32 = require("rescript/lib/js/caml_int32.js");
var Caml_exceptions = require("rescript/lib/js/caml_exceptions.js");

var InvalidToken = /* @__PURE__ */Caml_exceptions.create("Rpn.InvalidToken");

var InvalidExpression = /* @__PURE__ */Caml_exceptions.create("Rpn.InvalidExpression");

function stringToTokens(string) {
  return Belt_List.fromArray(Belt_Array.map(string.split(" "), (function (ch) {
                    var num = Belt_Int.fromString(ch);
                    if (num !== undefined) {
                      return /* Num */{
                              _0: num
                            };
                    }
                    switch (ch) {
                      case "*" :
                          return /* Mul */2;
                      case "+" :
                          return /* Add */0;
                      case "-" :
                          return /* Sub */1;
                      case "/" :
                          return /* Div */3;
                      default:
                        throw {
                              RE_EXN_ID: InvalidToken,
                              _1: "invalid token: " + ch,
                              Error: new Error()
                            };
                    }
                  })));
}

function calculate(stack, fun) {
  if (stack) {
    var match = stack.tl;
    if (match) {
      return {
              hd: Curry._2(fun, match.hd, stack.hd),
              tl: match.tl
            };
    }
    throw {
          RE_EXN_ID: InvalidExpression,
          _1: "invalid expression",
          Error: new Error()
        };
  }
  throw {
        RE_EXN_ID: InvalidExpression,
        _1: "invalid expression",
        Error: new Error()
      };
}

function evaluateTokens(tokens) {
  var _tokens = tokens;
  var _stack = /* [] */0;
  while(true) {
    var stack = _stack;
    var tokens$1 = _tokens;
    if (tokens$1) {
      var tokens$2 = tokens$1.tl;
      var token = tokens$1.hd;
      if (typeof token === "number") {
        switch (token) {
          case /* Add */0 :
              _stack = calculate(stack, (function (l, r) {
                      return l + r | 0;
                    }));
              _tokens = tokens$2;
              continue ;
          case /* Sub */1 :
              _stack = calculate(stack, (function (l, r) {
                      return l - r | 0;
                    }));
              _tokens = tokens$2;
              continue ;
          case /* Mul */2 :
              _stack = calculate(stack, (function (l, r) {
                      return Math.imul(l, r);
                    }));
              _tokens = tokens$2;
              continue ;
          case /* Div */3 :
              _stack = calculate(stack, Caml_int32.div);
              _tokens = tokens$2;
              continue ;
          
        }
      } else {
        _stack = {
          hd: token._0,
          tl: stack
        };
        _tokens = tokens$2;
        continue ;
      }
    } else {
      if (stack) {
        return stack.hd;
      }
      throw {
            RE_EXN_ID: InvalidExpression,
            _1: "stack is empty",
            Error: new Error()
          };
    }
  };
}

function rpn(string) {
  return evaluateTokens(stringToTokens(string));
}

exports.InvalidToken = InvalidToken;
exports.InvalidExpression = InvalidExpression;
exports.stringToTokens = stringToTokens;
exports.calculate = calculate;
exports.evaluateTokens = evaluateTokens;
exports.rpn = rpn;
/* No side effect */
