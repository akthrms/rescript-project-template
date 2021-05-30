open Belt

// Types

type token =
  | Add
  | Sub
  | Num(int)

// Exceptions

exception InvalidToken(string)

exception InvalidExpression

// Functions

let stringToTokens = string =>
  Js.String.split(" ", string)
  ->Array.map(ch =>
    switch Int.fromString(ch) {
    | None =>
      if ch == "+" {
        Add
      } else if ch == "-" {
        Sub
      } else {
        raise(InvalidToken(ch))
      }
    | Some(num) => Num(num)
    }
  )
  ->List.fromArray

let calculate = (stack, fun) => {
  switch stack {
  | list{right, left, ...stack} => list{fun(left, right), ...stack}
  | _ => raise(InvalidExpression)
  }
}

let evaluateTokens = tokens => {
  let rec go = (tokens, stack) => {
    switch tokens {
    | list{} =>
      switch stack {
      | list{num, ..._} => num
      | _ => raise(InvalidExpression)
      }
    | list{token, ...tokens} =>
      switch token {
      | Add => go(tokens, calculate(stack, (l, r) => l + r))
      | Sub => go(tokens, calculate(stack, (l, r) => l - r))
      | Num(num) => go(tokens, list{num, ...stack})
      }
    }
  }
  go(tokens, list{})
}

let rpn = string => string->stringToTokens->evaluateTokens
