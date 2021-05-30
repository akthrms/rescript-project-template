open Belt

// Types

type token =
  | Add
  | Sub
  | Mul
  | Div
  | Num(int)

// Exceptions

exception InvalidToken(string)

exception InvalidExpression(string)

// Functions

let stringToTokens = string =>
  Js.String.split(" ", string)
  ->Array.map(ch =>
    switch Int.fromString(ch) {
    | None =>
      switch ch {
      | "+" => Add
      | "-" => Sub
      | "*" => Mul
      | "/" => Div
      | _ => raise(InvalidToken(`invalid token: ${ch}`))
      }
    | Some(num) => Num(num)
    }
  )
  ->List.fromArray

let calculate = (stack, fun) => {
  switch stack {
  | list{right, left, ...stack} => list{fun(left, right), ...stack}
  | _ => raise(InvalidExpression("invalid expression"))
  }
}

let evaluateTokens = tokens => {
  let rec go = (tokens, stack) => {
    switch tokens {
    | list{} =>
      switch stack {
      | list{num, ..._} => num
      | _ => raise(InvalidExpression("stack is empty"))
      }
    | list{token, ...tokens} =>
      switch token {
      | Add => go(tokens, calculate(stack, (l, r) => l + r))
      | Sub => go(tokens, calculate(stack, (l, r) => l - r))
      | Mul => go(tokens, calculate(stack, (l, r) => l * r))
      | Div => go(tokens, calculate(stack, (l, r) => l / r))
      | Num(num) => go(tokens, list{num, ...stack})
      }
    }
  }
  go(tokens, list{})
}

let rpn = string => string->stringToTokens->evaluateTokens

rpn("1 2 + +")->Js.log
