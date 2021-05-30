open Belt

let isFizz = n => mod(n, 3) == 0

let isBuzz = n => mod(n, 5) == 0

let fizzBuzz = n =>
  switch (isFizz(n), isBuzz(n), n) {
  | (true, true, _) => "FizzBuzz"
  | (true, _, _) => "Fizz"
  | (_, true, _) => "Buzz"
  | (_, _, n) => Js.Int.toString(n)
  }

Array.range(1, 100)->Array.map(fizzBuzz)->Array.forEach(Js.log)
