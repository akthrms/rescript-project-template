open Belt

Js.log("Guess the number!")

let secretNumber = Js.Math.random_int(1, 101)

let rec game = () => {
  Js.log("Please input your guess.")

  // read_line is not implemented
  switch read_line()->String.trim->Int.fromString {
  | None => game()
  | Some(guess) => {
      Js.log(`You guessed: ${Int.toString(guess)}`)

      if guess < secretNumber {
        Js.log("Too small!")
        game()
      } else if guess > secretNumber {
        Js.log("Too big!")
        game()
      } else {
        Js.log("You win!")
      }
    }
  }
}

game()
