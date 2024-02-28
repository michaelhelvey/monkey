let x = fn(x, y) {
  let foo =
}
^ SyntaxError: expected expression found '}'

let y = fn(x, y) {
  let x 2
        ^ SyntaxError: expected assignment, received number
  let y 3
        ^ SyntaxError: etc
}
