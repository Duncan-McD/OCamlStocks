open Random

let printff a = print_float a

let examplesumfunction w =
  let x = Random.float 1. in
  let y = Random.float 1. in
  let z = Random.float 1. in
  let a = ref 0. in
  for i = 1 to 100 do
    a :=
      !a
      +. (float_of_int i ** x)
         *. (float_of_int i ** (1. /. y))
         *. (float_of_int i ** (1. /. z))
  done;
  print_float (w *. !a)

let examplesumfunction_n_times n =
  for i = 1 to n do
    examplesumfunction 1.
  done
