(*test different values of [x], [y], [w1], and [w2] on the output of algorithm to see which one results in the "best" weighted list*)
val test_n_per_var :
  int ->
  ((string * float list) * string list) * (float * float * float * float) array

val const_and_stocks_to_buy_array :
  ((string * float list) * string list) * (float * float * float * float) array
