(*Uniform_tesing will test different values of [x], [y], [w1], and [w2] 
on the output of algorithm to see which one results in the
 "best" weighted list*)

val initialize_testing_portfolios : User.t -> User.t
(**[initialize_testing_portfolios u] is the user with 
newly initialized testing portfolios*)

val optimized_constants : User.t -> float * float * float * float
(**[optimized_constants u] is the tuple of constants of the portfolio
 with the greatest net worth*)
