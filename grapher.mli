val graph_net_worth : User.t -> unit
(** [graph_net_worth u] graphs the net worth of user u in a side by side scatter
    plot and piecewise/line function*)

val graph_net_worth_and_liquidity : User.t -> unit
(** [graph_net_worth_and_liquidity u] graphs the net worth of user u added to
    their liquidity in a side by side scatter plot and piecewise/line function*)

val graph_stock_value : User.t -> string -> unit
(** [graph_stock_value u t] graphs the evaluation of a stock with ticker t in a 
    the past portfolios of user u on a side by side scatter plot and 
    piecewise/line function*)
