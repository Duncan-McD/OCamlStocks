let do_daily_optimize = false (* TODO: implement optimization *)

let static_consts = (1., 1., 1., 1.) (* TODO: replace with static constants from uniform testing *)

let daily_consts () = failwith "unimplemented daily optimization"

let constants () = if do_daily_optimize then daily_consts () else static_consts