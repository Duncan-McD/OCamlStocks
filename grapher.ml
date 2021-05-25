module Plot = Owl_plplot.Plot

let rec get_list_of_net_worths portfolios net_worths =
  match portfolios with
  | [] -> List.rev net_worths
  | h :: t -> get_list_of_net_worths t (Portfolio.net_worth h :: net_worths)

let rec get_list_of_timestamps portfolios timestamps =
  match portfolios with
  | [] -> List.rev timestamps
  | h :: t -> get_list_of_timestamps t (Portfolio.net_worth h :: timestamps)

let rec get_list_of_liquidities portfolios liquidities =
  match portfolios with
  | [] -> List.rev liquidities
  | h :: t -> get_list_of_liquidities t (Portfolio.liquidity h :: liquidities)

let rec get_list_of_values ticker portfolios values =
  match portfolios with
  | [] -> List.rev values
  | h :: t -> (
      let s = Portfolio.stock_from_ticker h ticker in
      match s with
      | Some stock ->
          get_list_of_values ticker t (Portfolio.value stock :: values)
      | None -> failwith "impossible")

let make_net_worth_and_liquidity_matrices user =
  let portfolios = User.current_portfolio user :: User.past_portfolios user in
  let timestamps = get_list_of_timestamps portfolios [] in
  let net_worths = get_list_of_net_worths portfolios [] in
  let liquidities = get_list_of_liquidities portfolios [] in
  let liquidity_and_net_worth = List.combine liquidities net_worths in
  let liquidities_plus_net_worths =
    List.map (fun t -> fst t +. snd t) liquidity_and_net_worth
  in
  let times = List.length timestamps in
  let liqs_plus_nets = List.length liquidities_plus_net_worths in
  let timestamp_matrix =
    Owl.Mat.of_array (timestamps |> Array.of_list) 1 times
  in
  let liquidity_net_worth_matrix =
    Owl.Mat.of_array
      (liquidities_plus_net_worths |> Array.of_list)
      1 liqs_plus_nets
  in
  (timestamp_matrix, liquidity_net_worth_matrix)

let make_net_worth_matrices user =
  let portfolios = User.current_portfolio user :: User.past_portfolios user in
  let timestamps = get_list_of_timestamps portfolios [] in
  let net_worths = get_list_of_net_worths portfolios [] in
  let times = List.length timestamps in
  let nets = List.length net_worths in
  let timestamp_matrix =
    Owl.Mat.of_array (timestamps |> Array.of_list) 1 times
  in
  let net_worth_matrix =
    Owl.Mat.of_array (net_worths |> Array.of_list) 1 nets
  in
  (timestamp_matrix, net_worth_matrix)

let rec get_list_of_portfolios_containing_a_stock ticker portfolios
    portfolios_with_stock =
  match portfolios with
  | [] -> List.rev portfolios_with_stock
  | h :: t -> (
      let s = Portfolio.stock_from_ticker h ticker in
      match s with
      | None -> List.rev portfolios
      | Some s ->
          get_list_of_portfolios_containing_a_stock ticker t
            (h :: portfolios_with_stock))

let make_stock_value_matrices ticker user =
  let portfolios = User.current_portfolio user :: User.past_portfolios user in
  let portfolios =
    get_list_of_portfolios_containing_a_stock ticker portfolios []
  in
  let timestamps = get_list_of_timestamps portfolios [] in
  let values = get_list_of_values ticker portfolios [] in
  let times = List.length timestamps in
  let vals = List.length values in
  let timestamp_matrix =
    Owl.Mat.of_array (timestamps |> Array.of_list) 1 times
  in
  let value_matrix = Owl.Mat.of_array (values |> Array.of_list) 1 vals in
  (timestamp_matrix, value_matrix)

let rgba_to_rgb () =
  Py.initialize ();
  ignore
    (Py.Run.eval ~start:Py.File
       {|

import PIL.Image


rgba_image = PIL.Image.open('plot.png')
rgb_image = rgba_image.convert('RGB')
rgb_image.save('plot.png')|});
  Py.finalize ();
  ()

let open_plot () =
  Graphics.open_graph "";
  let img = Images.load "plot.png" [] in
  let x = fst (Images.size img) in
  let y = snd (Images.size img) in
  Graphics.resize_window x y;
  let g = Graphic_image.of_image img in
  Graphics.draw_image g 0 0;
  Graphics.set_window_title "Plot One";
  print_endline "Enter Y to end";
  if read_line () = "Y" then Graphics.clear_graph () else ()

let make_function coordinates =
  let x1 = coordinates |> fst |> fst in
  let y1 = coordinates |> fst |> snd in
  let x2 = coordinates |> snd |> fst in
  let y2 = coordinates |> snd |> snd in
  let rise = y2 -. y1 in
  let run = x2 -. x1 in
  let slope = rise /. run in
  fun x -> (slope *. (x -. x1)) +. y1

let rec make_functions_for_piecewise coordinates functions =
  match coordinates with
  | [ h ] -> List.rev functions
  | h1 :: h2 :: t ->
      make_functions_for_piecewise (h2 :: t)
        (make_function (h1, h2) :: functions)
  | _ -> failwith "impossible"

let rec find_location x xs function_number =
  match xs with
  | [] -> function_number
  | h :: t ->
      if x <= h then function_number else find_location x t (function_number + 1)

let rec get_last l =
  match l with [ h ] -> h | h :: t -> get_last t | _ -> failwith "impossible"

let rec get_list_index l n =
  if n <= 1 then List.hd l else get_list_index (List.tl l) (n - 1)

let piece_wise_function xs ys x =
  let coordinates = List.combine xs ys in
  let functions = make_functions_for_piecewise coordinates [] in
  let function_to_evaluate = find_location x xs 0 in
  if function_to_evaluate <= 0 then List.hd ys
  else if function_to_evaluate >= List.length functions then get_last ys
  else (get_list_index functions function_to_evaluate) x

let rec find_maximum floats max =
  match floats with
  | [] -> max
  | h :: t -> find_maximum t (if h > max then h else max)

let rec find_minimum floats max =
  match floats with
  | [] -> max
  | h :: t -> find_maximum t (if h < max then h else max)

let convert_seconds_days s = s /. (24. *. 60. *. 60.)

let get_max_min user xs ys xs_list ys_list =
  let max_y = find_maximum ys_list (-1. *. Float.max_float) in
  let min_y = find_minimum ys_list Float.max_float in
  let max_xs = find_maximum xs_list (-1. *. Float.max_float) in
  let initial_x = User.account_creation_time user in
  let max_x_relatice_to_initial = max_xs -. initial_x in
  let max_x_days = convert_seconds_days max_x_relatice_to_initial in
  let initial_x_days = 0. in
  let maxx = max_x_days +. (max_x_days *. 0.05) in
  let minx = initial_x_days -. (max_x_days *. 0.05) in
  let maxy = max_y +. ((max_y -. min_y) *. 0.05) in
  let miny = max_y -. ((max_y -. min_y) *. 0.05) in
  ((minx, maxx), (miny, maxy))

let create_and_open_side_by_side name maxs_and_mins x_matrix y_matrix x_list
    y_list =
  let h = Plot.create ~m:1 ~n:2 "plot.png" in
  let maxx = maxs_and_mins |> fst |> snd in
  let minx = maxs_and_mins |> fst |> fst in
  let maxy = maxs_and_mins |> snd |> snd in
  let miny = maxs_and_mins |> snd |> fst in
  Plot.set_foreground_color h 0 0 0;
  Plot.set_background_color h 30 255 219;
  Plot.set_title h ("Net Worth of " ^ name);
  Plot.set_xlabel h "Time (days since account creation)";
  Plot.set_ylabel h "Net Worth";
  Plot.set_font_size h 4.;
  Plot.set_pen_size h 3.;
  Plot.subplot h 0 0;
  Plot.plot_fun ~h (piece_wise_function x_list y_list) minx maxx;
  Plot.subplot h 0 1;
  Plot.set_xrange h minx maxx;
  Plot.set_yrange h miny maxy;
  Plot.scatter ~h x_matrix y_matrix;
  Plot.output h;
  rgba_to_rgb ();
  open_plot ()

let graph_net_worth user =
  let name = User.name user in
  let matrices = make_net_worth_matrices user in
  let times = fst matrices in
  let net_worths = snd matrices in
  let xs_list = Array.to_list (Owl.Mat.to_array times) in
  let ys_list = Array.to_list (Owl.Mat.to_array net_worths) in
  let maxs_and_mins = get_max_min user times net_worths xs_list ys_list in
  create_and_open_side_by_side name maxs_and_mins times net_worths xs_list
    ys_list

let graph_net_worth_and_liquidity user =
  let name = User.name user in
  let matrices = make_net_worth_and_liquidity_matrices user in
  let times = fst matrices in
  let net_worths_plus_liquidities = snd matrices in
  let xs_list = Array.to_list (Owl.Mat.to_array times) in
  let ys_list = Array.to_list (Owl.Mat.to_array net_worths_plus_liquidities) in
  let maxs_and_mins =
    get_max_min user times net_worths_plus_liquidities xs_list ys_list
  in
  create_and_open_side_by_side name maxs_and_mins times
    net_worths_plus_liquidities xs_list ys_list

let graph_stock_value user ticker =
  let name = User.name user in
  let matrices = make_stock_value_matrices ticker user in
  let times = fst matrices in
  let values = snd matrices in
  let xs_list = Array.to_list (Owl.Mat.to_array times) in
  let ys_list = Array.to_list (Owl.Mat.to_array values) in
  let maxs_and_mins = get_max_min user times values xs_list ys_list in
  create_and_open_side_by_side name maxs_and_mins times values xs_list ys_list
