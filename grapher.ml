module Plot = Owl_plplot.Plot

let rec get_list_of_net_worths portfolios net_worths =
  match portfolios with
  | [] -> List.rev net_worths
  | h :: t -> get_list_of_net_worths t (Portfolio.net_worth h :: net_worths)

let rec get_list_of_timestamps portfolios timestamps =
  match portfolios with
  | [] -> List.rev timestamps
  | h :: t -> get_list_of_net_worths t (Portfolio.net_worth h :: timestamps)

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

let f x = Owl.Maths.sin x

let h = Plot.create ~m:1 ~n:2 "plot.png"

let x = Owl.Mat.of_array (Array.of_list [ 3.; 4.; 5. ]) 1 3

let y = Owl.Mat.of_array (Array.of_list [ 3.; 4.; 5. ]) 1 3

let () =
  Plot.set_foreground_color h 0 0 0;
  Plot.set_background_color h 255 255 219;
  Plot.set_title h "Function: f(x) = sine x / x";
  Plot.set_xlabel h "time";
  Plot.set_ylabel h "y-axis";
  Plot.set_font_size h 4.;
  Plot.set_pen_size h 3.;
  Plot.subplot h 0 0;
  Plot.plot_fun ~h f 1. 15.;
  Plot.subplot h 0 1;
  Plot.set_xrange h 0. 8.;
  Plot.set_yrange h 0. 8.;
  Plot.scatter ~h x y;

  Plot.output h

let () =
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

let () =
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
(*
open Images

let img = Images.load "plot_003.png" []

let g = Graphic_image.of_image img

let () =
  Graphics.draw_image g 0 0;
  Unix.sleep 10*)
