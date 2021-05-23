module Plot = Owl_plplot.Plot

let f x = Owl.Maths.sin x /. x

let h = Plot.create "plot_003.png"

let () =
  Plot.set_foreground_color h 0 0 0;
  Plot.set_background_color h 255 255 255;
  Plot.set_title h "Function: f(x) = sine x / x";
  Plot.set_xlabel h "x-axis";
  Plot.set_ylabel h "y-axis";
  Plot.set_font_size h 8.;
  Plot.set_pen_size h 3.;
  Plot.plot_fun ~h f 1. 15.;

  Plot.output h

let () = Graphics.open_graph ""

open Images

let img = Images.load "plot_003.png" []

let g = Graphic_image.of_image img

;;
Graphics.draw_image g 0 0

;;
Unix.sleep 10
