open OUnit2

let pp_string s = "\"" ^ s ^ "\""

let function_test_string name func str1 str2 =
  name >:: fun _ -> assert_equal ~printer:pp_string (func str1) str2

let scraper_tests = []

let suite = "test suite for A2" >::: List.flatten [ scraper_tests ]

let _ = run_test_tt_main suite