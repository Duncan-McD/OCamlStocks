open OUnit2
open Printf
open Cashset

let pp_string s = "\"" ^ s ^ "\""

let function_test_string name str1 str2 =
  name >:: fun _ -> assert_equal ~printer:pp_string str1 str2

let function_test_bool name val1 val2 =
  name >:: fun _ -> assert_equal val1 val2 ~printer:string_of_bool

let scraper_tests = []

let cashset_tests =
  [
    function_test_bool "Common words fail" (Cashset.is_stock_name "I") false;
    function_test_bool "Common words fail" (Cashset.is_stock_name "AND") false;
    function_test_bool "Common words fail" (Cashset.is_stock_name "OR") false;
    function_test_bool "Stock Names return true GME"
      (Cashset.is_stock_name "GME")
      true;
    function_test_bool "Stock Names return true AMC"
      (Cashset.is_stock_name "AMC")
      true;
    function_test_bool "Stock Names return true MKC"
      (Cashset.is_stock_name "MKC")
      true;
    function_test_bool "Stock Namxes return true $ASO"
      (Cashset.is_stock_name "$ASO")
      true;
    function_test_bool "$Stock Names return true $GME"
      (Cashset.is_stock_name "$GME")
      true;
    function_test_bool "$Stock Names return true $AMC"
      (Cashset.is_stock_name "$AMC")
      true;
    function_test_bool "$Stock Names return true $MKC"
      (Cashset.is_stock_name "$MKC")
      true;
    function_test_bool "7000 Stock Names return true ZYXI"
      (Cashset.is_stock_name "ZYXI")
      true;
    function_test_bool "6000 Stock Names return true SPRB"
      (Cashset.is_stock_name "SPRB")
      true;
    function_test_bool "5000 Stock Names return true OSTK"
      (Cashset.is_stock_name "OSTK")
      true;
    function_test_bool "4000 Stock Names return true LXFR"
      (Cashset.is_stock_name "LXFR")
      true;
    function_test_bool "3234 Stock Names return true HOLX"
      (Cashset.is_stock_name "HOLX")
      true;
    function_test_bool "3235 Stock Names return true HOMB"
      (Cashset.is_stock_name "HOMB")
      true;
    function_test_bool "3236 Stock Names return true HOME"
      (Cashset.is_stock_name "HOME")
      true;
  ]

let suite =
  "DEFINITELY NOT A COPY OF A2 (CamelStonks Test Suite)"
  >::: List.flatten [ scraper_tests; cashset_tests ]

let _ = run_test_tt_main suite
