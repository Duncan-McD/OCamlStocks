open OUnit2
open Printf
open Cashset
open Scraper

let pp_string s = "\"" ^ s ^ "\""

let pp_list pp_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [ h ] -> acc ^ pp_elt h
      | h1 :: (h2 :: t as t') ->
          if n = 100 then acc ^ "..." (* stop printing long list *)
          else loop (n + 1) (acc ^ pp_elt h1 ^ "; ") t'
    in
    loop 0 "" lst
  in
  "[" ^ pp_elts lst ^ "]"

let function_test_string name str1 str2 =
  name >:: fun _ -> assert_equal ~printer:pp_string str1 str2

let function_test_bool name val1 val2 =
  name >:: fun _ -> assert_equal val1 val2 ~printer:string_of_bool

let scraper_test name ?(amount = 100) json_file attr_func expec_attr =
  let scraped_body =
    match posts (scrape_json ~amount json_file) with
    | last_post :: t -> attr_func last_post
    | [] -> failwith "No posts scraped"
  in
  name >:: fun _ -> assert_equal expec_attr scraped_body

let scraper_tests =
  [
    scraper_test "First post body" ~amount:1 "testing_files/stocksnew.json" body
      "[The panic of 1901](https://en.wikipedia.org/wiki/Panic_of_1901) was \
       caused by a standoff between J.P. Morgan/James Hill and E. H. Harriman \
       for control over Northern Pacific Railways. Trying to take control of \
       the company, both parties combined bought over 94% of the float. At the \
       same time, other investors were heavily shorting the stock (to sell to \
       one of the parties), until at one point everyone realized that they \
       won't be able to cover their short position because Morgan/Hill and \
       Harriman won't sell their shares. The shorts were squeezed as they \
       fought for the remaining 6% of the shares and drove NP from $20 to \
       $240. To pay for the high covering share prices, the shorts liquidated \
       their other positions, causing mass panic that rippled through the \
       whole stock market. Overwhelming cries of \"sell! sell! sell!\" were \
       heard at the NYSE trading floor. Temporary relief were provided when \
       rumors of Morgan's broker's death were spread, but quickly dissipated \
       and the panic resumed. The whole ordeal concluded only when the two \
       parties reached a truce and the shorts were finally able to cover their \
       positions.";
    scraper_test "Second post body" ~amount:2 "testing_files/stocksnew.json"
      body
      "What's going on gamblers? Well looks like we're back to discussing \
       value stocks with solid fundamentals that are undervalued and being hit \
       hard by shorts. Similar to the original GME in it's inception but \
       obviously different in it's own right.\n\n\
       Disclosure: This is not meant to be another \"short attack\" stock \
       conversation, but it turns out it's looking like UWMC is currently \
       being heavily shorted against, which is just a positive for those of us \
       who invested in the stock and know it's fair value, market share, and \
       future growth.\n\n\
       Before I jump into some information regarding UWMC, the short volumes, \
       and a few other things. For those who are new to this ticker and want \
       DD on it, click \
       [here](https://www.reddit.com/r/wallstreetbets/comments/m5yvwc/uwmc_key_points_to_the_company/) \
       for my DD, it's pretty short and sweet.\n\n\
       Now, something that got everyone riled up over the last few days was \
       the fact that UWMC wasn't added to the Russell 1000 &amp; 3000 as \
       stated by FTSE Russell Preliminary Inclusions. Turns out for some \
       reason UWMC wasn't selected, and as the Russell Index were the one's \
       who mentioned that they were adding UWMC, CEO Matt Ishbia made a \
       statement to shareholders informing they didn't understand why it \
       wasn't added and believe that the Russell Index miscalculated the \
       available shares to the public (%5 minimum) and have sought out to \
       rectify this with the Russell Index. There's information on WSB  \
       regarding that so I won't go into details. But in short, there's a \
       chance still for UWMC to be added to the Russell Index and for it to be \
       corrected. Regardless if they're not added right away, we can \
       confidently expect them to at some point be added in the next future \
       quarters. CEO Matt Ishbia informed they've reached out to Russell Index \
       to try and have it rectified. Something that's important to understand \
       here is that UWMC being added to the Russell Index wasn't a major \
       factor for investors wanting to invest in this company. Being added to \
       the Russell Index, albeit would have been a great catalyst is not a \
       reason to be concerned, end of story. If you would like to have a quick \
       break down on some key points on the company, please click the link \
       above to my DD, it's pretty short and sweet.\n\n\
       Now, assuming you read my DD or already understand the value of this \
       company, the fact that it's severely undervalued, high dividends, %545 \
       growth year over year, etc etc. Something of notable mention here is \
       the current short volume percentage.\n\n\
       Before I jump into the details, there was a good post on here about \
       Finra and Fintel not reporting all of the short volume information and \
       not being able to provide the total amount of shares shorted from all \
       markets etc. This is very accurate as even on there websites, they \
       indicate that the information are from data pools that they have access \
       to and don't factor all shares, dark pools, etc. (To better understand; \
       to get the true volume of shorted stocks or short interest, you would \
       need to compile all trades from a variety of exchanges, NYSE, NASDAQ, \
       MM, etc.) Another thing I'd like to point out is that over the last \
       month or two, the short interest updates which are supposed to be \
       bi-weekly, has been reporting late. As of right now, we're 03/25/2021 \
       the available short interest data is currently dated as of 02/26/2021 \
       and that information was only updated last week from the previous date \
       of 02/12/2021 ... We're 03/25/2021 and the data is a whole month old \
       when it's supposed to be updated bi-weekly. I find that very odd, \
       unless I'm mistaken here, it seems there's a massive delay on updating \
       the Short Interests information on stocks. (Correct me if I'm wrong).\n\n\
       With that being said, I want to jump into the current short volume % as \
       per Fintel and ShortVolumes. This is not considered the true short \
       volume as it doesn't consider the entire market, but is used to probe \
       the current situation to get a better understanding as to how much of \
       the traded volume has been shorted and for the month of March, it's \
       incredibly high.\n\n\
       Most notable date is March 3rd, 2021 (Where it began), there was a \
       trading volume above 90M that day with 28% of the volume shorted. The \
       average volume traded daily is between 9M - 10M. That alone is massive. \
       Moving forward from the date of 03/03/2021, the short volume of the \
       daily trading volume has been averaging out to around 28-31%.\n\n\
       As of 03/11/2021 to present day; the average shorted volume has been \
       between 55% - 65%, which is absolutely ridiculous. Don't forget there's \
       only 90.23M float of shares available to the public. The rest is owned \
       by UWMC and Institutions and are not available to investors. Please \
       refer to the charts below.\n\n\
       &amp;#x200B;\n\n\
       https://preview.redd.it/du8ri17h84p61.png?width=842&amp;format=png&amp;auto=webp&amp;s=3ae417df7a25e14ca0a4095b98a86f0cc4e90c95\n\n\
       https://preview.redd.it/i8lyjf3i84p61.png?width=844&amp;format=png&amp;auto=webp&amp;s=a1561154897a405d1d891e4c855626d4af818518\n\n\
       &amp;#x200B;\n\n\
       As you can see by the information above, UWMC is in fact being heavily \
       shorted against. The red bars on the chart is the shorted volume, green \
       is the long volume.\n\n\
       On top of that, don't forget the high %4.83 dividend payout dated \
       03/09/2021 that's going to be paid out to investors in April, which the \
       shorts must cover as well.\n\n\
       Another thing to factor in here is as of March the 3rd, 2021 and \
       onward, the average price of the stock has been between $8 and above. \
       This means that shorts most likely did not make any profits yet apart \
       from maybe the few who sold when it was starting to hit $9. (AKA \
       possibilities for a short squeeze and/or gamma squeeze is looking very \
       plausible).\n\n\
       Overall, UWMC is a serious company with 34% market share, the largest \
       wholesale lender in America, they're extremely undervalued, payout high \
       dividends, and the icing on the cake; they're extremely shorted for \
       reasons that make absolutely no sense. This is most definitely the \
       play, and a great hedge for the current market we're in, personal \
       opinion.\n\n\
       Disclaimer: I am invested in UWMC with 211 shares @ $9.09 and will be \
       buying more. Proof of positions are on my DD which is linked above.\n\n\
       I'm not a financial advisors, this is not financial advice, etc. etc.\n\n\
       I can write more about the positive sentiments UWMC has going for it \
       but it's currently 2:34 AM and I need to go to sleep and get ready for \
       work.\n\n\
       I hope this provides useful and to all investors print those tendies \
       and get that ticket to the moon one way or another! 🚀🌔";
    scraper_test "Last post body" ~amount:25 "testing_files/stocksnew.json" body
      "";
    scraper_test "First post title" ~amount:1 "testing_files/stocksnew.json"
      title
      "TIL the first market crash at the NYSE was caused by an epic short \
       squeeze";
    scraper_test "Last post score" ~amount:25 "testing_files/stocksnew.json"
      score 274;
    scraper_test "Third post upvote ratio" ~amount:3
      "testing_files/stocksnew.json" upvote_ratio 0.83;
  ]

let get_stocks json n = scrape_json json ~amount:n

let check_stock_names name exp_stock_names stock_names =
  name >:: fun _ ->
  assert_equal
    (List.sort_uniq compare exp_stock_names)
    (List.sort_uniq compare stock_names)
    ~printer:(pp_list pp_string)

let check_post_props name post exp_upvote_ratio exp_upvote_score =
  let correct_uvr = Parser.upvote_ratio post = exp_upvote_ratio in
  let correct_uvs = Parser.upvote_score post = exp_upvote_score in
  name >:: fun _ -> assert_equal (correct_uvr && correct_uvs) true

let parser_tests =
  let subreddit_1 =
    Scraper.scrape_json "testing_files/stocksnew.json" ~amount:1
  in
  let stocks_1 = Parser.parse subreddit_1 in
  let subreddit_10 =
    Scraper.scrape_json "testing_files/stocksnew.json" ~amount:10
  in
  let stocks_10 = Parser.parse subreddit_10 in
  [
    check_stock_names "Stock Names for 1 post"
      (Parser.stock_names stocks_1)
      [ "NP"; "TIL" ];
    check_post_props "1st Post of stock \"NP\""
      (List.hd (Parser.data stocks_1 "NP"))
      1. 3;
    check_stock_names "Stock Names for 10 posts"
      (Parser.stock_names stocks_10)
      [
        "UWMC";
        "NP";
        "TIL";
        "VIAC";
        "SI";
        "AM";
        "PE";
        "CEO";
        "SEE";
        "ANY";
        "GME";
        "SO";
        "LINK";
        "$UWMC";
        "AMC";
        "CBS";
        "DD";
      ];
    check_post_props "Third Post of stock \"GME\""
      (List.hd (Parser.data stocks_10 "GME"))
      1. 1;
  ]

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
  >::: List.flatten [ parser_tests; scraper_tests; cashset_tests ]

let _ = run_test_tt_main suite
