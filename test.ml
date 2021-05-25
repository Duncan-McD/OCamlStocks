(** Test Plan:
      Scraper (OUnit + manual)
        - OUnit testing ran on JSON snapshots of subreddits
        - General scraping tests developed using glass box testing
        - Functions on post types developed with black box testing
        - Manual testing used for live scraping from web and different orderings
      Cashset (OUnit)
        - OUnit testing ran on stock tickers
        - Tested both real and fake ticker names
        - Tested common words factored out
      Stockdata (manual)
        - Manually tested retrieval of stock data since stockdata is all present
        based values that change constantly
      Parser (OUnit + manual)
        - General parsing tests developed using black box testing
        - Connotation tests developed with glass box and randomized testing
        - History score tested manually (similar to Stockdata)
        - DISCLAIMER: tests commented out by default due to the large amount of
        time required to complete tests; recommended that you run a maximum of
        half of the tests so there are no issues with timing out
      Grapher (manual)
        - Manually tested the grapher functinoality marginally
      Algorithm (manual)
        - Manually tested due to changing history score over time
        - Checked calculations done with various different constants
      Portfolio (OUnit + manual)
        - OUnit testing ran on simple portfolios
        - Manual testing used on time sensitive functions

    This testing approach aimed to rigorously test all aspects of the system
    that were possible to test. The tests used maneuvered around the system's
    reliance on present time values that were constantly changing, like stock
    values or reddit posts. In order to be assured of each modules' validity we
    not only tested in isolation but also required testing be done between
    modules as certain aspects relied upon the validity of earlier modules. The
    various use of different test case development systems (i.e. glass box or
    black box) were used whenever possible to gain maximum testing exposure and
    bisect coverage. *)

open OUnit2
open Printf
open Scraper
open Cashset
open Parser
open Portfolio

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

let scraper_name_test name json_file expec_name =
  let subreddit = scrape_json ~amount:1 ("testing_files/" ^ json_file) in
  let name = subreddit_name subreddit in
  "subreddit name: " ^ name >:: fun _ ->
  assert_equal expec_name name ~printer:pp_string

let scraper_posts_test name ?(amount = 24) json_file attr_func expec_attr
    printer =
  let scraped_post =
    match posts (scrape_json ~amount ("testing_files/" ^ json_file)) with
    | last_post :: t -> attr_func last_post
    | [] -> failwith "No posts scraped"
  in
  name >:: fun _ -> assert_equal expec_attr scraped_post ~printer

let scraper_title_test name ?(amount = 24) json_file expec_title =
  scraper_posts_test
    ("subreddit post title: " ^ name)
    ~amount json_file Scraper.title expec_title pp_string

let scraper_body_test name ?(amount = 24) json_file expec_body =
  scraper_posts_test
    ("subreddit post body: " ^ name)
    ~amount json_file Scraper.body expec_body pp_string

let scraper_score_test name ?(amount = 24) json_file expec_score =
  scraper_posts_test
    ("subreddit post score: " ^ name)
    ~amount json_file Scraper.score expec_score string_of_int

let scraper_ratio_test name ?(amount = 24) json_file expec_ratio =
  scraper_posts_test
    ("subreddit post upvote ratio: " ^ name)
    ~amount json_file Scraper.upvote_ratio expec_ratio string_of_float

let scraper_excep_test name ?(amount = 25) json_file =
  "scraper exception: " ^ name >:: fun _ ->
  assert_raises
    (TooManyPostsRequested (amount - 24))
    (fun () -> scrape_json ~amount ("testing_files/" ^ json_file))

let scraper_tests =
  [
    scraper_name_test "r/stocks hot" "stocks-hot.json" "r/stocks";
    scraper_name_test "r/stocks new" "stocks-new.json" "r/stocks";
    scraper_name_test "r/stocks rising" "stocks-rising.json" "r/stocks";
    scraper_name_test "r/stocks top alltime" "stocks-top-alltime.json"
      "r/stocks";
    scraper_name_test "r/stocks top today" "stocks-top-today.json" "r/stocks";
    scraper_name_test "r/investing hot" "investing-hot.json" "r/investing";
    scraper_name_test "r/wallstreetbets hot" "wallstreetbets-hot.json"
      "r/wallstreetbets";
    scraper_title_test "post 1 in r/stocks hot" ~amount:1 "stocks-hot.json"
      "Rate My Portfolio - r/Stocks Quarterly Thread March 2021";
    scraper_title_test "post 2 in r/stocks hot" ~amount:2 "stocks-hot.json"
      "r/Stocks Daily Discussion &amp; Fundamentals Friday May 14, 2021";
    scraper_title_test "post 7 in r/stocks hot" ~amount:7 "stocks-hot.json"
      "If your goal is to live off your stock returns, how much are you \
       looking to invest?";
    scraper_title_test "post 1 in r/stocks new" ~amount:1 "stocks-new.json"
      "Does anyone know of publicly traded companies that specialize/ have \
       relations with nanotechnology?";
    scraper_title_test "post 2 in r/stocks new" ~amount:2 "stocks-new.json"
      "Should I invest more than half of my money as a first year college \
       student living with their parents?";
    scraper_title_test "post 11 in r/stocks new" ~amount:11 "stocks-new.json"
      "Trading simulator?";
    scraper_title_test "post 1 in r/stocks rising" ~amount:1
      "stocks-rising.json"
      "Dymon Asia Capital purchases $100,000,000 worth of Palantir ($PLTR) \
       shares";
    scraper_title_test "post 2 in r/stocks rising" ~amount:2
      "stocks-rising.json" "Trading simulator?";
    scraper_title_test "post 15 in r/stocks rising" ~amount:15
      "stocks-rising.json"
      "What is the general sentiment about the stock market?";
    scraper_title_test "post 1 in r/stocks top alltime" ~amount:1
      "stocks-top-alltime.json"
      "It's fucking awful seeing the \"Silver\" misinformation campaign \
       everywhere I look";
    scraper_title_test "post 2 in r/stocks top alltime" ~amount:2
      "stocks-top-alltime.json"
      "Companies try to prevent people from trading GME and AMC";
    scraper_title_test "post 18 in r/stocks top alltime" ~amount:18
      "stocks-top-alltime.json"
      "Sign The Petition: Retail Investors Demand Market Transparency! Make \
       the Hedge Funds report their Shorts!";
    scraper_title_test "post 1 in r/stocks top today" ~amount:1
      "stocks-top-today.json"
      "Why would anyone invest in IBM? Losing money since 2013?";
    scraper_title_test "post 2 in r/stocks top today" ~amount:2
      "stocks-top-today.json"
      "What's causing ICLN to gradually decline since the January? Do you \
       reckon it will recover?";
    scraper_title_test "post 9 in r/stocks top today" ~amount:9
      "stocks-top-today.json" "What are some aggressive/risky ETFs";
    scraper_title_test "post 1 in r/investing hot" ~amount:1
      "investing-hot.json"
      "Daily General Discussion and spitballin thread - May 16, 2021";
    scraper_title_test "post 2 in r/investing hot" ~amount:2
      "investing-hot.json"
      "Daily Advice Thread - All basic help or advice questions must be posted \
       here.";
    scraper_title_test "post 20 in r/investing hot" ~amount:20
      "investing-hot.json"
      "(US) How will a lazy portfolio handle the inflation we are starting to \
       see?";
    scraper_title_test "post 1 in r/wallstreetbets hot" ~amount:1
      "wallstreetbets-hot.json" "What Are Your Moves Tomorrow, May 17, 2021";
    scraper_title_test "post 2 in r/wallstreetbets hot" ~amount:2
      "wallstreetbets-hot.json" "Government hates this simple tax trick";
    scraper_title_test "post 14 in r/wallstreetbets hot" ~amount:14
      "wallstreetbets-hot.json"
      "This is not economic boom, this is just recovery !!!!";
    scraper_body_test "post 1 in r/stocks hot" ~amount:1 "stocks-hot.json"
      "Please use this thread to discuss your portfolio, learn of other stock \
       tickers, and help out users by giving constructive criticism.\n\n\
       Why quarterly?  Public companies report earnings quarterly; many \
       investors take this as an opportunity to rebalance their portfolios.  \
       We highly recommend you do some reading:  A list of [relevant posts \
       &amp; book \
       recommendations.](https://www.reddit.com/r/stocks/wiki/index#wiki_relevant_posts_.26amp.3B_book_recommendations)\n\n\
       You can find stocks on your own by using a scanner like your broker's \
       or [Finviz.](https://finviz.com/screener.ashx)  To help further, here's \
       a list of [relevant \
       websites.](https://www.reddit.com/r/stocks/wiki/index#wiki_relevant_websites.2Fapps)\n\n\
       If you don't have a broker yet, see our [list of \
       brokers](https://www.reddit.com/r/stocks/wiki/index#wiki_brokers_for_investing) \
       or search old posts.  If you haven't started investing or trading yet, \
       then setup your [paper \
       trading.](https://www.reddit.com/r/stocks/wiki/index#wiki_is_there_a_way_to_practice.3F)\n\n\
       Be aware of [Business Cycle \
       Investing](https://eresearch.fidelity.com/eresearch/markets_sectors/sectors/si_business_cycle.jhtml?tab=sibusiness) \
       which Fidelity issues updates to the state of global business cycles \
       every 1 to 3 months (note: Fidelity changes their links often, so \
       search for it since their take on it is enlightening).  [Investopedia's \
       take on the Business \
       Cycle](https://www.investopedia.com/articles/investing/061316/business-cycle-investing-ratios-use-each-cycle.asp) \
       and their \
       [video.](https://www.investopedia.com/video/play/business-cycle/)\n\n\
       If you need help with a falling stock price, check out Investopedia's \
       [The Art of Selling A Losing \
       Position](https://www.investopedia.com/articles/02/022002.asp) and \
       their [list of \
       biases.](https://www.investopedia.com/articles/stocks/08/capital-losses.asp)\n\n\
       Here's a list of all the [previous portfolio \
       stickies.](https://www.reddit.com/r/stocks/search?q=author%3Aautomoderator+%22Rate+My+Portfolio%22+-+r%2FStocks+Quarterly+Thread&amp;restrict_sr=on&amp;include_over_18=on&amp;sort=new&amp;t=all)";
    scraper_body_test "post 2 in r/stocks hot" ~amount:2 "stocks-hot.json"
      "This is the daily discussion, so anything stocks related is fine, but \
       the theme for today is on fundamentals, but if fundamentals aren't your \
       thing then just ignore the theme and/or [post your arguments against \
       fundamentals \
       here](https://www.reddit.com/r/stocks/search?q=author%3Aautomoderator+%22r%2Fstocks+semiannual+arguments+against+TA%22&amp;restrict_sr=on&amp;include_over_18=on&amp;sort=new&amp;t=all) \
       and not in the current post.\n\n\
       Some helpful day to day links, including news:\n\n\
       * [Finviz](https://finviz.com/quote.ashx?t=spy) for charts, \
       fundamentals, and aggregated news on individual stocks\n\
       * [Bloomberg market news](https://www.bloomberg.com/markets)\n\
       * StreetInsider news:\n\
      \  * [Market Check](https://www.streetinsider.com/Market+Check) - \
       Possibly why the market is doing what it's doing including sudden \
       spikes/dips\n\
      \  * [Reuters aggregated](https://www.streetinsider.com/Reuters) - \
       Global news\n\n\
       -----\n\n\
       Most fundamentals are updated every 3 months due to the fact that \
       corporations release earnings reports every quarter, so traders are \
       always speculating at what those earnings will say, and investors may \
       change the size of their holdings based on those reports.  Expect a lot \
       of volatility around earnings, but it usually doesn't matter if you're \
       holding long term, but keep in mind the importance of earnings reports \
       because a trend of declining earnings or a decline in some other \
       fundamental will drive the stock down over the long term as well.\n\n\
       See the following word cloud and click through for the wiki:\n\n\
       [Market Cap - Shares Outstanding - Volume - Dividend - EPS - P/E Ratio \
       - EPS Q/Q - PEG - Sales Q/Q - Return on Assets (ROA) - Return on Equity \
       (ROE) - BETA - SMA - quarterly \
       earnings](https://www.reddit.com/r/stocks/wiki/fundamentals-themed-post)\n\n\
       If you have a basic question, for example \"what is EBITDA,\" then \
       google \"investopedia EBITDA\" and click the Investopedia article on \
       it; do this for everything until you have a more in depth question or \
       just want to share what you learned.\n\n\
       Useful links:\n\n\
       * [Investopedia \
       page](https://www.investopedia.com/fundamental-analysis-4689757/) on \
       fundamental analysis including [Discounted Cash \
       Flow](https://www.investopedia.com/university/dcf/) analysis; see \
       [definition here](https://www.investopedia.com/terms/d/dcf.asp) and \
       read [their PDF on the \
       topic.](http://i.investopedia.com/inv/pdf/tutorials/fundamentalanalysis_intro.pdf)\n\
       * [FINVIZ](https://finviz.com/quote.ashx?t=aapl) for fundamental data, \
       charts, and aggregated news\n\
       * [Earnings Whisper](https://www.earningswhispers.com/stocks/aapl) for \
       earnings details\n\n\
       See our past [daily discussions \
       here.](https://www.reddit.com/r/stocks/search?q=author%3Aautomoderator+%22r%2Fstocks+daily+discussion%22&amp;restrict_sr=on&amp;sort=new&amp;t=all)  \
       Also links for:  \
       [Technicals](https://www.reddit.com/r/stocks/search?q=author%3Aautomoderator+title%3Atechnicals&amp;restrict_sr=on&amp;include_over_18=on&amp;sort=new&amp;t=all) \
       Tuesday, [Options \
       Trading](https://www.reddit.com/r/stocks/search?q=author%3Aautomoderator+title%3Aoptions&amp;restrict_sr=on&amp;include_over_18=on&amp;sort=new&amp;t=all) \
       Thursday, and \
       [Fundamentals](https://www.reddit.com/r/stocks/search?q=author%3Aautomoderator+title%3Afundamentals&amp;restrict_sr=on&amp;include_over_18=on&amp;sort=new&amp;t=all) \
       Friday.";
    scraper_body_test "post 7 in r/stocks hot" ~amount:7 "stocks-hot.json"
      "I was looking into how much I wanna have invested in stocks before I go \
       part time somewhere in the future. I don't really spend a lot of money \
       so most of my income just sits there, with some of it invested.\n\n\
       I know I could easily live off 2k bucks a month, but obviously that \
       takes quite the investment to get passively each month.\n\n\
       What are you guys aiming for? What's your strategy to achieve your goal?";
    scraper_body_test "post 2 in r/stocks new" ~amount:2 "stocks-new.json"
      "I know that as a general rule most people tend to save the majority of \
       their money to cover their living expenses and only invest any spare \
       capacity of money that they can afford to lose, but considering that I \
       live at home with little to no living expenses, should I take more of a \
       risk with my money and set aside a larger chunk of my savings for \
       investments? I have never invested in the stock market, so my \
       experience is limited and I'm still learning how to analyse trends and \
       financial statements and all that, so I'm a little unsure of how much \
       risk I should really take considering my inexperience. My budget is \
       pretty low as well at just around 5,000 AUD, so in order to really make \
       any worthwhile returns I probably need to invest around half of that. \
       What do you guys think?";
    scraper_body_test "post 1 in r/stocks rising" ~amount:1 "stocks-rising.json"
      "http://pdf.secdatabase.com/44/0001172661-21-001130.pdf\n\n\
       SECURITIES AND EXCHANGE COMMISSION\n\n\
       FORM 13F-HR\n\n\
       Initial quarterly Form 13F holdings report filed by institutional \
       managers\n\n\
       Filing Date: 2021-05-14\n\
       Period of Report: 2021-03-31 SEC Accession No. 0001172661-21-001130\n\n\
       FILER:\n\n\
       DYMON ASIA CAPITAL (SINGAPORE) PTE. LTD. CIK:1672142\n\n\
       IRS No.: 000000000\n\
       State of Incorp.:U0\n\
       Fiscal Year End: 1231\n\
       Type: 13F-HR\n\
       Act: 34\n\
       File No.: 028-17414\n\
       Film No.: 21922569\n\n\
       Mailing Address:\n\n\
       ONE TEMASEK AVENUE #11-01 MILLENIA TOWER SINGAPORE U0 039192\n\n\
       Business Address:\n\n\
       ONE TEMASEK AVENUE #11-01 MILLENIA TOWER SINGAPORE U0 039192 65-67051666";
    scraper_body_test "post 2 in r/stocks top alltime" ~amount:2
      "stocks-top-alltime.json"
      "Not sure about the other trading apps but Trading212 prevents people \
       now from buying shares. Quote:\n\n\
       - Warning! In the interest of mitigating risk for our clients, we have \
       temporarily placed GameStop and AMC Entertainment in reduce-only mode \
       as highly unusual volumes have led to an unprecedented market \
       environment. New positions cannot be opened, existing ones can be \
       reduced or closed. -\n\n\
       Not sure if they are really concerned about their customers, or they've \
       been lobbied by hedge funds to prevent ordinary people from destroying \
       them. I don't care about GME and AMC, I have no position, but now I am \
       angry for this decision. They always go against the poor individuals \
       and let the billionaires save their asses. No one saves us when we go \
       bankrupt by them.\n\n\
       Let that sink in\n\n\
       Edit: thank you for all the rewards and comments! What a great \
       community we are!";
    scraper_body_test "post 18 in r/stocks top alltime" ~amount:18
      "stocks-top-alltime.json"
      "**Sign the petition here: https://www.change.org/sec-amend-13-f**\n\n\n\
       Retail Investors demand more visibility into institutional trading and \
       borrowing. Anyone investing over 1 billion dollars (i.e. hedge funds \
       and other investment institutions) is required to disclose their \
       holdings to promote transparency in our markets - it's called Form \
       13-F. **But did you know that they only need to disclose it 4 times a \
       year? And did you know its published with a 1 month delay? And did you \
       know that they don't need to disclose all of their positions?**\n\n\n\
       We the people are asking for a re-evaluation of transparency \
       requirements for Institutional Investors. We have access to technology \
       and data that gives us new sophistication - and are beginning to \
       understand there is a tremendous disparity in access between retail and \
       institutional investors, and are concerned that this access is being \
       used against us, in ways that we genuinely worry could be in flagrant \
       violation of Securities Laws. We believe that with better access to \
       institutional trading data, retail investors can better participate in \
       the market when making buying and selling decisions.\n\n\n\
       **According to Form 13F (https://www.sec.gov/files/form13f.pdf), \
       Institutional Investors only need to disclose their positions 4 times a \
       year. Why?**\n\n\n\
       *Filing of Form 13F. A Manager must file a Form 13F report with the \
       Commission within 45 days after the end of each calendar year and each \
       of the first three calendar quarters of each calendar year. As required \
       by Section 13(f)(5) of the Exchange Act, a Manager which is a bank, the \
       deposits of which are insured in accordance with the Federal Deposit \
       Insurance Act, must file with the appropriate regulatory agency for the \
       bank a copy of every Form 13F report filed with the Commission pursuant \
       to this subsection by or with respect to such bank. Filers who file \
       Form 13F electronically can satisfy their obligation to file with other \
       regulatory agencies by sending (a) a paper copy of the EDGAR filing \
       (provided the Manager removes or blanks out the confidential access \
       codes); (b) the filing in electronic format, if the regulatory agency \
       with which the filing is being made has made provisions to receive \
       filings in electronic format;*\n\n\n\
       **In your FAQ (https://www.sec.gov/divisions/investment/13ffaq.htm), it \
       is clear Institutional Investors are not required to disclose short \
       positions. Why?**\n\n\n\
       *Question 41\n\
       Q: What about short positions?\n\
       A: You should not include short positions on Form 13F. You also should \
       not subtract your short position(s) in a security from your long \
       position(s) in that same security; report only the long position.*\n\n\n\
       **Contact the SEC and let them know retail investors demand increased \
       transparency (https://www.sec.gov/contact-information/sec-directory)**";
    scraper_body_test "post 1 in r/stocks top today" ~amount:1
      "stocks-top-today.json"
      "I don't get it.  What would be the incentive of investing in a company \
       like IBM?  The stock price has been depreciating since 2013.\n\n\
       I realize they have a nice 4.5% dividend, but does that make up for \
       losing your principle investment?\n\n\
       What am I failing to see?";
    scraper_body_test "post 2 in r/stocks top today" ~amount:2
      "stocks-top-today.json"
      "When I look at the graph of ICLN in the last 10 years, the there's been \
       barely any growth in it until the pandemic hit and suddenly after April \
       2020, it started growing and kept growing until the beginning of \
       January 2021.\n\n\
       Perhaps this sudden growth is down to the optimism that Biden will \
       promote renewable energy, along with the US government's cash injection \
       to boost the economy during the pandemic.\n\n\
       However, now that ICLN is back to its normal trend (4/2011 - 3/2020), \
       do you reckon it will come back to north trending graph any time soon? \
       I bought ICLN at the end of February and now it's more than 27% down. \
       Do you reckon it will grow back any time soon? Or should I give up on \
       it?";
    scraper_body_test "post 1 in r/investing hot" ~amount:1 "investing-hot.json"
      "Have a general question?  Want to offer some commentary on markets?  \
       Maybe you would just like to throw out a neat fact that doesn't warrant \
       a self post?  Feel free to post here! \n\n\
       This thread is for: \n\n\
       * General questions\n\
       * Your personal commentary on markets\n\
       * Opinion gathering on a given stock\n\
       * Non advice beginner questions\n\n\
       Keep in mind that this subreddit, and this thread, is not an \
       appropriate venue for questions that should be directed towards your \
       broker's customer support or google.  \n\n\
       If you would like to ask a question about your personal situation or if \
       you are asking for advice please keep these posts in the daily advice \
       thread as that thread is more well suited for those questions.\n\n\
       Any posts that should be comments in this thread will likely be removed.";
    scraper_body_test "post 2 in r/investing hot" ~amount:2 "investing-hot.json"
      "If your question is \"I have $10,000, what do I do?\" or other \"advice \
       for my personal situation\" questions, you should include relevant \
       information, such as the following:\n\n\
       * How old are you? What country do you live in?  \n\
       * Are you employed/making income? How much?  \n\
       * What are your objectives with this money? (Buy a house? Retirement \
       savings?)  \n\
       * What is your time horizon? Do you need this money next month? Next \
       20yrs?  \n\
       * What is your risk tolerance? (Do you mind risking it at blackjack or \
       do you need to know its 100% safe?)  \n\
       * What are you current holdings? (Do you already have exposure to \
       specific funds and sectors? Any other assets?)  \n\
       * Any big debts (include interest rate) or expenses?  \n\
       * And any other relevant financial information will be useful to give \
       you a proper answer.  \n\n\
       Please consider consulting our FAQ first - \
       https://www.reddit.com/r/investing/wiki/faq\n\
       And our [side bar](https://www.reddit.com/r/investing/about/sidebar) \
       also has useful resources.  \n\n\
       Be aware that these answers are just opinions of Redditors and should \
       be used as a starting point for your research. You should strongly \
       consider seeing a registered financial rep before making any financial \
       decisions!";
    scraper_body_test "post 20 in r/investing hot" ~amount:20
      "investing-hot.json"
      "For example, how will a 3-fund or 4-fund portfolio weather the \
       increased inflation we are starting to see?\n\n\
       For example, with the following vanguard etfs...\n\n\
       vti vxus vnq bnd\n\n\
       I would guess that vxus would do well purely from a US dollar inflation \
       perspective since it is international and therefore priced on other \
       currencies.\n\n\
       I'd expect vnq to do well as well with a weakening dollar.\n\n\
       With vti, would it be the same? Or would investors move money elsewhere \
       from the s&amp;p 500 companies?\n\n\
       With bnd, I'd expect it to struggle with inflation?\n\n\
       Any recommendations for adjustment in a portfolio? I see varied \
       opinions online about inflation protected investments and also vtv.";
    scraper_body_test "post 1 in r/wallstreetbets hot" ~amount:1
      "wallstreetbets-hot.json"
      "Your daily trading discussion thread. Please keep the shitposting to a \
       minimum. \n\n\
       ^Navigate ^WSB|^We ^recommend ^best ^daily ^DD\n\
       :--|:--                                 \n\
       **DD** | \
       [All](https://reddit.com/r/wallstreetbets/search?sort=new&amp;restrict_sr=on&amp;q=flair%3ADD) \
       / [**Best \
       Daily**](https://www.reddit.com/r/wallstreetbets/search?sort=top&amp;q=flair%3ADD&amp;restrict_sr=on&amp;t=day) \
       / [Best \
       Weekly](https://www.reddit.com/r/wallstreetbets/search?sort=top&amp;q=flair%3ADD&amp;restrict_sr=on&amp;t=week)\n\
       **Discussion** | \
       [All](https://reddit.com/r/wallstreetbets/search?sort=new&amp;restrict_sr=on&amp;q=flair%3ADiscussion) \
       / [**Best \
       Daily**](https://www.reddit.com/r/wallstreetbets/search?sort=top&amp;q=flair%3ADiscussion&amp;restrict_sr=on&amp;t=day) \
       / [Best \
       Weekly](https://www.reddit.com/r/wallstreetbets/search?sort=top&amp;q=flair%3ADiscussion&amp;restrict_sr=on&amp;t=week)\n\
       **YOLO** | \
       [All](https://reddit.com/r/wallstreetbets/search?sort=new&amp;restrict_sr=on&amp;q=flair%3AYOLO) \
       / [**Best \
       Daily**](https://www.reddit.com/r/wallstreetbets/search?sort=top&amp;q=flair%3AYOLO&amp;restrict_sr=on&amp;t=day) \
       / [Best \
       Weekly](https://www.reddit.com/r/wallstreetbets/search?sort=top&amp;q=flair%3AYOLO&amp;restrict_sr=on&amp;t=week)\n\
       **Gain** | \
       [All](https://reddit.com/r/wallstreetbets/search?sort=new&amp;restrict_sr=on&amp;q=flair%3AGain) \
       / [**Best \
       Daily**](https://www.reddit.com/r/wallstreetbets/search?sort=top&amp;q=flair%3AGain&amp;restrict_sr=on&amp;t=day) \
       / [Best \
       Weekly](https://www.reddit.com/r/wallstreetbets/search?sort=top&amp;q=flair%3AGain&amp;restrict_sr=on&amp;t=week)\n\
       **Loss** | \
       [All](https://reddit.com/r/wallstreetbets/search?sort=new&amp;restrict_sr=on&amp;q=flair%3ALoss) \
       / [**Best \
       Daily**](https://www.reddit.com/r/wallstreetbets/search?sort=top&amp;q=flair%3ALoss&amp;restrict_sr=on&amp;t=day) \
       / [Best \
       Weekly](https://www.reddit.com/r/wallstreetbets/search?sort=top&amp;q=flair%3ALoss&amp;restrict_sr=on&amp;t=week)\n\n\
       [Weekly Earnings Discussion \
       Thread](https://www.reddit.com/r/wallstreetbets/search?sort=new&amp;restrict_sr=on&amp;q=flair%3A%22Earnings%20Thread%22)\n\n\
       **Read the \
       [rules](https://www.reddit.com/r/wallstreetbets/wiki/contentguide) and \
       make sure other people follow them.**\n\n\
       Try [No Meme \
       Mode](https://www.reddit.com/r/wallstreetbets/search/?q=-flair%3AMeme%20-flair%3ASatire%20-flair%3AShitpost&amp;restrict_sr=1&amp;t=day&amp;sort=hot), \
       also accessible through the top bar.\n\n\
       Follow [@Official_WSB](https://twitter.com/Official_WSB) on Twitter, \
       all other accounts are impersonators.";
    scraper_body_test "post 2 in r/wallstreetbets hot" ~amount:2
      "wallstreetbets-hot.json" "";
    scraper_score_test "post 1 in r/stocks hot" ~amount:1 "stocks-hot.json" 486;
    scraper_score_test "post 2 in r/stocks hot" ~amount:2 "stocks-hot.json" 62;
    scraper_score_test "post 7 in r/stocks hot" ~amount:7 "stocks-hot.json" 468;
    scraper_score_test "post 1 in r/stocks new" ~amount:1 "stocks-new.json" 0;
    scraper_score_test "post 2 in r/stocks new" ~amount:2 "stocks-new.json" 2;
    scraper_score_test "post 11 in r/stocks new" ~amount:11 "stocks-new.json" 10;
    scraper_score_test "post 1 in r/stocks rising" ~amount:1
      "stocks-rising.json" 13;
    scraper_score_test "post 2 in r/stocks rising" ~amount:2
      "stocks-rising.json" 3;
    scraper_score_test "post 15 in r/stocks rising" ~amount:15
      "stocks-rising.json" 117;
    scraper_score_test "post 1 in r/stocks top alltime" ~amount:1
      "stocks-top-alltime.json" 101660;
    scraper_score_test "post 2 in r/stocks top alltime" ~amount:2
      "stocks-top-alltime.json" 88217;
    scraper_score_test "post 18 in r/stocks top alltime" ~amount:18
      "stocks-top-alltime.json" 17084;
    scraper_score_test "post 1 in r/stocks top today" ~amount:1
      "stocks-top-today.json" 906;
    scraper_score_test "post 2 in r/stocks top today" ~amount:2
      "stocks-top-today.json" 608;
    scraper_score_test "post 9 in r/stocks top today" ~amount:9
      "stocks-top-today.json" 36;
    scraper_score_test "post 1 in r/investing hot" ~amount:1
      "investing-hot.json" 13;
    scraper_score_test "post 2 in r/investing hot" ~amount:2
      "investing-hot.json" 10;
    scraper_score_test "post 20 in r/investing hot" ~amount:20
      "investing-hot.json" 40;
    scraper_score_test "post 1 in r/wallstreetbets hot" ~amount:1
      "wallstreetbets-hot.json" 254;
    scraper_score_test "post 2 in r/wallstreetbets hot" ~amount:2
      "wallstreetbets-hot.json" 17067;
    scraper_score_test "post 14 in r/wallstreetbets hot" ~amount:14
      "wallstreetbets-hot.json" 321;
    scraper_ratio_test "post 1 in r/stocks hot" ~amount:1 "stocks-hot.json" 0.99;
    scraper_ratio_test "post 2 in r/stocks hot" ~amount:2 "stocks-hot.json" 0.92;
    scraper_ratio_test "post 7 in r/stocks hot" ~amount:7 "stocks-hot.json" 0.94;
    scraper_ratio_test "post 1 in r/stocks new" ~amount:1 "stocks-new.json" 0.5;
    scraper_ratio_test "post 2 in r/stocks new" ~amount:2 "stocks-new.json" 1.0;
    scraper_ratio_test "post 11 in r/stocks new" ~amount:11 "stocks-new.json"
      0.86;
    scraper_ratio_test "post 1 in r/stocks rising" ~amount:1
      "stocks-rising.json" 0.84;
    scraper_ratio_test "post 2 in r/stocks rising" ~amount:2
      "stocks-rising.json" 0.8;
    scraper_ratio_test "post 15 in r/stocks rising" ~amount:15
      "stocks-rising.json" 0.81;
    scraper_ratio_test "post 1 in r/stocks top alltime" ~amount:1
      "stocks-top-alltime.json" 0.9;
    scraper_ratio_test "post 2 in r/stocks top alltime" ~amount:2
      "stocks-top-alltime.json" 0.94;
    scraper_ratio_test "post 18 in r/stocks top alltime" ~amount:18
      "stocks-top-alltime.json" 0.99;
    scraper_ratio_test "post 1 in r/stocks top today" ~amount:1
      "stocks-top-today.json" 0.94;
    scraper_ratio_test "post 2 in r/stocks top today" ~amount:2
      "stocks-top-today.json" 0.93;
    scraper_ratio_test "post 9 in r/stocks top today" ~amount:9
      "stocks-top-today.json" 0.83;
    scraper_ratio_test "post 1 in r/investing hot" ~amount:1
      "investing-hot.json" 0.79;
    scraper_ratio_test "post 2 in r/investing hot" ~amount:2
      "investing-hot.json" 0.82;
    scraper_ratio_test "post 20 in r/investing hot" ~amount:20
      "investing-hot.json" 0.79;
    scraper_ratio_test "post 1 in r/wallstreetbets hot" ~amount:1
      "wallstreetbets-hot.json" 0.87;
    scraper_ratio_test "post 2 in r/wallstreetbets hot" ~amount:2
      "wallstreetbets-hot.json" 0.96;
    scraper_ratio_test "post 14 in r/wallstreetbets hot" ~amount:14
      "wallstreetbets-hot.json" 0.84;
    scraper_excep_test "r/stocks hot" "stocks-hot.json";
    scraper_excep_test "r/stocks hot with 50" ~amount:50 "stocks-hot.json";
    scraper_excep_test "r/stocks hot with 100" ~amount:100 "stocks-hot.json";
    scraper_excep_test "r/stocks new" "stocks-new.json";
    scraper_excep_test "r/stocks rising" "stocks-rising.json";
    scraper_excep_test "r/stocks top alltime" "stocks-top-alltime.json";
    scraper_excep_test "r/stocks top today" "stocks-top-today.json";
    scraper_excep_test "r/investing hot" "investing-hot.json";
    scraper_excep_test "r/wallstreetbets hot" "wallstreetbets-hot.json";
  ]

let cashset_test ticker expec =
  "cashset: " ^ ticker >:: fun _ ->
  assert_equal expec (Cashset.is_stock_name ticker) ~printer:string_of_bool

let cashset_tests =
  [
    cashset_test "AMC" true;
    cashset_test "$AMC" true;
    cashset_test "AMc" false;
    cashset_test "AmC" false;
    cashset_test "aMC" false;
    cashset_test "amc" false;
    cashset_test "$amc" false;
    cashset_test "GME" true;
    cashset_test "$GME" true;
    cashset_test "AAPL" true;
    cashset_test "AMZN" true;
    cashset_test "MKC" true;
    cashset_test "$ASO" true;
    cashset_test "ZYXI" true;
    cashset_test "SPRB" true;
    cashset_test "OSTK" true;
    cashset_test "LXFR" true;
    cashset_test "HOLX" true;
    cashset_test "HOME" true;
    cashset_test "HOMB" true;
    cashset_test "SPCE" true;
    cashset_test "$RBLX" true;
    cashset_test "RETA" true;
    cashset_test "UPWK" true;
    cashset_test "CVLT" true;
    cashset_test "TDUP" true;
    cashset_test "OSTK" true;
    cashset_test "TSLA" true;
    cashset_test "F" true;
    cashset_test "GE" true;
    cashset_test "DIS" true;
    cashset_test "$MSFT" true;
    cashset_test "NOK" true;
    cashset_test "GPRO" true;
    cashset_test "DAL" true;
    cashset_test "I" false;
    cashset_test "AND" false;
    cashset_test "OR" false;
    cashset_test "THE" false;
    cashset_test "NOT" false;
    cashset_test "WE" false;
    cashset_test "OMG" false;
    cashset_test "IF" false;
    cashset_test "WHAT" false;
    cashset_test "TIL" false;
  ]

let parser_stocks_test name ?(amount = 24) json_file expec_stocks =
  let subreddit = scrape_json ~amount ("testing_files/" ^ json_file) in
  let stocks = List.sort String.compare (stock_names @@ parse subreddit) in
  let sort_expec = List.sort String.compare expec_stocks in
  "parser stocks: " ^ name >:: fun _ ->
  assert_equal sort_expec stocks ~printer:(pp_list pp_string)

type conn = POS | NEG | NEU

let parser_conn_test name str expec_conn =
  let conn = connotation_str str in
  let conn_range =
    match expec_conn with
    | POS -> conn >= 0.05
    | NEG -> conn <= -0.05
    | NEU -> conn > -0.05 && conn < 0.05
  in
  "parser connotation: " ^ name >:: fun _ ->
  assert_bool "Connotation does not match expected range" conn_range

let parser_tests =
  [
    (* parser_stocks_test "1 post in r/stocks hot" ~amount:1 "stocks-hot.json" [];
       parser_stocks_test "2 posts in r/stocks hot" ~amount:2 "stocks-hot.json" ["PEG"];
       parser_stocks_test "7 posts in r/stocks hot" ~amount:7 "stocks-hot.json" ["PEG"];
       parser_stocks_test "24 posts in r/stocks hot" ~amount:24 "stocks-hot.json" ["ABBV"; "ALTO"; "ATI"; "CTXR"; "CVX"; "GO"; "NIO"; "PEG"; "REGN"; "RH"; "SPG"; "T"; "TOO"];
       parser_stocks_test "1 post in r/stocks new" ~amount:1 "stocks-new.json" [];
       parser_stocks_test "2 posts in r/stocks new" ~amount:2 "stocks-new.json" [];
       parser_stocks_test "11 posts in r/stocks new" ~amount:11 "stocks-new.json" ["ATI"; "TOO"];
       parser_stocks_test "24 posts in r/stocks new" ~amount:24 "stocks-new.json" ["ABBV"; "ALTO"; "ATI"; "B"; "CTXR"; "CVX"; "DISCA"; "DISCB"; "DISCK"; "K"; "NIO"; "PLAY"; "REGN"; "RH"; "SPG"; "SPLK"; "SUMO"; "TOO"; "TSE"];
       parser_stocks_test "1 post in r/stocks rising" ~amount:1 "stocks-rising.json" [];
       parser_stocks_test "2 posts in r/stocks rising" ~amount:2 "stocks-rising.json" [];
       parser_stocks_test "15 posts in r/stocks rising" ~amount:15 "stocks-rising.json" ["ABBV"; "CVX"; "GO"; "REGN"; "RH"; "T"];
       parser_stocks_test "24 posts in r/stocks rising" ~amount:24 "stocks-rising.json" ["ABBV"; "ALTO"; "CTXR"; "CVX"; "GO"; "NIO"; "RDFN"; "REGN"; "RH"; "T"; "UK"; "YETI"];
       parser_stocks_test "1 post in r/stocks top alltime" ~amount:1 "stocks-top-alltime.json" [];
       parser_stocks_test "2 posts in r/stocks top alltime" ~amount:2 "stocks-top-alltime.json" ["AMC"; "GME"];
       parser_stocks_test "18 posts in r/stocks top alltime" ~amount:18 "stocks-top-alltime.json" ["AMC"; "ATH"; "CTO"; "DATA"; "DD"; "EV"; "GME"; "L"; "MORE"; "MY"; "RH"; "TD"; "VERY"; "WELL"];
       parser_stocks_test "24 posts in r/stocks top alltime" ~amount:24 "stocks-top-alltime.json" ["AMC"; "ATH"; "B"; "C"; "CTO"; "DATA"; "DD"; "DM"; "EV"; "GME"; "L"; "MORE"; "MY"; "RH"; "TD"; "TV"; "VERY"; "WELL"];
       parser_stocks_test "1 post in r/stocks top today" ~amount:1 "stocks-top-today.json" [];
       parser_stocks_test "2 posts in r/stocks top today" ~amount:2 "stocks-top-today.json" [];
       parser_stocks_test "9 posts in r/stocks top today" ~amount:9 "stocks-top-today.json" ["GO"; "T"];
       parser_stocks_test "24 posts in r/stocks top today" ~amount:24 "stocks-top-today.json" ["ABBV"; "ALTO"; "ATI"; "CVX"; "DD"; "GO"; "RDFN"; "REGN"; "RH"; "SPG"; "T"; "TOO"; "TV"; "UK"; "YETI"];
       parser_stocks_test "1 post in r/investing hot" ~amount:1 "investing-hot.json" [];
       parser_stocks_test "2 posts in r/investing hot" ~amount:2 "investing-hot.json" [];
       parser_stocks_test "20 posts in r/investing hot" ~amount:20 "investing-hot.json" ["AI"; "AMC"; "BABA"; "CTV"; "DCA"; "DCF"; "DD"; "DISH"; "FUND"; "GME"; "GOOGL"; "ID"; "JD"; "LMT"; "M"; "MSCI"; "NIO"; "ONE"; "Q"; "RTX"; "T"; "TKC"; "TTD"; "TV"; "UBER"; "UPS"];
       parser_stocks_test "24 posts in r/investing hot" ~amount:24 "investing-hot.json" ["AI"; "AMC"; "BABA"; "CTV"; "DCA"; "DCF"; "DD"; "DISH"; "FUND"; "GME"; "GOOGL"; "HUGE"; "ID"; "JD"; "LMT"; "M"; "MSCI"; "NIO"; "ONE"; "Q"; "RTX"; "T"; "TKC"; "TTD"; "TV"; "UBER"; "UPS"; "X"];
       parser_stocks_test "1 post in r/wallstreetbets hot" ~amount:1 "wallstreetbets-hot.json" [];
       parser_stocks_test "2 posts in r/wallstreetbets hot" ~amount:2 "wallstreetbets-hot.json" [];
       parser_stocks_test "14 posts in r/wallstreetbets hot" ~amount:14 "wallstreetbets-hot.json" ["AMC"; "ANY"; "API"; "DD"; "DIS"; "GDP"; "OC"; "ONE"; "PE"; "PLTR"; "SE"; "UWMC"];
       parser_stocks_test "24 posts in r/wallstreetbets hot" ~amount:24 "wallstreetbets-hot.json" ["AMC"; "ANY"; "API"; "DD"; "DIS"; "GDP"; "HD"; "OC"; "ONE"; "PE"; "PLTR"; "RIOT"; "SE"; "UWMC"]; *)
    parser_conn_test "postive" "VADER is smart, handsome, and funny." POS;
    parser_conn_test "postive punctuation"
      "VADER is smart, handsome, and funny!" POS;
    parser_conn_test "postive enhancer"
      "VADER is very smart, handsome, and funny." POS;
    parser_conn_test "postive captials"
      "VADER is VERY SMART, handsome, and FUNNY." POS;
    parser_conn_test "postive capitals and punctuation"
      "VADER is VERY SMART, handsome, and FUNNY!!!" POS;
    parser_conn_test "postive emphasis"
      "VADER is VERY SMART, uber handsome, and FRIGGIN FUNNY!!!" POS;
    parser_conn_test "positive negation"
      "VADER is not smart, handsome, nor funny." NEG;
    parser_conn_test "positive simple" "The book was good." POS;
    parser_conn_test "complex positive" "At least it isn't a horrible book." POS;
    parser_conn_test "postive diminisher" "The book was only kind of good." POS;
    parser_conn_test "longer sentence"
      "The plot was good, but the characters are uncompelling and the dialog \
       is not great."
      NEG;
    parser_conn_test "negative mispelling" "Today SUX!" NEG;
    parser_conn_test "social media slang"
      "Today only kinda sux! But I'll get by, lol" POS;
    parser_conn_test "emoticons" "Make sure you :) or :D today!" POS;
    parser_conn_test "negative negation" "Not bad at all" POS;
    parser_conn_test "random positive" "the dog is very cute and fluffy" POS;
    parser_conn_test "random positive"
      "last week's book was very interesting and profound" POS;
    parser_conn_test "random positive"
      "Superman is so cool because he saves people" POS;
    parser_conn_test "random positive"
      "I saw your mom yesterday and she was very nice to me" POS;
    parser_conn_test "random positive"
      "This orange is not sour at all, yummy and tasy and sweet" POS;
    parser_conn_test "random positive" "food tastes delicious when eaten" POS;
    parser_conn_test "random negative" "eww that lunch was gross and yucky" NEG;
    parser_conn_test "random negative"
      "my house is super ugly and messy right now" NEG;
    parser_conn_test "random negative"
      "I hate when people brush their teeth while walking" NEG;
    parser_conn_test "random negative" "today was a very bad no good dady" NEG;
    parser_conn_test "random negative"
      "water splashed all over me, not cool, very unfortunate, super unhappy \
       and mad!!!"
      NEG;
    parser_conn_test "random negative"
      "I do not like the cat that scratched me and attacked me" NEG;
    parser_conn_test "random neutral" "I exist" NEU;
    parser_conn_test "random neutral" "that thing" NEU;
  ]

let portfolio_ticker_test name portfolio expec_tickers =
  "portfolio tickers: " ^ name >:: fun _ ->
  assert_equal expec_tickers
    (list_of_tickers portfolio)
    ~printer:(pp_list pp_string)

let portfolio_stocks_test name portfolio expec_tickers =
  "portfolio tickers: " ^ name >:: fun _ ->
  assert_equal expec_tickers (list_of_stocks portfolio)

let portfolio_worth_test name portfolio expec_worth =
  "portfolio worth: " ^ name >:: fun _ ->
  assert_equal expec_worth (net_worth portfolio) ~printer:string_of_float

let portfolio_liquidity_test name portfolio expec_liq =
  "portfolio worth: " ^ name >:: fun _ ->
  assert_equal expec_liq (liquidity portfolio) ~printer:string_of_float

let portfolio_equal_test name portfolio expec_portfolio =
  "portfolio equal: " ^ name >:: fun _ ->
  assert_equal 0 (compare expec_portfolio portfolio) ~printer:string_of_int

let portfolio_tests =
  [
    portfolio_ticker_test "empty portfolio has no tickers" empty_portfolio [];
    portfolio_stocks_test "empty portfolio has no stocks" empty_portfolio [];
    portfolio_worth_test "empty portfolio has no worth" empty_portfolio 0.;
    portfolio_liquidity_test "empty portfolio has no liquidity" empty_portfolio
      0.;
    portfolio_equal_test "empty portfolio is still empty when sold"
      empty_portfolio (refresh empty_portfolio);
    portfolio_equal_test "empty portfolio is still empty when copied"
      empty_portfolio (copy empty_portfolio);
    portfolio_equal_test "empty portfolio is still empty when sold"
      empty_portfolio (sell_all empty_portfolio);
  ]

let suite =
  "CamelStonks Test Suite"
  >::: List.flatten
         [ scraper_tests; cashset_tests; parser_tests; portfolio_tests ]

let _ = run_test_tt_main suite
