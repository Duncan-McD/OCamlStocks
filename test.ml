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
      Parser (OUnit)
      Optimizer (OUnit + manually)
      Algorithm (OUnit + manually)
      Portfolio ()

    This testing approach aimed to rigorously test all aspects of the system
    that were possible to test. The tests used maneuvered around the system's
    reliance on present time values that were constantly changing, like stock
    values or reddit posts. In order to be assured of each modules' validity we
    not only tested in isolation but also required testing be done between
    modules as certain aspects relied upon the validity of earlier modules. The
    various use of different test case development systems (i.e. glass box or
    black box) were used whenever possible to gain maximum testing exposure and
    bisect coverage.

    Rubric: (TODO: remove rubric when completed test plan)
    -4: The test plan is missing.
    -1: The test plan does not explain which parts of the system were automatically tested by OUnit vs. manually tested.
    -1: The test plan does not explain what modules were tested by OUnit and how test cases were developed (black box, glass box, randomized, etc.).
    -1: The test plan does not provide an argument for why the testing approach demonstrates the correctness of the system.
 *)

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
 
 let scraper_name_test name json_file expec_name =
   let subreddit = scrape_json ~amount:1 ("testing_files/" ^ json_file) in
   let name = subreddit_name subreddit in
   "subreddit name: " ^ name >:: fun _ -> 
     assert_equal expec_name name ~printer:pp_string
 
 let scraper_posts_test name ?(amount = 24) json_file attr_func expec_attr printer =
   let scraped_body =
     match posts (scrape_json ~amount ("testing_files/" ^ json_file)) with
     | last_post :: t -> attr_func last_post
     | [] -> failwith "No posts scraped"
   in
   name >:: fun _ -> assert_equal expec_attr scraped_body ~printer:printer
 
 let scraper_title_test name ?(amount = 24) json_file expec_attr =
   scraper_posts_test ("subreddit post title: " ^ name) ~amount json_file 
     Scraper.title expec_attr pp_string
     
 let scraper_body_test name ?(amount = 24) json_file expec_attr =
   scraper_posts_test ("subreddit post body: " ^ name) ~amount json_file 
     Scraper.body expec_attr pp_string
 
 let scraper_score_test name ?(amount = 24) json_file expec_attr =
   scraper_posts_test ("subreddit post score: " ^ name) ~amount json_file 
     Scraper.score expec_attr string_of_int
 
 let scraper_ratio_test name ?(amount = 24) json_file expec_attr =
   scraper_posts_test ("subreddit post upvote ratio: " ^ name) ~amount json_file 
     Scraper.upvote_ratio expec_attr string_of_float
 
 let scraper_excep_test name ?(amount = 25) json_file =
   "scraper exception: " ^ name >:: fun _ -> assert_raises 
     (TooManyPostsRequested (amount - 24))
     (fun () -> scrape_json ~amount ("testing_files/" ^ json_file))
 
 let scraper_tests =
   [
     scraper_name_test "r/stocks hot" "stocks-hot.json" "r/stocks";
     scraper_name_test "r/stocks new" "stocks-new.json" "r/stocks";
     scraper_name_test "r/stocks rising" "stocks-rising.json" "r/stocks";
     scraper_name_test "r/stocks top alltime" "stocks-top-alltime.json" "r/stocks";
     scraper_name_test "r/stocks top today" "stocks-top-today.json" "r/stocks";
     scraper_name_test "r/investing hot" "investing-hot.json" "r/investing";
     scraper_name_test "r/wallstreetbets hot" "wallstreetbets-hot.json" "r/wallstreetbets";
     scraper_title_test "post 1 in r/stocks hot" ~amount:1 "stocks-hot.json" "Rate My Portfolio - r/Stocks Quarterly Thread March 2021";
     scraper_title_test "post 2 in r/stocks hot" ~amount:2 "stocks-hot.json" "r/Stocks Daily Discussion &amp; Fundamentals Friday May 14, 2021";
     scraper_title_test "post 7 in r/stocks hot" ~amount:7 "stocks-hot.json" "If your goal is to live off your stock returns, how much are you looking to invest?";
     scraper_title_test "post 1 in r/stocks new" ~amount:1 "stocks-new.json" "Does anyone know of publicly traded companies that specialize/ have relations with nanotechnology?";
     scraper_title_test "post 2 in r/stocks new" ~amount:2 "stocks-new.json" "Should I invest more than half of my money as a first year college student living with their parents?";
     scraper_title_test "post 11 in r/stocks new" ~amount:11 "stocks-new.json" "Trading simulator?";
     scraper_title_test "post 1 in r/stocks rising" ~amount:1 "stocks-rising.json" "Dymon Asia Capital purchases $100,000,000 worth of Palantir ($PLTR) shares";
     scraper_title_test "post 2 in r/stocks rising" ~amount:2 "stocks-rising.json" "Trading simulator?";
     scraper_title_test "post 15 in r/stocks rising" ~amount:15 "stocks-rising.json" "What is the general sentiment about the stock market?";
     scraper_title_test "post 1 in r/stocks top alltime" ~amount:1 "stocks-top-alltime.json" "It's fucking awful seeing the \"Silver\" misinformation campaign everywhere I look";
     scraper_title_test "post 2 in r/stocks top alltime" ~amount:2 "stocks-top-alltime.json" "Companies try to prevent people from trading GME and AMC";
     scraper_title_test "post 18 in r/stocks top alltime" ~amount:18 "stocks-top-alltime.json" "Sign The Petition: Retail Investors Demand Market Transparency! Make the Hedge Funds report their Shorts!";
     scraper_title_test "post 1 in r/stocks top today" ~amount:1 "stocks-top-today.json" "Why would anyone invest in IBM? Losing money since 2013?";
     scraper_title_test "post 2 in r/stocks top today" ~amount:2 "stocks-top-today.json" "What's causing ICLN to gradually decline since the January? Do you reckon it will recover?";
     scraper_title_test "post 9 in r/stocks top today" ~amount:9 "stocks-top-today.json" "What are some aggressive/risky ETFs";
     scraper_title_test "post 1 in r/investing hot" ~amount:1 "investing-hot.json" "Daily General Discussion and spitballin thread - May 16, 2021";
     scraper_title_test "post 2 in r/investing hot" ~amount:2 "investing-hot.json" "Daily Advice Thread - All basic help or advice questions must be posted here.";
     scraper_title_test "post 20 in r/investing hot" ~amount:20 "investing-hot.json" "(US) How will a lazy portfolio handle the inflation we are starting to see?";
     scraper_title_test "post 1 in r/wallstreetbets hot" ~amount:1 "wallstreetbets-hot.json" "What Are Your Moves Tomorrow, May 17, 2021";
     scraper_title_test "post 2 in r/wallstreetbets hot" ~amount:2 "wallstreetbets-hot.json" "Government hates this simple tax trick";
     scraper_title_test "post 14 in r/wallstreetbets hot" ~amount:14 "wallstreetbets-hot.json" "This is not economic boom, this is just recovery !!!!";
     scraper_body_test "post 1 in r/stocks hot" ~amount:1 "stocks-hot.json" "Please use this thread to discuss your portfolio, learn of other stock tickers, and help out users by giving constructive criticism.\n\nWhy quarterly?  Public companies report earnings quarterly; many investors take this as an opportunity to rebalance their portfolios.  We highly recommend you do some reading:  A list of [relevant posts &amp; book recommendations.](https://www.reddit.com/r/stocks/wiki/index#wiki_relevant_posts_.26amp.3B_book_recommendations)\n\nYou can find stocks on your own by using a scanner like your broker's or [Finviz.](https://finviz.com/screener.ashx)  To help further, here's a list of [relevant websites.](https://www.reddit.com/r/stocks/wiki/index#wiki_relevant_websites.2Fapps)\n\nIf you don't have a broker yet, see our [list of brokers](https://www.reddit.com/r/stocks/wiki/index#wiki_brokers_for_investing) or search old posts.  If you haven't started investing or trading yet, then setup your [paper trading.](https://www.reddit.com/r/stocks/wiki/index#wiki_is_there_a_way_to_practice.3F)\n\nBe aware of [Business Cycle Investing](https://eresearch.fidelity.com/eresearch/markets_sectors/sectors/si_business_cycle.jhtml?tab=sibusiness) which Fidelity issues updates to the state of global business cycles every 1 to 3 months (note: Fidelity changes their links often, so search for it since their take on it is enlightening).  [Investopedia's take on the Business Cycle](https://www.investopedia.com/articles/investing/061316/business-cycle-investing-ratios-use-each-cycle.asp) and their [video.](https://www.investopedia.com/video/play/business-cycle/)\n\nIf you need help with a falling stock price, check out Investopedia's [The Art of Selling A Losing Position](https://www.investopedia.com/articles/02/022002.asp) and their [list of biases.](https://www.investopedia.com/articles/stocks/08/capital-losses.asp)\n\nHere's a list of all the [previous portfolio stickies.](https://www.reddit.com/r/stocks/search?q=author%3Aautomoderator+%22Rate+My+Portfolio%22+-+r%2FStocks+Quarterly+Thread&amp;restrict_sr=on&amp;include_over_18=on&amp;sort=new&amp;t=all)";
     scraper_body_test "post 2 in r/stocks hot" ~amount:2 "stocks-hot.json" "This is the daily discussion, so anything stocks related is fine, but the theme for today is on fundamentals, but if fundamentals aren't your thing then just ignore the theme and/or [post your arguments against fundamentals here](https://www.reddit.com/r/stocks/search?q=author%3Aautomoderator+%22r%2Fstocks+semiannual+arguments+against+TA%22&amp;restrict_sr=on&amp;include_over_18=on&amp;sort=new&amp;t=all) and not in the current post.\n\nSome helpful day to day links, including news:\n\n* [Finviz](https://finviz.com/quote.ashx?t=spy) for charts, fundamentals, and aggregated news on individual stocks\n* [Bloomberg market news](https://www.bloomberg.com/markets)\n* StreetInsider news:\n  * [Market Check](https://www.streetinsider.com/Market+Check) - Possibly why the market is doing what it's doing including sudden spikes/dips\n  * [Reuters aggregated](https://www.streetinsider.com/Reuters) - Global news\n\n-----\n\nMost fundamentals are updated every 3 months due to the fact that corporations release earnings reports every quarter, so traders are always speculating at what those earnings will say, and investors may change the size of their holdings based on those reports.  Expect a lot of volatility around earnings, but it usually doesn't matter if you're holding long term, but keep in mind the importance of earnings reports because a trend of declining earnings or a decline in some other fundamental will drive the stock down over the long term as well.\n\nSee the following word cloud and click through for the wiki:\n\n[Market Cap - Shares Outstanding - Volume - Dividend - EPS - P/E Ratio - EPS Q/Q - PEG - Sales Q/Q - Return on Assets (ROA) - Return on Equity (ROE) - BETA - SMA - quarterly earnings](https://www.reddit.com/r/stocks/wiki/fundamentals-themed-post)\n\nIf you have a basic question, for example \"what is EBITDA,\" then google \"investopedia EBITDA\" and click the Investopedia article on it; do this for everything until you have a more in depth question or just want to share what you learned.\n\nUseful links:\n\n* [Investopedia page](https://www.investopedia.com/fundamental-analysis-4689757/) on fundamental analysis including [Discounted Cash Flow](https://www.investopedia.com/university/dcf/) analysis; see [definition here](https://www.investopedia.com/terms/d/dcf.asp) and read [their PDF on the topic.](http://i.investopedia.com/inv/pdf/tutorials/fundamentalanalysis_intro.pdf)\n* [FINVIZ](https://finviz.com/quote.ashx?t=aapl) for fundamental data, charts, and aggregated news\n* [Earnings Whisper](https://www.earningswhispers.com/stocks/aapl) for earnings details\n\nSee our past [daily discussions here.](https://www.reddit.com/r/stocks/search?q=author%3Aautomoderator+%22r%2Fstocks+daily+discussion%22&amp;restrict_sr=on&amp;sort=new&amp;t=all)  Also links for:  [Technicals](https://www.reddit.com/r/stocks/search?q=author%3Aautomoderator+title%3Atechnicals&amp;restrict_sr=on&amp;include_over_18=on&amp;sort=new&amp;t=all) Tuesday, [Options Trading](https://www.reddit.com/r/stocks/search?q=author%3Aautomoderator+title%3Aoptions&amp;restrict_sr=on&amp;include_over_18=on&amp;sort=new&amp;t=all) Thursday, and [Fundamentals](https://www.reddit.com/r/stocks/search?q=author%3Aautomoderator+title%3Afundamentals&amp;restrict_sr=on&amp;include_over_18=on&amp;sort=new&amp;t=all) Friday.";
     scraper_body_test "post 7 in r/stocks hot" ~amount:7 "stocks-hot.json" "I was looking into how much I wanna have invested in stocks before I go part time somewhere in the future. I don't really spend a lot of money so most of my income just sits there, with some of it invested.\n\nI know I could easily live off 2k bucks a month, but obviously that takes quite the investment to get passively each month.\n\nWhat are you guys aiming for? What's your strategy to achieve your goal?";
     (* scraper_body_test "post 1 in r/stocks new" ~amount:1 "stocks-new.json" "I was wondering this because the market for nanotechnology is being talked about more and more and expected to rise a decent amount within the span of 5 years and I\u2019m looking for some companies to research and invest some money in."; *)
     scraper_body_test "post 2 in r/stocks new" ~amount:2 "stocks-new.json" "I know that as a general rule most people tend to save the majority of their money to cover their living expenses and only invest any spare capacity of money that they can afford to lose, but considering that I live at home with little to no living expenses, should I take more of a risk with my money and set aside a larger chunk of my savings for investments? I have never invested in the stock market, so my experience is limited and I'm still learning how to analyse trends and financial statements and all that, so I'm a little unsure of how much risk I should really take considering my inexperience. My budget is pretty low as well at just around 5,000 AUD, so in order to really make any worthwhile returns I probably need to invest around half of that. What do you guys think?";
     (* scraper_body_test "post 11 in r/stocks new" ~amount:11 "stocks-new.json" "I\u2019m looking for a good simulator that allows me to replay the stocks for that day and also simulate buys and sells. \n\n1. I\u2019ve used think or swim for this the problem is they don\u2019t let you do that days stocks you have to wait 5 or 6 trading days before you can go back and replay that particular day. I like it I just don\u2019t want to wait a week to be able to replay my watchlist.\n\n2. Trading view - I\u2019ve used this which lets me replay for same day but it doesn\u2019t simulate buys and sells.\n\nI\u2019m just trying to find something that will allow me to do same day replay WITH buys and sells. \n\nAny suggestions?"; *)
     scraper_body_test "post 1 in r/stocks rising" ~amount:1 "stocks-rising.json" "http://pdf.secdatabase.com/44/0001172661-21-001130.pdf\n\nSECURITIES AND EXCHANGE COMMISSION\n\nFORM 13F-HR\n\nInitial quarterly Form 13F holdings report filed by institutional managers\n\nFiling Date: 2021-05-14\nPeriod of Report: 2021-03-31 SEC Accession No. 0001172661-21-001130\n\nFILER:\n\nDYMON ASIA CAPITAL (SINGAPORE) PTE. LTD. CIK:1672142\n\nIRS No.: 000000000\nState of Incorp.:U0\nFiscal Year End: 1231\nType: 13F-HR\nAct: 34\nFile No.: 028-17414\nFilm No.: 21922569\n\nMailing Address:\n\nONE TEMASEK AVENUE #11-01 MILLENIA TOWER SINGAPORE U0 039192\n\nBusiness Address:\n\nONE TEMASEK AVENUE #11-01 MILLENIA TOWER SINGAPORE U0 039192 65-67051666";
     (* scraper_body_test "post 2 in r/stocks rising" ~amount:2 "stocks-rising.json" "I\u2019m looking for a good simulator that allows me to replay the stocks for that day and also simulate buys and sells. \n\n1. I\u2019ve used think or swim for this the problem is they don\u2019t let you do that days stocks you have to wait 5 or 6 trading days before you can go back and replay that particular day. I like it I just don\u2019t want to wait a week to be able to replay my watchlist.\n\n2. Trading view - I\u2019ve used this which lets me replay for same day but it doesn\u2019t simulate buys and sells.\n\nI\u2019m just trying to find something that will allow me to do same day replay WITH buys and sells. \n\nAny suggestions?"; *)
     (* scraper_body_test "post 15 in r/stocks rising" ~amount:15 "stocks-rising.json" "Hello all,\n\nI\u2019m curious to hear what other redditor\u2019s opinions are on the current market. Do you see it continue to rise within the next year? Do you expect a crash? A correction? What are your thoughts on the state of the housing market over the next two years? How bad of a hit do you expect stocks/housing to take when rates eventually go up. \n\nI understand this all speculation and no one knows what will happen, just curious to see if anyone has any opinions and why, based on what you\u2019re seeing.\n\nI personally believe stocks will keep going up with a few corrections but no big crash. I think the big one will happen within the next 5-10 years due to a financial crisis. As for the housing frenzy, I believe that will burn out in a couple years and drive prices down ~20%.\n\nYour thoughts?"; *)
     (* scraper_body_test "post 1 in r/stocks top alltime" ~amount:1 "stocks-top-alltime.json" "\u26a0\ufe0f\u26a0\ufe0f\u26a0\ufe0f ***DON'T BUY SILVER, IT'S A TRAP***\u26a0\ufe0f\u26a0\ufe0f\u26a0\ufe0f\n\nThey're talking on CNBC as if people on Reddit are actually squeezing silver. It's fucking absurd, they're practically encouraging it. \n\nThey're like, \"Wow, these redditors are squeezing silver, how cool\" actually fucking encouraging it. \n\nLiterally scum\n\nEdit: Should have mentioned, it's literally fucking impossible to squeeze silver. It's not shorted at all. Hedge funds and Citadel hold lots of Long positions in it, not shorts. Buying it would be playing right into their hands.\n\nBuying silver will make you likely lose money and absolutely give it to the hedge funds and Citadel.\n\nBy Silver, I mean $SLV, *I know nothing about phisical silver*. For anybody confused\n\nEdit 2: If you bought $SLV months or years ago and made a profit, that's fantastic. This post is just saying that you should not buy silver right now.\n\nThis isn't financial advice, I am mentally challenged"; *)
     scraper_body_test "post 2 in r/stocks top alltime" ~amount:2 "stocks-top-alltime.json" "Not sure about the other trading apps but Trading212 prevents people now from buying shares. Quote:\n\n- Warning! In the interest of mitigating risk for our clients, we have temporarily placed GameStop and AMC Entertainment in reduce-only mode as highly unusual volumes have led to an unprecedented market environment. New positions cannot be opened, existing ones can be reduced or closed. -\n\nNot sure if they are really concerned about their customers, or they've been lobbied by hedge funds to prevent ordinary people from destroying them. I don't care about GME and AMC, I have no position, but now I am angry for this decision. They always go against the poor individuals and let the billionaires save their asses. No one saves us when we go bankrupt by them.\n\nLet that sink in\n\nEdit: thank you for all the rewards and comments! What a great community we are!";
     scraper_body_test "post 18 in r/stocks top alltime" ~amount:18 "stocks-top-alltime.json" "**Sign the petition here: https://www.change.org/sec-amend-13-f**\n\n\nRetail Investors demand more visibility into institutional trading and borrowing. Anyone investing over 1 billion dollars (i.e. hedge funds and other investment institutions) is required to disclose their holdings to promote transparency in our markets - it's called Form 13-F. **But did you know that they only need to disclose it 4 times a year? And did you know its published with a 1 month delay? And did you know that they don't need to disclose all of their positions?**\n\n\nWe the people are asking for a re-evaluation of transparency requirements for Institutional Investors. We have access to technology and data that gives us new sophistication - and are beginning to understand there is a tremendous disparity in access between retail and institutional investors, and are concerned that this access is being used against us, in ways that we genuinely worry could be in flagrant violation of Securities Laws. We believe that with better access to institutional trading data, retail investors can better participate in the market when making buying and selling decisions.\n\n\n**According to Form 13F (https://www.sec.gov/files/form13f.pdf), Institutional Investors only need to disclose their positions 4 times a year. Why?**\n\n\n*Filing of Form 13F. A Manager must file a Form 13F report with the Commission within 45 days after the end of each calendar year and each of the first three calendar quarters of each calendar year. As required by Section 13(f)(5) of the Exchange Act, a Manager which is a bank, the deposits of which are insured in accordance with the Federal Deposit Insurance Act, must file with the appropriate regulatory agency for the bank a copy of every Form 13F report filed with the Commission pursuant to this subsection by or with respect to such bank. Filers who file Form 13F electronically can satisfy their obligation to file with other regulatory agencies by sending (a) a paper copy of the EDGAR filing (provided the Manager removes or blanks out the confidential access codes); (b) the filing in electronic format, if the regulatory agency with which the filing is being made has made provisions to receive filings in electronic format;*\n\n\n**In your FAQ (https://www.sec.gov/divisions/investment/13ffaq.htm), it is clear Institutional Investors are not required to disclose short positions. Why?**\n\n\n*Question 41\nQ: What about short positions?\nA: You should not include short positions on Form 13F. You also should not subtract your short position(s) in a security from your long position(s) in that same security; report only the long position.*\n\n\n**Contact the SEC and let them know retail investors demand increased transparency (https://www.sec.gov/contact-information/sec-directory)**";
     scraper_body_test "post 1 in r/stocks top today" ~amount:1 "stocks-top-today.json" "I don't get it.  What would be the incentive of investing in a company like IBM?  The stock price has been depreciating since 2013.\n\nI realize they have a nice 4.5% dividend, but does that make up for losing your principle investment?\n\nWhat am I failing to see?";
     scraper_body_test "post 2 in r/stocks top today" ~amount:2 "stocks-top-today.json" "When I look at the graph of ICLN in the last 10 years, the there's been barely any growth in it until the pandemic hit and suddenly after April 2020, it started growing and kept growing until the beginning of January 2021.\n\nPerhaps this sudden growth is down to the optimism that Biden will promote renewable energy, along with the US government's cash injection to boost the economy during the pandemic.\n\nHowever, now that ICLN is back to its normal trend (4/2011 - 3/2020), do you reckon it will come back to north trending graph any time soon? I bought ICLN at the end of February and now it's more than 27% down. Do you reckon it will grow back any time soon? Or should I give up on it?";
     (* scraper_body_test "post 9 in r/stocks top today" ~amount:9 "stocks-top-today.json" "What is currently considered an agressive ETF?\n\nI am looking for an aggressive/risky ETF to pair with VT to hold long term.\n\nI was at first looking at growth ETFs like VUG but then looked at value ETFs such as VIOV and VTV.\n\nBut I\u2019m wondering what\u2019s a good example of an aggressive/risky ETF I can hold a little bit of for the long term."; *)
     scraper_body_test "post 1 in r/investing hot" ~amount:1 "investing-hot.json" "Have a general question?  Want to offer some commentary on markets?  Maybe you would just like to throw out a neat fact that doesn't warrant a self post?  Feel free to post here! \n\nThis thread is for: \n\n* General questions\n* Your personal commentary on markets\n* Opinion gathering on a given stock\n* Non advice beginner questions\n\nKeep in mind that this subreddit, and this thread, is not an appropriate venue for questions that should be directed towards your broker's customer support or google.  \n\nIf you would like to ask a question about your personal situation or if you are asking for advice please keep these posts in the daily advice thread as that thread is more well suited for those questions.\n\nAny posts that should be comments in this thread will likely be removed.";
     scraper_body_test "post 2 in r/investing hot" ~amount:2 "investing-hot.json" "If your question is \"I have $10,000, what do I do?\" or other \"advice for my personal situation\" questions, you should include relevant information, such as the following:\n\n* How old are you? What country do you live in?  \n* Are you employed/making income? How much?  \n* What are your objectives with this money? (Buy a house? Retirement savings?)  \n* What is your time horizon? Do you need this money next month? Next 20yrs?  \n* What is your risk tolerance? (Do you mind risking it at blackjack or do you need to know its 100% safe?)  \n* What are you current holdings? (Do you already have exposure to specific funds and sectors? Any other assets?)  \n* Any big debts (include interest rate) or expenses?  \n* And any other relevant financial information will be useful to give you a proper answer.  \n\nPlease consider consulting our FAQ first - https://www.reddit.com/r/investing/wiki/faq\nAnd our [side bar](https://www.reddit.com/r/investing/about/sidebar) also has useful resources.  \n\nBe aware that these answers are just opinions of Redditors and should be used as a starting point for your research. You should strongly consider seeing a registered financial rep before making any financial decisions!";
     scraper_body_test "post 20 in r/investing hot" ~amount:20 "investing-hot.json" "For example, how will a 3-fund or 4-fund portfolio weather the increased inflation we are starting to see?\n\nFor example, with the following vanguard etfs...\n\nvti vxus vnq bnd\n\nI would guess that vxus would do well purely from a US dollar inflation perspective since it is international and therefore priced on other currencies.\n\nI'd expect vnq to do well as well with a weakening dollar.\n\nWith vti, would it be the same? Or would investors move money elsewhere from the s&amp;p 500 companies?\n\nWith bnd, I'd expect it to struggle with inflation?\n\nAny recommendations for adjustment in a portfolio? I see varied opinions online about inflation protected investments and also vtv.";
     scraper_body_test "post 1 in r/wallstreetbets hot" ~amount:1 "wallstreetbets-hot.json" "Your daily trading discussion thread. Please keep the shitposting to a minimum. \n\n^Navigate ^WSB|^We ^recommend ^best ^daily ^DD\n:--|:--                                 \n**DD** | [All](https://reddit.com/r/wallstreetbets/search?sort=new&amp;restrict_sr=on&amp;q=flair%3ADD) / [**Best Daily**](https://www.reddit.com/r/wallstreetbets/search?sort=top&amp;q=flair%3ADD&amp;restrict_sr=on&amp;t=day) / [Best Weekly](https://www.reddit.com/r/wallstreetbets/search?sort=top&amp;q=flair%3ADD&amp;restrict_sr=on&amp;t=week)\n**Discussion** | [All](https://reddit.com/r/wallstreetbets/search?sort=new&amp;restrict_sr=on&amp;q=flair%3ADiscussion) / [**Best Daily**](https://www.reddit.com/r/wallstreetbets/search?sort=top&amp;q=flair%3ADiscussion&amp;restrict_sr=on&amp;t=day) / [Best Weekly](https://www.reddit.com/r/wallstreetbets/search?sort=top&amp;q=flair%3ADiscussion&amp;restrict_sr=on&amp;t=week)\n**YOLO** | [All](https://reddit.com/r/wallstreetbets/search?sort=new&amp;restrict_sr=on&amp;q=flair%3AYOLO) / [**Best Daily**](https://www.reddit.com/r/wallstreetbets/search?sort=top&amp;q=flair%3AYOLO&amp;restrict_sr=on&amp;t=day) / [Best Weekly](https://www.reddit.com/r/wallstreetbets/search?sort=top&amp;q=flair%3AYOLO&amp;restrict_sr=on&amp;t=week)\n**Gain** | [All](https://reddit.com/r/wallstreetbets/search?sort=new&amp;restrict_sr=on&amp;q=flair%3AGain) / [**Best Daily**](https://www.reddit.com/r/wallstreetbets/search?sort=top&amp;q=flair%3AGain&amp;restrict_sr=on&amp;t=day) / [Best Weekly](https://www.reddit.com/r/wallstreetbets/search?sort=top&amp;q=flair%3AGain&amp;restrict_sr=on&amp;t=week)\n**Loss** | [All](https://reddit.com/r/wallstreetbets/search?sort=new&amp;restrict_sr=on&amp;q=flair%3ALoss) / [**Best Daily**](https://www.reddit.com/r/wallstreetbets/search?sort=top&amp;q=flair%3ALoss&amp;restrict_sr=on&amp;t=day) / [Best Weekly](https://www.reddit.com/r/wallstreetbets/search?sort=top&amp;q=flair%3ALoss&amp;restrict_sr=on&amp;t=week)\n\n[Weekly Earnings Discussion Thread](https://www.reddit.com/r/wallstreetbets/search?sort=new&amp;restrict_sr=on&amp;q=flair%3A%22Earnings%20Thread%22)\n\n**Read the [rules](https://www.reddit.com/r/wallstreetbets/wiki/contentguide) and make sure other people follow them.**\n\nTry [No Meme Mode](https://www.reddit.com/r/wallstreetbets/search/?q=-flair%3AMeme%20-flair%3ASatire%20-flair%3AShitpost&amp;restrict_sr=1&amp;t=day&amp;sort=hot), also accessible through the top bar.\n\nFollow [@Official_WSB](https://twitter.com/Official_WSB) on Twitter, all other accounts are impersonators.";
     scraper_body_test "post 2 in r/wallstreetbets hot" ~amount:2 "wallstreetbets-hot.json" "";
     (* scraper_body_test "post 14 in r/wallstreetbets hot" ~amount:14 "wallstreetbets-hot.json" " The recent NFIB survey suggests we are only in an economic recovery, not an expansion. \n\n While the mainstream media overlooks the NFIB data, they really shouldn\u2019t. There are currently 30.7 million small businesses in the United States[*.*](https://cdn.advocacy.sba.gov/wp-content/uploads/2019/04/23142610/2019-Small-Business-Profiles-States-Territories.pdf) Small businesses *(defined as fewer than 500 employees)* **account for 99% of all enterprises, employ 60 million people, and account for nearly 70% of employment.** The chart below shows the breakdown of firms and jobs from the 2019 Census Bureau Data. \n\n&amp;#x200B;\n\nhttps://preview.redd.it/zw4426oprhz61.png?width=739&amp;format=png&amp;auto=webp&amp;s=5692ffc300115b9a511e1647232fb8032c5f958b\n\n Let\u2019s dig in. \n\n \n\n### NFIB Shows Confidence Drop\n\n**The April survey showed a slight increase in the over \u201cconfidence\u201d index to 99.8 vs. 98.2 in March. The reading is substantially lower than the August 2018 reading of 108.8.** Notably, despite a year-long economic recovery from the Q2-2020 lows, the level of confidence remains near recessionary levels.\n\n&amp;#x200B;\n\nhttps://preview.redd.it/w6h4saayrhz61.png?width=907&amp;format=png&amp;auto=webp&amp;s=05dc1256be3e935048e02e19f4c57d0e09cb3e16\n\n Not surprisingly, given the economy is still struggling with the current recovery,   \u201cconfidence\u201d                               remains extremely weak. \n\n The importance of *\u201cweak confidence\u201d* also affects the *\u201crisk\u201d* business owners will take concerning capital expenditures, employment, and sales. **Importantly,\u00a0this is a** ***\u201csentiment\u201d*** **based survey.** Such is a crucial concept to understand as ***\u201cplanning\u201d*** **to do something and** ***\u201cdoing\u201d*** **it can be very different.** \n\n \n\n### An Economic Boom Will Require Participation\n\nCurrently, many analysts expect a massive economic boom in 2021. The basis of those expectations is massive *\u201cpent-up\u201d* demand as the economy reopens.\n\n**I would agree with that expectation had there been no stimulus programs or expanded unemployment benefits.** Those inflows allowed individuals to spend during a recession where such would not usually be the case. Those artificial inputs dragged forward future or *\u201cpent-up\u201d* consumption into the present.\n\nHowever, the NFIB survey also suggests much the same.\n\n**Small businesses are susceptible to economic downturns and don\u2019t have access to public markets for debt or secondary offerings. As such, they tend to focus heavily on operating efficiencies and profitability.**\n\n**If businesses were expecting a massive surge in** ***\u201cpent up\u201d*** **demand, they would be doing several things to prepare for it.** Such includes planning to increase capital expenditures to meet expected demand. Unfortunately, those expectations peaked in 2018 and are dropping back to the March 2020 lows.\n\n \n\n### Employment To Remain Weak\n\nIf small businesses think the economy is *\u201cactually\u201d* improving over the longer term, they would also be increasing employment. Given business owners are always optimistic, over-estimating hiring plans is not surprising. However, reality occurs when actual *\u201cdemand\u201d* meets its operating cash flows.\n\nTo increase employment, which is the single most considerable cost to any business, you need two things:\n\n1. *Confidence the economy is going to continue to grow in the future, which leads to;*\n2. *The increase in the production of goods or services requiring increased employment to meet growing demand.*\n\n**Currently, there is little expectation for a strongly recovering economy.**\n\n&amp;#x200B;\n\nhttps://preview.redd.it/qgpd1rqeshz61.png?width=804&amp;format=png&amp;auto=webp&amp;s=cdf5af23ed3d590678c02894f1f43198031bf874\n\n Businesses understand that the stimulus [***\u201cpulled forward\u201d*** ](https://realinvestmentadvice.com/the-problem-of-pulling-forward-sales-revenue/)much of the current demand. **As such, they can not commit to the** ***\u201ccosts\u201d*** **of** ***\u201clong-term employment\u201d*** **for a** ***\u201cshort-term\u201d*** **artificial economic boost.** \n\n \n\n### The Big Hit Is Coming\n\n**Retail sales make up about 40% of personal consumption expenditures (PCE), which comprises roughly 70% of the GDP calculation.**\u00a0Each month the NFIB tracks both actual sales over the last quarter and expected sales over the next quarter. There is always a significant divergence between expectations and reality.\n\n&amp;#x200B;\n\nhttps://preview.redd.it/9vqsosfkshz61.png?width=876&amp;format=png&amp;auto=webp&amp;s=d46a184ce87c222fc678ef69b207c8956ba94d17\n\n \n\nWhile stimulus may lead to a short-term boost in consumption, the impact of higher taxes, more regulations, and weak employment growth will suppress consumption longer-term.\n\nDespite economic headlines, the recovery of sales for small businesses has been less than *\u201cbooming.\u201d*\n\n \n\n### A Recovery Versus An Expansion\n\nWith this background, it is easier to understand why the recent exuberance in chasing small-cap stocks may be premature. **While small-cap companies do historically perform well coming out of recession, the basis was an organic recovery cycle of increasing productivity.**\n\nCurrently, the run-up remains the assumption that the stimulus-fueled recovery is sustainable. **Such is only the case if the stimulus becomes a regular benefit and increases in size annually. However, since** [***deficit-based spending is deflationary***](https://realinvestmentadvice.com/macroview-a-vaccine-and-the-new-new-normal/)***,***\u00a0the outcome will fall well short of expectations.\n\n&gt;*\u201cin 1998, the Federal Reserve \u201ccrossed the \u2018Rubicon,\u2019 whereby lowering interest rates failed to stimulate economic growth or inflation as the \u2018debt burden\u2019 detracted from it. When compared to the total debt of the economy, monetary velocity shows the problem facing the Fed.\u201d*\n\nhttps://preview.redd.it/2budem4qshz61.png?width=1025&amp;format=png&amp;auto=webp&amp;s=d3597e6ad0a525708b14a84e4b20ec242d56f381\n\n **Such is a critical point as it relates to small-cap companies given their high correlation to small-business confidence.** The correlation between the small-cap index (Russell 2000) and underlying confidence is very high. Given the annual change in \u201cconfidence\u201d is declining, it is not surprising to see that small-cap stocks recently peaked. \n\n&amp;#x200B;\n\nhttps://preview.redd.it/1fi4wzptshz61.png?width=774&amp;format=png&amp;auto=webp&amp;s=8a30c19ab1f3f10f823bbcb3eab9b287201d622e\n\n \n\n### Conclusion\n\nGiven that debt-driven government spending programs have a dismal history of providing the economic growth promised, disappointment over the next year is almost a guarantee.\n\nWhile there are indeed *\u201cinflationary pressures\u201d* short-term as the massive infusions, coupled with supply shortages and delivery bottlenecks, higher prices will erode purchasing power. The decline in purchasing power, combined with higher input costs, and potentially higher taxes, will continue to weigh on confidence near term.\n\nThere are risks to assuming a strong economic and employment recovery over the next couple of quarters. The damage from the shutdown on the economy, and most importantly, small business, suggests recovery may remain elusive.\n\n**Most importantly, there is a massive difference between** ***\u201cgetting back to even\u201d*** **versus** ***\u201cgrowing the economy.\u201d*** **One creates economic prosperity by expanding production, which creates consumption. The other does not.**\n\nBeing optimistic about the economy and the markets currently is far more entertaining than doom and gloom. However, it is the honest assessment of the data and the underlying trends, which help protect one\u2019s wealth longer-term.\n\nThanks for addendum :  Lance Roberts"; *)
     scraper_score_test "post 1 in r/stocks hot" ~amount:1 "stocks-hot.json" 486;
     scraper_score_test "post 2 in r/stocks hot" ~amount:2 "stocks-hot.json" 62;
     scraper_score_test "post 7 in r/stocks hot" ~amount:7 "stocks-hot.json" 468;
     scraper_score_test "post 1 in r/stocks new" ~amount:1 "stocks-new.json" 0;
     scraper_score_test "post 2 in r/stocks new" ~amount:2 "stocks-new.json" 2;
     scraper_score_test "post 11 in r/stocks new" ~amount:11 "stocks-new.json" 10;
     scraper_score_test "post 1 in r/stocks rising" ~amount:1 "stocks-rising.json" 13;
     scraper_score_test "post 2 in r/stocks rising" ~amount:2 "stocks-rising.json" 3;
     scraper_score_test "post 15 in r/stocks rising" ~amount:15 "stocks-rising.json" 117;
     scraper_score_test "post 1 in r/stocks top alltime" ~amount:1 "stocks-top-alltime.json" 101660;
     scraper_score_test "post 2 in r/stocks top alltime" ~amount:2 "stocks-top-alltime.json" 88217;
     scraper_score_test "post 18 in r/stocks top alltime" ~amount:18 "stocks-top-alltime.json" 17084;
     scraper_score_test "post 1 in r/stocks top today" ~amount:1 "stocks-top-today.json" 906;
     scraper_score_test "post 2 in r/stocks top today" ~amount:2 "stocks-top-today.json" 608;
     scraper_score_test "post 9 in r/stocks top today" ~amount:9 "stocks-top-today.json" 36;
     scraper_score_test "post 1 in r/investing hot" ~amount:1 "investing-hot.json" 13;
     scraper_score_test "post 2 in r/investing hot" ~amount:2 "investing-hot.json" 10;
     scraper_score_test "post 20 in r/investing hot" ~amount:20 "investing-hot.json" 40;
     scraper_score_test "post 1 in r/wallstreetbets hot" ~amount:1 "wallstreetbets-hot.json" 254;
     scraper_score_test "post 2 in r/wallstreetbets hot" ~amount:2 "wallstreetbets-hot.json" 17067;
     scraper_score_test "post 14 in r/wallstreetbets hot" ~amount:14 "wallstreetbets-hot.json" 321;
     scraper_ratio_test "post 1 in r/stocks hot" ~amount:1 "stocks-hot.json" 0.99;
     scraper_ratio_test "post 2 in r/stocks hot" ~amount:2 "stocks-hot.json" 0.92;
     scraper_ratio_test "post 7 in r/stocks hot" ~amount:7 "stocks-hot.json" 0.94;
     scraper_ratio_test "post 1 in r/stocks new" ~amount:1 "stocks-new.json" 0.5;
     scraper_ratio_test "post 2 in r/stocks new" ~amount:2 "stocks-new.json" 1.0;
     scraper_ratio_test "post 11 in r/stocks new" ~amount:11 "stocks-new.json" 0.86;
     scraper_ratio_test "post 1 in r/stocks rising" ~amount:1 "stocks-rising.json" 0.84;
     scraper_ratio_test "post 2 in r/stocks rising" ~amount:2 "stocks-rising.json" 0.8;
     scraper_ratio_test "post 15 in r/stocks rising" ~amount:15 "stocks-rising.json" 0.81;
     scraper_ratio_test "post 1 in r/stocks top alltime" ~amount:1 "stocks-top-alltime.json" 0.9;
     scraper_ratio_test "post 2 in r/stocks top alltime" ~amount:2 "stocks-top-alltime.json" 0.94;
     scraper_ratio_test "post 18 in r/stocks top alltime" ~amount:18 "stocks-top-alltime.json" 0.99;
     scraper_ratio_test "post 1 in r/stocks top today" ~amount:1 "stocks-top-today.json" 0.94;
     scraper_ratio_test "post 2 in r/stocks top today" ~amount:2 "stocks-top-today.json" 0.93;
     scraper_ratio_test "post 9 in r/stocks top today" ~amount:9 "stocks-top-today.json" 0.83;
     scraper_ratio_test "post 1 in r/investing hot" ~amount:1 "investing-hot.json" 0.79;
     scraper_ratio_test "post 2 in r/investing hot" ~amount:2 "investing-hot.json" 0.82;
     scraper_ratio_test "post 20 in r/investing hot" ~amount:20 "investing-hot.json" 0.79;
     scraper_ratio_test "post 1 in r/wallstreetbets hot" ~amount:1 "wallstreetbets-hot.json" 0.87;
     scraper_ratio_test "post 2 in r/wallstreetbets hot" ~amount:2 "wallstreetbets-hot.json" 0.96;
     scraper_ratio_test "post 14 in r/wallstreetbets hot" ~amount:14 "wallstreetbets-hot.json" 0.84;
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
   "cashset test: " ^ ticker >:: fun _ -> 
     assert_equal expec (Cashset.is_stock_name ticker) ~printer:string_of_bool
 
 let cashset_tests = [
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
 
 (* let function_test_string name str1 str2 =
   name >:: fun _ -> assert_equal ~printer:pp_string str1 str2
 
 let function_test_bool name val1 val2 =
   name >:: fun _ -> assert_equal val1 val2 ~printer:string_of_bool
 
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
       (List.hd (snd(Parser.data stocks_1 "NP")))
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
         "SEE";
         "ANY";
         "GME";
         "SO";
         "AMC";
         "CBS";
         "DD";
       ];
     check_post_props "Third Post of stock \"GME\""
       (List.hd (snd(Parser.data stocks_10 "GME")))
       1. 1;
   ] *)
 
 let suite =
   "CamelStonks Test Suite"
   >::: List.flatten [ scraper_tests; cashset_tests; (*parser_tests; cashset_tests*) ]
 
 let _ = run_test_tt_main suite
 