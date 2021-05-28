# OCamlStocks

Duncan McDonald, Peter Bell, Miguel Roberts, Matthew Chan

## OCamlStocks Overview

Welcome to OCamlStocks! This program aims to enable the user to research the effectiveness of Reddit as an Investment Advisor. This is built through multiple different steps:

 - Scraping and Parsing Reddit: The program can scrape subreddits looking for any stock tickers and then storing data about how that stock is mentioned.

 - Scraping Yahoo Finance: The program scrapes Yahoo Finance to factor past data and recommendation ratings for stocks into the formula.

 - Stock Algorithm: Our algorithm aims to combine various features about a subreddit post and Yahoo's recommendation rating. 

 <img src="github.com/Duncan-McD/OCamlStocks/blob/main/resources/algorithm.png">

The above equation is our algorithm. 
 - s is the Reddit Score of a post containing a stock, 
 - c is the connotation of that post (This is calculated using the VaderSentiment)
 - n is the number of reddit posts containg this particular stock
 - h is the history score which is calculated from the Yahoo Reccomendation Rating with the falling equation where r is the recommendation rating

<img src="github.com/Duncan-McD/OCamlStocks/blob/main/resources/history-score.png">

 - x,y, w1, and w2 are constants to weight the effects of each of the variables

 - Optimizer: The optimizer determines the ideal constants for a day by performing uniform distrubution testing on one day and then determining which of these had the best resulting net gain in income the next day.

 Further details of included features are described in the built in help instructions and menu descriptions within the program.


 ## How To Run:

1. run `opam install . --deps-only` to install dependencies using opam
2. run `make` or `make bot` to run the bot/program
3. run `make build` to build project
4. run `make utop` to open the project in utop
5. run `make test` to run ounit test cases
6. run `make clean` to remove the old _build folder and docs
7. run `make zip` to make a zip file of the project
8. run `make install` to be walked through an install process (Further details are in Install.md)
9. run `make docs` to make the documentation
  - After making docs:
    - open index.html in _doc.public to see public docs
    - open index.html in _doc.private to see private docs