#!/bin/bash

read -p "Press ENTER to begin demo"
echo
echo "Welcome to the OCaml Stocks Bot Demo for CS3130 Final Project Milestone 1" | pv -qL 30
sleep 1
echo "This project and demo is brought to you by:" | pv -qL 30
echo "Duncan McDonald, Peter Bell, Matthew Chan, and Miguel Roberts" | pv -qL 30
sleep 1
read -p "Press ENTER to continue"
echo
echo "The first module and piece of our software stack is the scraper" | pv -qL 30
echo "This module takes in the name of a subreddit and will return any number of posts you specify in any order you want" | pv -qL 30
sleep 1
read -p "Press ENTER to continue to the interactive session"
echo
echo "please wait while the project builds and initializes" | pv -qL 30
sleep 3
make scraperdemo
echo
echo "The second module and piece of our software stack is the cashset" | pv -qL 30
echo "This module converts a csv file containing stock tickers to a hashset on startup." | pv -qL 30
echo "You can then query the cashset to test whether or not a stock ticker is an actual ticker." | pv -qL 30
sleep 1
read -p "Press ENTER to continue to the second interactive session"
echo
echo "please wait while the project builds and initializes" | pv -qL 30
sleep 3
make cashsetdemo
echo
echo "The third module and piece of our software stack is the stockdata module" | pv -qL 30
echo "This module takes a stock ticker and returns the relevent data from Yahoo Finance" | pv -qL 30
sleep 1
read -p "Press ENTER to continue to the third interactive session"
echo
echo "please wait while the project builds and initializes" | pv -qL 30
sleep 3
make stockdatademo
echo
echo "The fourth module and piece of our software stack is the parser" | pv -qL 30
echo "This module takes in a scraped subreddit and produces informatoin about the stocks mentioned in these posts" | pv -qL 30
sleep 1
read -p "Press ENTER to continue to fourth interactive session"
echo
echo "please wait while the project builds and initializes" | pv -qL 30
sleep 3
echo "PARSER DEMO WILL BE IMPLEMENTED SOON"