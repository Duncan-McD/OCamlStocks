#!/bin/bash

read -p "Press ENTER to begin demo"
echo
echo "Welcome to the OCaml Stocks Bot Demo for CS3110 Final Project Milestone 1" | pv -qL 10
sleep 1
echo "This project and demo is brought to you by:" | pv -qL 10
echo "Duncan McDonald, Peter Bell, Matthew Chan, and Miguel Roberts" | pv -qL 10
sleep 1
read -p "Press ENTER to continue"
echo
echo "The first module and piece of our software stack is the scraper" | pv -qL 10
echo "This module takes in the name of a subreddit and will return any number of posts you specify in any order you want" | pv -qL 10
sleep 1
read -p "Press ENTER to continue to the interactive session"
echo
echo "please wait while the project builds and initializes" | pv -qL 10
sleep 3
make scraperdemo
echo
echo "The second module and piece of our software stack is the cashset" | pv -qL 10
echo "This module converts a csv file containing stock tickers and converts this to a hashset on startup." | pv -qL 10
echo "You can then query the cashset to test whether or not a stock ticker is an actual ticker." | pv -qL 10
read -p "Press ENTER to continue to the second interactive session"
echo
echo "please wait while the project builds and initializes" | pv -qL 10
make cashsetdemo
echo
echo "The third module and piece of our software stack is the parser" | pv -qL 10
echo "This module takes in a scraped subreddit and produces informatoin about the stocks mentioned in these posts" | pv -qL 10
read -p "Press ENTER to continue to third interactive session"
echo
echo "please wait while the project builds and initializes" | pv -qL 10
echo "WILL BE IMPLEMENTED WHEN PARSER IS IN MAIN"