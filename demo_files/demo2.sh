#!/bin/bash


read -p "Press ENTER to begin demo"
echo
echo "Welcome to the OCaml Stocks Bot Demo for CS3110 Final Project Milestone 2" | pv -qL 20
sleep 1
echo "This project and demo is brought to you by:" | pv -qL 20
echo "Duncan McDonald, Peter Bell, Matthew Chan, and Miguel Roberts" | pv -qL 20
sleep 1
read -p "Press ENTER to continue"
echo
echo "Now that we have displayed our scraping and parsing functionalities, we will show how we can analyze and manipulate this data" | pv -qL 20
echo "One of the first things we can do is request a recommendation rating from Yahoo Finance" | pv -qL 20
echo "We can then convert this to a history score using a Function we developed:" | pv -qL 20
sleep 1
w3m history_score.png
sleep 1
read -p "Press ENTER to continue into the first interactive session"
echo
echo "please wait while the project builds and initializes" | pv -qL 20
sleep 3
make historydemo
echo
echo "We also want to make a rating for the connotations of posts" | pv -qL 20
echo "To do this we are implementing a package called pyml which imports a python module" | pv -qL 20
echo "This module uses VaderSentiments to calculate the connotation of a post and assigns this to stocks" | pv -qL 20
sleep 1
read -p "Press ENTER to continue to the second interactive session"
echo
echo "please wait while the project builds and initializes" | pv -qL 20
sleep 3
make connotationdemo
echo
echo "Next comes the piece of our project that pieces together all the data" | pv -qL 20
echo "and puts it into one algorithm function that we developed that should give ratings to each stock and rate them accordingly" | pv -qL 20
sleep 1
echo "the score of a post is calculated using a function we developed:"
sleep 2
w3m algorithm.png
sleep 1
read -p "Press ENTER to continue to the third interactive session"
echo
echo "please wait while the project builds and initializes" | pv -qL 20
sleep 3
make algorithmdemo
echo 
echo "Thank you for coming to our demo - please come again!" | pv -qL 20
