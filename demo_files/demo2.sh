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
read -p "Press ENTER to continue"
echo