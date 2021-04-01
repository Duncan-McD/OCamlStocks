#!/bin/bash
echo "Press any key to continue"
enter_not_pressed=true
while [ -t 3 -n 1 ] ; do
sleep 1
done

echo "Welcome to the OCaml Stocks Bot Demo for CS3110 Final Project Milestone 1" | pv -qL 10

make
echo "exited utop" | pv -qL 10