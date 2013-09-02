#!/bin/sh

F="$1"
EX="./abd2main.native"

$EX -P $F -s -p -sd > saf.out
$EX -P $F -s -p -sd -T > term.out
