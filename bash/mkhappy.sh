#!/usr/bin/env bash
#
# mkhappy -- make you happy
#
# Copyright 2018 (c) Cj-bc
# This software is released under MIT License
#
# @(#) version -

name="$1"


rows=()
while read line; do
  rows=("${rows[@]}" "$line")
done < <(toilet "happy")

for row in ${rows[@]}; do
  for i in $(eval echo {0..${#row}}); do
    if [ "$i" != " " ]; then
      res_col+=${name[$i]}
    else
      res_col++" "
    fi
  done
done
