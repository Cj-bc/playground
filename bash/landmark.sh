#!/usr/bin/env bash
#
# Copyright 2019 (c) Cj-bc
# This software is released under MIT License
#
# @(#) version -

# usage: ./landmark.sh <target command> <comamnd to compair>
#

readonly target="$1"
readonly compair_to="$2"

echo "execute target for 1,000 times: ${target}"
{ time \
  for i in {0..1000};do
    $target 2>/dev/null
  done; } >/dev/null

echo "execute compair to for 1,000 times: ${compair_to}"
{ time \
  for i in {0..1000};do
    $compair_to 2>/dev/null
  done; } >/dev/null
