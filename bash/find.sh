#!/usr/bin/env bash
#
# Copyright 2019 (c) Cj-bc
# This software is released under MIT License
#
# @(#) usage:
# @(#)    ./find.sh (path) (name)
#
# @(#) feature:
# @(#)    refer to commit for the latest version
#
# landmark:
#   /usr/bin/find: 0m0.691s
#     this script: 0m11.957s
#
# @(#) version 0.1.0

EX_DATAERR=65
EX_SUCCESS=0

# find_core: find executable {{{
# @param <string dir>
# @param <string target_filename>
function find_core()
{
  local _path="$1"
  local target="$2"

  if [ -d "$_path" ];then
    local c_pwd="$(pwd)"
    cd "$_path" || return $EX_DATAERR;
    for _file in *; do
      find_core "$_file" "$target"
    done
    cd "$c_pwd" || :
    if [[ "$_path" =~ ^${target}$ ]];then
      echo "$(pwd)/$_path"
      return $EX_SUCCESS;
    else
      return 1;
    fi
  else
    # for -name option.currently not used.
    # local regex_protected="${target/\./\\.}"
    if [[ "$_path" =~ ^$target$ ]];then
      echo "$(pwd)/$_path"
      return $EX_SUCCESS;
    else
      return 1;
    fi
  fi
}
# }}}

find_core "$1" "$2"
