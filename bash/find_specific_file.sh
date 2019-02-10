#!/usr/bin/env bash
#
# find_specific --- find files that matchs $2 completely
#
# Copyright 2019 (c) Cj-bc
# This software is released under MIT License
# @(#) usage:
# @(#)    ./find.sh (path) [-name|-regex] (regex)
#
# @(#) feature:
# @(#)    refer to commit for the latest version
#
# landmark(using ./landmark):
#   /usr/bin/find: 0m6.893s
#       ./find.sh: 0m16s-0m40.
#     this script: 0m13.514s-0m26.331s
#
# landmark detail:
#   execute each command 1,000 times using `bash/landmark.sh`
#   ./landmark.sh './find.sh ~/.ghq/github.com/Cj-bc/playground/cpp/EzoeRyou_cpp_intro -regex all\.h' './find_specific_file.sh ~/.ghq/github.com/Cj-bc/playground/cpp/EzoeRyou_cpp_intro all.h'
#   ./landmark.sh 'find ~/.ghq/github.com/Cj-bc/playground/cpp/EzoeRyou_cpp_intro -regex all\.h' './find_specific_file.sh ~/.ghq/github.com/Cj-bc/playground/cpp/EzoeRyou_cpp_intro all.h'
#   playground's commit hash:  01a5bd5
#
#
# @(#) version v0.1.0

# find_core: find executable {{{
# @param <string dir>
# @param <string target_filename>
function find_core()
{
  [[ ! -d "$1" ]] && { { [[ "$1" = "${2}" ]] && echo "$(pwd)/$1"; } && return 0 || return 1; }

  local c_pwd="$(pwd)"
  cd "$1" # don't make subshell to prievnt fork bomb
  for _file in *; do
    find_core "$_file" "$2"
  done
  cd "$c_pwd"
  [[ "$1" = "${2}" ]] && echo "$(pwd)/$1"
}
# }}}

find_core "$@"
