#!/usr/bin/env bash
set -e

if [[ ${FCKIT_EVAL_ARGS_EXCLUDE:-unset} != unset ]]; then
  excludes=()
  IFS=${FCKIT_EVAL_ARGS_EXCLUDE_DELIMITER:-,} read -r -a excludes <<< "$FCKIT_EVAL_ARGS_EXCLUDE"
  args=("$@")
  for ((i=0; i<"${#args[@]}"; ++i)); do
    for exclude in "${excludes[@]}"; do
      if [[ "${args[i]}" =~ ${exclude} ]]; then
        unset args[i];
      fi
    done
  done
  if [[ ${FCKIT_EVAL_VERBOSE:-unset} == 1 ]]; then
    echo "+ ${args[@]}"
  fi
  eval "${args[@]}"
else
  if [[ ${FCKIT_EVAL_VERBOSE:-unset} == 1 ]]; then
    echo "+ $@"
  fi
  eval "$@"
fi

