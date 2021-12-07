#!/usr/bin/env sh

file=$1
proof_file=$2

echo "file: '$file', proof_file: '$proof_file'"

sleep 0.5

if grep -q -E '^SAT' $file ; then echo "SAT" ;
elif grep -q -E '^UNSAT' $file ; then
  echo "UNSAT" ;
  if [ -n "$proof_file" ] ; then
    echo "writing proof to '$proof_file'";
    echo "the proof" > $proof_file ;
  fi
else echo "UNKNOWN"
fi
