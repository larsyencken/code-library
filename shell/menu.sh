#!/usr/bin/bash
#
#  menu.sh
#
#  A simple example of a numbered menu implemented in shell.
#


OPTIONS="Hello Quit"

select opt in $OPTIONS; do
  if [ "${opt}" = "Hello" ]; then
    echo "Hello World"
  elif [ "${opt}" = "Quit" ]; then
    exit
  else
    echo "Bad option"
  fi
done

# vim: set ts=2 sts=2 et:
