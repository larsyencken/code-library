#!/bin/bash
#
# setEnvironment.sh
#
# Sets the necessary environment variables for the ML Framework to function.
# Don't run this file, instead type:
#   
#   source environmentVars.sh
#
# from shell.
#

# Ensure we're in the correct directory
if [ ! -d www ]; then
  echo 'ERROR: please execute this from the src/ directory' 
  exit 1
fi

export DJANGO_SETTINGS_MODULE='www.settings'
export PYTHONPATH="$(pwd)/.."

echo 'Environment variables set'
