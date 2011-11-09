#!/usr/bin/env bash
#
#  arrays.sh
#
#  Demonstration of lists and accociative arrays in bash.
#

declare -a animals
animals=(cat dog sheep mouse)
echo "We have ${#animals[@]} animals, in order:"
i=0
while [ $i -lt ${#animals[@]} ]; do
  echo "  ${i}: ${animals[$i]}"
  i=$(( $i + 1 ))
done
echo

declare -A pets
pets=([lars]=nero [lauren]=henry [bree]=mimi)
echo "We have ${#pets[@]} pet owners:"
for owner in "${!pets[@]}"; do
  echo "  ${owner}: ${pets[${owner}]}"
done
echo
