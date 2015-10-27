#!/bin/bash

for file in $(find ./tests/idris -name "*.idr")
do
  echo "Type checking" $file
  idris --check -isrc $file
  [ $? -ne 0 ] && exit 1
done

exit 0
