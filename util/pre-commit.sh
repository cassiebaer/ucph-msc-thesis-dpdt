#!/bin/bash

# To install as a pre-commit hook in git, run:
#
#   ln -s ../../util/pre-commit.sh ./.git/hooks/pre-commit.sh
#
# To commit even if tests are failing,
#  use the --no-verify flag to git commit.

git stash -q -k
util/run_tests.sh
RES=$?
git stash pop -q

exit $RES

