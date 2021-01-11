#!/bin/sh

emacs -Q --batch --directory="." -l ert -l test/test-all.el -f ert-run-tests-batch-and-exit
