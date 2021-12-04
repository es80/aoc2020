#!/bin/bash

set -e

diff <(./formatAndRun.sh) answers
cat answers

