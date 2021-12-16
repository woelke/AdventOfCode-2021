#!/bin/bash

set -eu -o pipefail

ghci -i"/home/woelke/workspace/AdventOfCode-2021" -XBangPatterns -Wall +RTS -K4000M
