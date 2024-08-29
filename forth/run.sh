#!/bin/sh

mkdir -p output
gforth -e "include rom.f .S bye"
echo  # put end of line to avoid "%" output
