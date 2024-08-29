#!/bin/sh

# from inside docker:
# docker exec -it icestorm /bin/bash

#cd /Projects
#icebram -v -g 16 256 > from.hex
sed '1d' rom.hex > to.hex
iceunpack -v uf16soc_bitmap.bin > tmp.asc
icebram -v from.hex to.hex < tmp.asc > tmp_rommed.asc
icepack -v tmp_rommed.asc > uf16soc_bitmap_rommed.bin
rm tmp.asc tmp_rommed.asc
