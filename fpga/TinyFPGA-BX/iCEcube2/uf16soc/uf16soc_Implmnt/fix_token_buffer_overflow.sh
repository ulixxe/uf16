#!/bin/sh

# fix icecube2 error on parsing edif file: 
# Token buffer overflow

sed -i '/^\s*(property ROM_DATA (string "[0-9a-fA-F]*"))/d' uf16soc.edf
