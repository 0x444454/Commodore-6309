#!/bin/bash

LWASM_PATH="/mnt/c/lwtools-4.23/lwasm"

"${LWASM_PATH}"/lwasm --abs --6309 -o rom/1-k6309.rom rom.asm 

if [ $? -eq 0 ]; then
    # Hexdump (debug)
    hexdump -C rom/1-k6309.rom
fi
