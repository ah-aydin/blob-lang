#!/bin/bash

# Check if the user provided a base name
if [ -z "$1" ]; then
  echo "Usage: $0 <program_name>"
  exit 1
fi

BASE_NAME="$1"

nasm -f elf64 -o "${BASE_NAME}.blob.o" "${BASE_NAME}.blob.asm" && \
ld "${BASE_NAME}.blob.o" -o "${BASE_NAME}.blob.app" && \
./"${BASE_NAME}.blob.app"

