#!/bin/bash

grep "^$2:" "$1" | cut -b"$((${#2} + 2))-"
