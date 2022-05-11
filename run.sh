#!/bin/sh
echo "$1" | stack run | lli

echo $?
