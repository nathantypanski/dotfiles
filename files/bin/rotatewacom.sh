#!/bin/bash

# This script rotates a wacom tablet for lefties.

device="Wacom Intuos4 6x9"
stylus="$device Pen stylus"
eraser="$device Pen eraser"
touch="$device Finger touch"
pad="$device Finger pad"

xsetwacom --set "$stylus" Rotate $1
xsetwacom --set "$eraser" Rotate $1
xsetwacom --set "$touch"  Rotate $1
xsetwacom --set "$pad"    Rotate $1
