#!/bin/bash

import -window root png:- | convert - -blur 0x20 png:- | i3lock -n -f -i /dev/stdin
