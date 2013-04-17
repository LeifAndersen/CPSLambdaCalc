#!/usr/bin/env bash

racket compile.rkt < $1 | racket lc-hybrid.rkt > $1.out
