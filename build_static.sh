#!/bin/bash

stack ghc -- -O2 --make -static -optc-static -optl-static -optl-pthread Aggregator.hs
