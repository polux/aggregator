#!/bin/bash

stack ghc -- -outputdir out -O2 --make -static -optc-static -optl-static -optl-pthread Aggregator.hs
