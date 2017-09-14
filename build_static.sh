#!/bin/bash

stack ghc -- -threaded -outputdir out -O2 --make -static -optc-static -optl-static -optl-pthread Aggregator.hs
