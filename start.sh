#!/bin/bash

node external/faye/server.js &

# http-server -p 80 -S -C cert/cert.pem -K cert/key.pem -c-1
http-server -p 80 -c-1
