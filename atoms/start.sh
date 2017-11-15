#!/bin/bash

ln -sf index-client-light.html index.html
http-server -p 80 &
node external/faye/server.js &
