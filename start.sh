#!/bin/bash

http-server -p 80 &
node external/faye/server.js &
