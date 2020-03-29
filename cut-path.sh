#!/bin/bash

ROOT=$1; shift
PWD=$1; shift

echo ${PWD#$ROOT}
