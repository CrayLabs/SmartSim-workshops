#!/bin/bash


mkdir build
cd build
CC=cc CXX=CC FC=ftn cmake .. -DSMARTREDIS_INSTALL_PATH=${SMARTREDIS_INSTALL_PATH}
make

cd ..
