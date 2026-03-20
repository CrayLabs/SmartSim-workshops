#!/bin/bash


mkdir build
cd build
cmake .. -DSMARTREDIS_INSTALL_PATH=${SMARTREDIS_INSTALL_PATH}
make

cd ..