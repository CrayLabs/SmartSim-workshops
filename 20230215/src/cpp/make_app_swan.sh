module switch PrgEnv-cray/6.0.10 PrgEnv-gnu
module switch gcc gcc/9.3.0
module unload atp craype-broadwell

# Use generic target to be able to run on any x86_64 CPU
export CRAY_CPU_TARGET=x86-64

SMARTREDIS_INSTALL_PATH="${SMARTREDIS_INSTALL_PATH:-${SMARTREDIS_LIBRARY}}"

mkdir build
cd build
cmake .. -DSMARTREDIS_INSTALL_PATH=${SMARTREDIS_INSTALL_PATH}
make

cd ..