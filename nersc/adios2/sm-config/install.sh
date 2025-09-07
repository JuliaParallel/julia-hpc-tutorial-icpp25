set -u

ml load PrgEnv-${INSTALL_VERSION}
ml load cray-hdf5-parallel

git clone --depth 1 https://github.com/ornladios/ADIOS2.git
mkdir ${INSTALL_NAME}-build && cd ${INSTALL_NAME}-build
cmake ../ADIOS2 \
  -DADIOS2_BUILD_EXAMPLES=ON \
  -DADIOS2_USE_MPI=ON \
  -DADIOS2_USE_HDF5=ON \
  -DCMAKE_INSTALL_PREFIX=${INSTALL_DIR}/${INSTALL_VERSION_VARIANT}
make install -j ${N_PROCS}
