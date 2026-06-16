#!/bin/bash

printf "\n ########## CREATING CONTAINER FOLDER ##########\n\n"

mkdir -p fabm_recom_setup
cd fabm_recom_setup
export FABM_RECOM_DIR=$(pwd)

printf "\n ########## CLONING FABM ##########\n\n"

git clone --depth 1 https://github.com/fabm-model/fabm.git

printf "\n ########## CLONING RECOM ##########\n\n"

git clone --depth 1 https://github.com/RECOM-Regulated-Ecosystem-Model/REcoM.git

printf "\n ########## (RE)CLONING FABM_RECOM ##########\n\n"

git clone --depth 1 https://github.com/wiltonloch/fabm_recom

printf "\n ########## FETCHING GOTM ##########\n\n"

mkdir gotm
cd gotm
wget https://github.com/gotm-model/code/releases/download/v6.0.7/code.tar.gz
tar -xzf code.tar.gz

printf "\n ########## LOADING DEPENDENCY PACKAGES ##########\n\n"

ml gcc/11.2.0-gcc-11.2.0
ml openmpi/4.1.2-gcc-11.2.0
ml netcdf-fortran/4.5.3-gcc-11.2.0
export FC=mpif90

printf "\n ########## BUILDING RECOM ##########\n\n"

cd ${FABM_RECOM_DIR}/REcoM
mkdir build
cd build
cmake ..
make

printf "\n ########## BUILDING GOTM WITH FABM ##########\n\n"

cd ${FABM_RECOM_DIR}
mkdir -p install_dir gotm/build
cd gotm
cmake -S code -B build --install-prefix=${FABM_RECOM_DIR}/install_dir -DFABM_BASE=${FABM_RECOM_DIR}/fabm -DRECOM_LIB_PATH=${FABM_RECOM_DIR}/REcoM/build/librecom.a -DRECOM_INCLUDE_PATH=${FABM_RECOM_DIR}/REcoM/build -DFABM_EXTRA_INSTITUTES=awi -DFABM_AWI_BASE=${FABM_RECOM_DIR}/fabm_recom/src
cmake --build build --target install

printf "\n ########## COPYING TEMPLATE CONFIG FILES ##########\n\n"

cd ${FABM_RECOM_DIR}/install_dir/bin
cp ${FABM_RECOM_DIR}/fabm_recom/config/* ${FABM_RECOM_DIR}/install_dir/bin

printf "\n ########## RUNNING GOTM WITH THE FABM_RECOM IMPLEMENTATION ##########\n\n"

export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:/sw/spack-levante/netcdf-fortran-4.5.3-pywf2l/lib
cd ${FABM_RECOM_DIR}/install_dir/bin
./gotm
