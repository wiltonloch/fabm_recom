# FABM implementation for REcoM

This is the first version of a [FABM](https://github.com/fabm-model/fabm/wiki) implementation for the [REcoM](https://github.com/RECOM-Regulated-Ecosystem-Model/REcoM) model.

It depends both on the FABM framework and on the REcoM public library.

## Building

This guide details the build and running process for the setup on the Levante supercomputer at DKRZ. For any other system most of the steps should be similar to some extent, you should mostly be careful with the dependencies and required packages.

To simplify setting it up, there is also a convenience script called `build_levante.sh` that automates all the following steps.

The FABM build and run will be based on the GOTM model.

First, create a folder to contain all the code necessary to build the setup. Here we'll call it `fabm_recom_setup`. You can assign the absolute path to this folder to an environment variable, here called `FABM_RECOM_DIR`.

```bash
mkdir fabm_recom_setup
cd fabm_recom_setup
export FABM_RECOM_DIR=$(pwd)
```

If you do this, you can basically just copy and paste all of the following commands and it should work.


Next, clone the FABM, REcoM and fabm-recom repositories.

```bash
git clone --depth 1 https://github.com/fabm-model/fabm.git
git clone --depth 1 https://github.com/RECOM-Regulated-Ecosystem-Model/REcoM.git
git clone --depth 1 https://github.com/wiltonloch/fabm_recom
```

You will also need to download the GOTM model. Here we will use packaged release version 6.0.7

```bash
mkdir gotm
cd gotm
wget https://github.com/gotm-model/code/releases/download/v6.0.7/code.tar.gz
tar -xzf code.tar.gz
```

We then proceed to building each of the pieces: REcoM, GOTM (with external FABM) and finally the FABM implementation for REcoM.

For that we'll need a number of packages available on the Levante system, which are loaded as follows:

```bash
ml gcc/11.2.0-gcc-11.2.0
ml openmpi/4.1.2-gcc-11.2.0
ml netcdf-fortran/4.5.3-gcc-11.2.0
export FC=mpif90
```

### Building REcoM

We first go into the REcoM folder and create a build directory. Then we run CMake to generate the Makefiles and finally we build the library.

```bash
cd ${FABM_RECOM_DIR}/REcoM
mkdir build
cd build
cmake ..
make
````

After this, you should have a `librecom.a` file in the `REcoM/build/lib` folder, which is the library we will link against when building the FABM implementation.

### Building GOTM with external FABM

Then we'll build GOTM (our host model) with FABM support. GOTM is already packaged with FABM, but we will build an external version, so that we can add the fabm-recom implementation.

Here we need a number of flags passed to the CMake configuration step.

The first is where the external FABM is located.

The second and third are respectively the path to the REcoM library and to its include folder. This is necessary because the fabm-recom implementation relies on variables and subroutines from the actual REcoM model.

The remaining fourth and fifth flags are passed to FABM, so that it can include the fabm-recom implementation in its build.


```bash
cd ${FABM_RECOM_DIR}
mkdir -p install_dir gotm/build
cd gotm
cmake -S code -B build --install-prefix=${FABM_RECOM_DIR}/install_dir -DFABM_BASE=${FABM_RECOM_DIR}/fabm -DRECOM_LIB_PATH=${FABM_RECOM_DIR}/REcoM/build/librecom.a -DRECOM_INCLUDE_PATH=${FABM_RECOM_DIR}/REcoM/build -DFABM_EXTRA_INSTITUTES=awi -DFABM_AWI_BASE=${FABM_RECOM_DIR}/fabm_recom/src
cmake --build build --target install
```

## Running

After building and installing in the previous step, you should have the gotm binary in the install folder, bundled with the fabm infrastructure and the fabm-recom implementation.

To run it you'll need a GOTM configuration file, a FABM configuration file, and a REcoM namelist file.

For the GOTM configuration file you can simply generate one by running the binary itself:

Templates for all these configuration files are provided inside of the config folder of this repository. You can simply copy the files to the install folder and you should be ready to run:

```bash
cd ${FABM_RECOM_DIR}/install_dir/bin
cp ${FABM_RECOM_DIR}/fabm_recom/config/* ${FABM_RECOM_DIR}/install_dir/bin
```


Before running on Levante, you still have to set the runtime seach path for the netCDF dynamic library with the following:

```bash
export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:/sw/spack-levante/netcdf-fortran-4.5.3-pywf2l/lib
```

You can then finally run the setup simply with:

```bash
cd ${FABM_RECOM_DIR}/install_dir/bin
./gotm
```
