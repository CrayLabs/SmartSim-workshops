# SmartSim 101

In this hands-on session, you will discover the basics of SmartSim. The code can be run on a supercomputer or on your laptop. Instructions on how to connect to the supercomputer will be given separately.

## Goals of this tutorial
By the end of this tutorial you will be able to:
- launch a simulation and a Redis DB using SmartSim
- upload the results of the simulation to the Redis DB using the SmartRedis clients
- download the results of the simulation from a Python script or a Jupyter notebook and visualize them

## Setup
As a first step, you will need to `git clone` this repo. Given it is a small repo, you can do it on your home directory. Then you will need to navigate to the directory named `LRZ_2026` (the one containing this `README.md` file).

If you are working on `Simba` you will need to use a Charliecloud container which has `SmartSim` installed. We have provided a enviroment to use the container and set up the environment for using `SmartSim` and `SmartRedis`.

When you log on to `Simba` you need to run:

```bash
    srun -N 1  -p test -A [your account]  --pty bash -i 
    module use /dss/dsshome1/02/di54cil/tutorial/modulefiles
    module load smartsim
    $CONV_IMG
```

in order to set up the SmartSim container. Note that the last step extracts the Smartsim container image into your local user space. This step will take a couple of minutes.

### Building SmartSim and SmartRedis (optional)

> **NOTE** This paragraph can be skipped if you are on `Simba` and use the provided SmartSim container and SmartRedis installation.


If you are working on your laptop, you will have to:
- follow the instructions available in [the SmartSim official documentation](https://craylabs.org)
    - the suggested approach uses `conda`, but you can also install `cmake` and `git-lfs` on your laptop and then install `smartsim` in a python environment
- you can decide to build from source if you want to use the latest features, just `git clone https://github.com/CrayLabs/SmartSim`, enter the directory, and run `pip install .` (or `pip install .`) instead of the standard `pip install smartsim`. Then just follow the rest of the installation instructions.
- you will also need `matplotlib` to plot the results

The simulation you will instrument with SmartSim is available in four languages: Python, C, C++, and Fortran. If you are planning to work on the Python version, you are all set!

If you want to work on the C, C++, or Fortran version of the simulation, you will need to build the SmartRedis clients, following [these instructions](https://www.craylabs.org/docs/installation.html#build-smartredis-library-c-c-fortran) (you can also clone the current `develop` branch instead of the stable release).
Notice you will need to use the path to the SmartRedis install directory later. Depending on the CMake version you have, you may need to also set the environment variable `CMAKE_POLICY_VERSION_MINIMUM` to `3.5`. For example, on a bash shell, you will need to run

```bash
    export CMAKE_POLICY_VERSION_MINIMUM=3.5
```

## The simulation

We will work with the code developed by [John Burkardt](https://people.sc.fsu.edu/~jburkardt/) for computing [the steady state of a heated two-dimensional plate](https://people.sc.fsu.edu/~jburkardt/f_src/fd2d_heat_steady/fd2d_heat_steady.html). The code is available in C, C++, Fortran, Python, and many other languages. We slightly modified the original code used to run a test:
- we removed the part which would plot the results using `gnuplot`
- we added a parser to make it accept command line arguments for `nx` and `ny` (referred to as $NX$ and $NY$ in the link above)
- for compiled languages, we added `CMake` files to build the test, they already include the instructions needed to link the code against SmartSim


### Building the test

If you decide to work with one of the compiled languages, you should navigate to `/src/<language>`.

then, if you are working on `Simba`, you must first set the environment while on the compute node:

```bash
    srun -N 1  -p test -A [your account] --pty bash -i
    module use /dss/dsshome1/02/di54cil/tutorial/modulefiles
    module load smartsim
```

Then to build you can run (ensure you use srun to compile on the compute nodes):

```bash
    chmod +x make_app.sh
    ./make_app.sh
    exit
```

and the app will be built.

On your laptop, you should make sure to set the SMARTREDIS_INSTALL_PATH to the location of where you install SmartRedis and then you should run:

```bash
    mkdir build
    cd build
    cmake .. -DSMARTREDIS_INSTALL_PATH=${SMARTREDIS_INSTALL_PATH}
    make
    cd ..
```

and it should work: if you encounter problems, make sure you are using a supported compiler.


## Running the driver script

If you are on `Simba`, please follow the next step, if you are on your laptop, just skip it.

### Getting the allocation on Slurm
To launch jobs on `Simba`, you will need to work within an interactive *allocation* given by the workload manager (Slurm) in order to run SmartSim on the compute nodes.


SmartSim's native language is Python. We will use the driver script `smartsim_101.py` to launch the app and retrieve its results.


The command to run the driver script in an interactive allocation is:


```bash
    srun -N 1  -p test -A [your account]  --pty bash -i
    module use /dss/dsshome1/02/di54cil/tutorial/modulefiles
    module load smartsim
    $SSIM_SH
    python3 smartsim_101.py 
    exit
    exit
```

### Jupyter notebook
If you are on your laptop, you can also use the Jupyter notebook `smartsim_101.ipynb`, which contains the same code, just install `jupyterlab` in your virtual environment, and run `jupyter lab` from the directory containing the `README.md` file.

### Your exercise starts here!
That's it, the rest of the instructions are available in the script or in the notebook, you will just have to replace a bunch of `EXERCISE` lines to get this workflow up and running. Have fun!

If you want to see more output from SmartSim, you can set the environment variable `SMARTSIM_LOG_LEVEL` to `debug` or `developer`: this will reveal a bit of the internal mechanisms of SmartSim - it should not be necessary for this very simple exercise, anyhow.

If you have questions, you can either ping the instructor directly, or [join our Slack Workspace](https://join.slack.com/t/craylabs/shared_invite/zt-nw3ag5z5-5PS4tIXBfufu1bIvvr71UA) and enter the `#workshops` channel.

### Solutions
If you get stuck and do not want to ask an instructor (or you're not at the event), the solutions are available (not really hidden) in the workshop directory. If you want to _run_ the solution driver, don't forget to build the corresponding SmartRedis-instrumented simulation code which is available in `solutions/src/<your language>` (unless you're using the Python simulation, which does not have to be compiled).

## Notes
This repository contains a modified version of code taken from [John Burkardt's webpage](https://people.sc.fsu.edu/~jburkardt/). This repository also contains a file taken from [this repo](https://github.com/haniibrahim/f90getopt). Licenses are inherited.
