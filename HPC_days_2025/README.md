# SmartSim 101

In this hands-on session, you will discover the basics of SmartSim. The code can be run on a supercomputer (`hamilton`) or on your laptop. Instructions on how to connect to the supercomputer will be given separately.

## Goals of this tutorial
By the end of this tutorial you will be able to:
- launch a simulation and a Redis DB using SmartSim
- upload the results of the simulation to the Redis DB using the SmartRedis clients
- download the results of the simulation from a Python script or a Jupyter notebook and visualize them

## Setup
As a first step, you will need to `git clone` this repo. Given it is a small repo, you can do it on your home directory.

If you are working on `hamilton`, you will be able to use a python environment which has `SmartSim` and `SmartRedis` installed. Moreover, the SmartRedis C, C++, and Fortran clients have been compiled and can be used in the simulation. All you will need to do when you log on to `hamilton` is to run:

```bash
    module load python/3.10.8
    source /projects/hpcday/smartsim_tutorial/venv/smartsim/bin/activate
    export SMARTREDIS_LIBRARY=/projects/hpcday/smartsim_tutorial/SmartRedis/install
    export SMARTREDIS_INSTALL_PATH=/projects/hpcday/smartsim_tutorial/SmartRedis/install
```
and you are ready to go!

### Building SmartSim and SmartRedis (optional)

> **NOTE** This paragraph can be skipped if you are on Hamilton and use the provided virtual environment and SmartRedis installation.


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

If you decide to work with one of the compiled languages, you should navigate to `/src/<language>`, then, if you haven't set the environment variable `SMARTREDIS_LIBRARY`, it is a good moment to do so. Again, on `hamilton`, it will be

```bash
    export SMARTREDIS_LIBRARY=/projects/hpcday/smartsim_tutorial/SmartRedis/install
    export SMARTREDIS_INSTALL_PATH=/projects/hpcday/smartsim_tutorial/SmartRedis/install
```

then, if you are working on `hamilton`, you can simply run

```bash
    chmod +x make_app.sh
    ./make_app.sh
```

and the app will be built.

On your laptop, you should run

```bash
    mkdir build
    cd build
    cmake .. -DSMARTREDIS_INSTALL_PATH=${SMARTREDIS_INSTALL_PATH}
    make
    cd ..
```

and it should work: if you encounter problems, make sure you are using a supported compiler.


## Running the driver script

If you are on `hamilton`, please follow the next step, if you are on your laptop, just skip it.

### Getting the allocation on Slurm
To launch jobs on `hamilton`, you will need to work within an interactive *allocation* given by the workload manager, Slurm. For this tutorial, you will only need two nodes.
SmartSim's native language is Python. We will use the driver script `smartsim_101.py` to launch the app and retrieve its results.

Make sure you have activated the right virtual environment (you will see its name at the beginning of the terminal line, e.g. `(smartsim) [username@node]$`.
The command to run the driver script in an interactive allocation we need is

```bash
    salloc -N 2 --time=00:05:00 python smartsim_101.py
```

### Jupyter notebook
If you are on your laptop, you can also use the Jupyter notebook `smartsim_101.ipynb`, which contains the same code, just install `jupyterlab` in your virtual environment, and run `jupyter lab` from the directory containing the `README.md` file.

That's it, the rest of the instructions are available in the script or in the notebook. Have fun!

If you have questions, you can either ping the instructor directly, or [join our Slack Workspace](https://join.slack.com/t/craylabs/shared_invite/zt-nw3ag5z5-5PS4tIXBfufu1bIvvr71UA) and enter the `#workshops` channel.

## Notes
This repository contains a modified version of code taken from [John Burkardt's webpage](https://people.sc.fsu.edu/~jburkardt/). This repository also contains a file taken from [this repo](https://github.com/haniibrahim/f90getopt). Licenses are inherited.
