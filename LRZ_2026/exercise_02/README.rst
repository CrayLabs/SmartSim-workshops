This workflow has
four components: a mock simulation, a sampling service that reduces the amount
of data from the simulation, a training service that consumes the data to train
a simple neural network, and an in-memory datastore used to transfer data
between the various components.

The data used for this example is a small subset of work done under an
ASCR Leadership Computing Challenge project using the MFIX-Exa multiphase
flow solver:
- William Fullmer, Jordan Musser, Aytekin Gel, Sarah Beetham, Machine
Learning-Enhanced Multiphase CFD for Carbon Capture Modeling, 11/29/2023,
https://edx.netl.doe.gov/dataset/machine-learning-enhanced-multiphase-cfd-for-carbon-capture-modeling,
DOI: 10.18141/2344941

Installation (Simba)
-------------------------

.. code:: bash

    srun -n 1 -p test -A [your account] --pty /bin/bash -i
    module use /dss/dsshome1/02/di54cil/tutorial/modulefiles
    module load smartsim
    mkdir -p build
    cd build
    cmake -Dsmartredis_DIR=$SMARTREDIS_HOME/install/share/cmake/smartredis \
          -Dsmartredis-fortran_DIR=$SMARTREDIS_HOME/install/share/cmake/smartredis-fortran \
          -DCMAKE_INSTALL_PREFIX=../ ..
    make
    cd ..


Running Examples (Simba)
-----------------------------

.. code:: bash

    srun -n 1 -p test -A [your account] --pty /bin/bash -i
    module use /dss/dsshome1/02/di54cil/tutorial/modulefiles
    module load smartsim
    $SSIM_SH
    python3 driver.py
