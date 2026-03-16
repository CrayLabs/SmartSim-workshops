SmartSim OLCF/NERSC Workshop 2024
=================================

This repository contains materials to run a hybrid scientific simulation/AI
workflow prepared for a joint workshop between NERSC and OLCF. This workflow has
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


Installation (LUMI)
-------------------------

**Step 1:** Load the SmartSim module

.. code:: bash

    module use ...
    module load smartsim


**Step 2:** Compile the mock simulation

.. code:: bash

    mkdir -p build
    cd build
    cmake -Dsmartredis_DIR=$SMARTREDIS_HOME/install/share/cmake/smartredis \
          -Dsmartredis-fortran_DIR=$SMARTREDIS_HOME/install/share/cmake/smartredis-fortran \
          -DCMAKE_INSTALL_PREFIX=../ ..
    make
    cd ..

Running Examples (LUMI)
-----------------------------

.. code:: bash

    module use ...
    module load smartsim
    salloc -N 4 -p standard-g --account=project_465002763 --time=00:10:00
    python driver.py

