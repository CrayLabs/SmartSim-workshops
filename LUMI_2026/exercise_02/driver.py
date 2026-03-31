
from collections import defaultdict

import argparse
import os
import time
from random import *

from smartsim import Experiment

def get_db_settings(system_name):

    port = randint(6000,8000)
    db_settings = defaultdict(lambda: {"interface":"lo0"}) # Note: try "lo" if this does not work
    db_settings["frontier"] = {"interface": ["hsn0","hsn1","hsn2","hsn3"]}
    db_settings["perlmutter"] = {"interface": ["hsn0","hsn1","hsn2","hsn3"]}
    db_settings["LUMI"] = {"interface": ["hsn0","hsn1"], "port": port}
    return db_settings[system_name]

def get_launcher(system_name):
    launcher = defaultdict(lambda: "local")
    launcher["frontier"] = "slurm"
    launcher["perlmutter"] = "slurm"
    launcher["LUMI"] = "slurm"
    return launcher[system_name]

def main(args):

    # Get system-specific settings
    system_name = "LUMI"
    launcher = get_launcher(system_name)
    db_settings = get_db_settings(system_name)

    exp = Experiment("online-training", launcher=launcher)

    # Create the objects for the mock simulation
    if args.fortran_sim:
        rs_sim = exp.create_run_settings("build/mock_fortran_simulation")
    else:
        rs_sim = exp.create_run_settings("build/mock_cpp_simulation")
    if launcher == "slurm":
        rs_sim.set_tasks(4)
        rs_sim.set_nodes(1)
    model = exp.create_model("mock_simulation", rs_sim)
    model.attach_generator_files(to_symlink=["data"])

    # Create the objects for the sampler
    rs_sampler = exp.create_run_settings("python", exe_args="sampler.py")
    if launcher == "slurm":
        rs_sampler.set_tasks(1)
        rs_sampler.set_nodes(1)
    sampler = exp.create_model("sampler", rs_sampler)
    sampler.attach_generator_files(to_symlink=["sampler.py"])

    # Create the objects for the trainer
    rs_trainer = exp.create_run_settings("python", exe_args="trainer.py")
    if launcher == "slurm":
        rs_trainer.set_tasks(1)
        rs_trainer.set_nodes(1)
    trainer = exp.create_model("trainer", rs_trainer)
    trainer.attach_generator_files(to_symlink=["trainer.py"])

    time.sleep(10)
    # Create and configure the database
    db = exp.create_database(**db_settings)

    try:
        exp.start(db)
        exp.generate(model, sampler, trainer, overwrite=True)
        exp.start(model, sampler, trainer, block=True)
    finally:
        exp.stop(model, sampler, trainer, db)

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Train data on the fly")
    parser.add_argument(
        "--fortran-sim",
        action="store_true",
        help="Use the Fortran-based mock simulation"
    )
    args = parser.parse_args()
    main(args)
