# simulation-dts-renege
Script and input template for discrete-time simulation (DTS) solution of a multi-node, multi-server queuing system with reneging.

## Purpose
This code was written to simulate the flow of patients in discrete time steps (of one day) of patients through various service points (e.g. clinics, therapy services, crisis services, inpatient beds) in a mental health system.

## Versions of the code
The simulation code is presented in two versions - a "Full" version which allows arrival rates, length of service rates, and capacities to change with the (simulation) time index, and a "Light" version for which those parameters remain fixed during the simulation period (n.b. but the actual values at each event/time step are randomly sampled from the given distributions).

The Full version was used to produce the modelling results presented in the paper which references this code. The main difference between the two versions is the complexity of the inputs.

## Light version
The Light version takes a single CSV input file with a row for each service point and exit, and columns corresponding to transition rates between service points (and service points to exits), arrival distribution names and rates, length of service distribution names and rates, capacities, a function to calculate the risk of reneging (leaving the queue) for given lengths of time in the queue, and a second set of transition rates from service points to service points and exits in the event that a renege is realised.

By editing this CSV, the user can create their own set of service points and exits, with appropriate parameters.

## Full version
The Full version takes two additional input CSVs (for simplicity, the network CSV used in the Light version is used in exactly the same form here, but some of its values are over-ridden by those in the supplementary inputs).

- An "arrivals" template, which specifies different arrival and length of service distributions, and capacities, for each service point for given (simulation) time ranges.

- A "postcov_flow" template, which specifies flows between service points and service points/exits, and is used to supersede the flows given in the network template once a given (simulation) time point is passed. This is intended to represent a step change in patient flows as the result of a one-off policy change.


## Running the code
As well as populating the input CSVs, the user must also some values into the wrapper.R script of the code itself - the filepaths of the input CSVs, the number of time units to run the simulation for, a warmup time, and the number of simulation replications to perform.

In the Full version, they must also specify a "surge_time" parameter - this is the (simulated) time point at which the transtion rates between those in the "network" template, and those in the "postcov_flow" template.

They must also create a "./sim_results" folder in their working environment for the outputs to be saved to. The outputs are saved as a list of two dataframes, compressed in RDS format.

Filepaths given in the code are relative, so for both the Full and Light versions, the wrapper.R scripts will source the others if they are all saved in the same folder/working directory. The input files should be placed in that same directory, or the filepaths names in the wrapper.R code amended accordingly, before using.

The Light version here also includes code to produce some example graphics from the outputs.
