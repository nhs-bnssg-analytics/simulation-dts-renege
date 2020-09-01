# simulation-dts-renege
Script and input template for discrete-time simulation (DTS) solution of a multi-node, multi-server queuing system with reneging.

The model can be run by sourcing the "wrapper.R" script. This reads in the input template "dummy_inputs.csv" and sources the other scripts needed to run the model. It also sources the "output_plots.R" script which produces some example visualisations from the model output.

To change the input network configuration, arrival times, servcie times, and capacities, save an edited version of the input template and change the "input_file" parameter in the wrapper.R script to match it. The length (in simulated time units) of the simualtion, the warm up period, and the number of replciations to perform can also be set by changing the appropraite parameters at the head of the wrapper.R script.

Note that this simple version of the model does not allow any of the input parameters to change over time.
