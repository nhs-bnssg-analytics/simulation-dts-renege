library(data.table)


#for the "empf" distribution
#you put a semicolon separated list
#of the form value1;proportion1;value2,proportion2; etc.
#into the pars column of the input spreadsheet
#(if and only if you are using empf as your distribution)
#e.g. if you want a probability of LOS of
#10 days 50%, 14 days 25%, 20 days 15%, and 22 days 10%
#you would enter 10;0.5;14;0.25;20;0.15;22;0.1
#and the function will sample accordingly

#set these inputs
#WARNING - max time here needs to have the warmup added onto it
  #consider changing to make more intuitive
max_time <- 801 + 730
warmup <- 730
reps <- 35

#time at which the post-covid surge starts ####
#i.e. this is the first day of June 2020
#for the max_time and warmup values above,
  #lockdown starts at time 1096 and runs until 1165, and then the "surge" begins and is run for a year
#used to decide whether to switch to a new transition matrix
surge_time <- 1166



#network setup
input_file <- "./network_baseline.csv"

#time varying parameters - arrivals, lengths of service, capacities
arrivals_file <- "./arrivals_baseline.csv"

#postcov transitions
postcov_flow_file <- "./postcov_baseline.csv"

#leave the rest alone
source("./setup.R")

#set to an appropriate number of CPU cores to use
#if you don't know how many you have, parallel::detectCores() will tell you
#suggestion - don't use all of them (or your computer might freeze)
cluster <- parallel::makeCluster(35)
parallel::clusterExport(cluster,
                        c("max_time",
                          "surge_time",
                        "input_file",
                        "inputs",
                        "nodes",
                        "exits",
                        "trans_lookup",
                        "destination",
                        "cont.to.int",
                        "extr.rate",
                        "arrival_lookup",
                        "service_lookup",
                        "capacity_lookup",
                        "transition_matrix",
                        "renege_matrix",
                        "transition_lookup",
                        "postcov_transition_lookup",
                        "renege_lookup",
                        "renege_risk_lookup"))

parallel::clusterEvalQ(cluster,library("data.table"))
       
                           
tmp <- parallel::parLapply(cl=cluster,
                      X = 1:reps,
            fun = function(rep){
  
  seed_start <- 10000*rep
  
  source("./schedule.R",local=TRUE)
  source("./state_trackers.R",local=TRUE)
  source("./sim_loop.R",local=TRUE)
  
  node_tracker <- rbindlist(node_current,
                         use.names=TRUE)
  rm(current_queue)
  rm(unique_patients)
  rm(seed_start)
  
  res <- list(schedule,
              node_tracker)
  
  rm(schedule)
  rm(node_current)
  rm(node_tracker)
  
  return(res)
  
})

parallel::stopCluster(cluster)

#extract the schedules and node informatino from each rep
#and collapse into a single table for each
#with a column to inidicate which rep it came from
schedules <- rbindlist(lapply(tmp,function(x){x[[1]]}),
                       use.names = TRUE,
                       idcol="rep")


#distinguish warmup period from intented simulation results period
#should NOT drop and re-index at this point because start times are need for
  #patients who leave the queue during the simulation period,
  #but had joined during the warmup period
  #in order to calculate their time in queue at end
#Instead, create a new result_time column

schedules[,sim_time:=time,]  
schedules[,time:=sim_time-(warmup+1),]

#then drop the sim_time and subset to time>0 for the analysis
#AFTER the total times in queue have been calculated

#note that the time IS being re-indexed to the post-warup time here
#since no further calculations need to be done with it before using it
node_trackers <- rbindlist(lapply(tmp,function(x){x[[2]]}),
                       use.names = TRUE,
                       idcol="rep")
#drop system state during warmup
node_trackers <- node_trackers[time>warmup,,]
#re-index time to start from post-warmup
node_trackers[,time:=time-warmup,]

saveRDS(list(schedules=schedules,
             node_trackers=node_trackers),
        "./sim_results/baseline.rds")


source("./output_plots.R")
