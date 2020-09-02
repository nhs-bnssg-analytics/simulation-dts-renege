library(data.table)


#note that in the setup,
#the extr.rate function has now been changed (2020-07-20)
#for the "empf" distribution
#you now put a semicolon separated list
#of the form value1;proportion1;value2,proportion2; etc.
#into the pars column of the input spreadsheet
#(if and only if you are using empf as your distribution)
#e.g. if you want a probability of LOS of
#10 days 50%, 14 days 25%, 20 days 15%, and 22 days 10%
#you would enter 10;0.5;14;0.25;20;0.15;22;0.1
#and the function will sample accordingly

#set these inputs
#WARNING - max time here needs to be number of days you want in simulate period PLUS whatever
  #warmup period you think it will take the system to get to an approriate starting
  #state (from empty)
  #consider changing to make more intuitive
max_time <- 365 + 50
#how long to run it from empty for to get to a reasonable starting state
  #warmup period will be chopped off the results
  #so set to zero in the first instance if you need to specifically investigate
  #how long the warmup period should be
warmup <- 50
#n.b. recommend doing more than 10
reps <- 10
#name of your input csv
input_file <- "./network_baseline.csv"


#leave the rest alone
source("./setup.R")

#set to an appropriate number of CPU cores to use
#if you don't know how many you have, parallel::detectCores() will tell you
#suggestion - don't use all of them (or your computer might freeze)
cluster <- parallel::makeCluster(18)
parallel::clusterExport(cluster,
                        c("max_time",
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
                        "transition_matrix",
                        "renege_matrix",
                        "transition_lookup",
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

#save the results somewhere
#remember - the node_trackers have already had the warmup period lopped off
#but the schedules haven't (that will be done in the outputs script)
saveRDS(list(schedules=schedules,
             node_trackers=node_trackers),
        "./results.rds")


source("./output_plots.R")
