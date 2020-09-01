
#this now needs to be a function with an extra argument,
#which tells it to increase the arrivals at specified nodes by x percent
#from a given time onwards (or to a new level by a given time)

#also could make sense to add in a warmup period argument in here


#data table version ####
schedule <- lapply(1:length(nodes),
                   function(y){
                     
                     tmp <- lapply(1:max_time,
                                   function(x){
                                     #sample from arrival distribution for node
                                     node <- nodes[y]
                                     
                                     #to compare orig with data.table versions
                                     set.seed(seed_start+x)
                                     
                                     #data.table scoping works differently than data.frame
                                     #have had to change the extr.rate2 function to take character string instead of object name for lookup
                                     #and then "get" that lookup within the function
                                     #or else it looks in the wrong environment (global instead of local) for its objects
                                     #and fails
                                     arrivals <- extr.rate(current_node=node,
                                                           current_time = x,
                                                           lookup="arrival_lookup")
                                     data.table(time=rep(x,arrivals),node=rep(node,arrivals),event=rep("arrival",arrivals))
                                   })
                     
                     
                     
                     tmp2 <- rbindlist(tmp,
                                       use.names=TRUE)
                   })
#put them all in one dataframe
schedule <- rbindlist(schedule,
                      use.names=TRUE)

#each of these is a unique patient
#no more unique patients will arrive in the system
#this will cease to be true in the next step once the initial service ends are added
#so record the number of unqiue patients now
unique_patients <- nrow(schedule)

#assign each one a unique integer id
schedule$patient_id <- 1:unique_patients
