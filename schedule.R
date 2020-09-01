
#data table version ####
schedule <- lapply(1:length(nodes),
                   function(y){
                     
                     tmp <- lapply(1:max_time,
                                   function(x){
                                     #sample from arrival distribution for node
                                     node <- nodes[y]
                                     
                                     set.seed(seed_start+x)
                                     
                                     arrivals <- extr.rate(current_node=node,lookup="arrival_lookup")
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
