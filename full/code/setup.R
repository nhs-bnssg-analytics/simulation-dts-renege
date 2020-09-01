#1. wrapper
#2. call setup once for all reps
#3. call schedule and state trackers anew for each rep
#4. and the same for sim_loop
#5. call a bind up/store results function as the last bit of the wrapper



#For a given network and capacity configuration
#These just need to be set up once
#and re-used throughout the replications

#the schedule
#and the state_trackers
#will need to be refreshed for each replication
#library(data.table)

#1 Read data ####

inputs <- fread(input_file)

#version with time varying arrivals
#read in new arrivals file
arr_pars <- fread(arrivals_file)

#CHANGE THIS TO BE MORE GENERALISABLE ####
#read in postcov transition matrix - one off swtich ####
postcov_flow <- fread(postcov_flow_file)

  #use automatically assigned column name "V1" as a variable for subsetting
  #becaue if you use the index "1" in the j position, it will return a data.table
nodes <- inputs[!is.na(capacity),
                         V1,
                         ]
exits <- inputs[is.na(capacity),
                         V1,
                         ]



#2 Helper functions ####

#2.1 Make transition lookup tables ####
#used in process data section of setup (s.3)

#'@param trans_matrix data.table - a transition matrix
#'@param nodes character - a string of node names, corresponding to the node column of the transition matrix
trans_lookup <- function(trans_matrix,
                         nodes=nodes){
  
  
  node_list <- lapply(nodes,function(x){
    
    
    trans <- get(trans_matrix)
    
    tmp <- trans[node==x,
                          ,
                          ]
    
    to <- names(tmp)[!is.na(tmp)][-1]
    lower <- cumsum(unlist(tmp[,
                               to,
                               with=FALSE])) -
      unlist(tmp[,
                 to,
                 with=FALSE])
    upper <- cumsum(unlist(tmp[,
                               to,
                               with=FALSE]))
    
    chances <- data.table(to = to,
                          lower= lower,
                          upper = upper)
    
    return(chances)
    
  })
  
  names(node_list) <- nodes
  lookup <- rbindlist(node_list,
                      use.names=TRUE,
                      idcol="from")
  
  return(lookup)
}


#2.2 destinations ####
#used in simulation loop

#data.table version ####
#replaces both destination, and renege destination

#'@param current_node character - name of the node patient is leaving
#'@param chance numeric - value of the "die roll" to see where they go
#'@param lookup data.table - the lookup table (transition or renege) to consult
destination <- function(current_node,
                         chance,
                         lookup){
  
  lookup <- get(lookup)
  res <- lookup[from==current_node & lower<= chance & upper > chance,
                to,
                ]
  res <- ifelse(length(res)==0,current_node,res)
  return(res)
}
#does this need to be vectorised?
destination <- Vectorize(destination)



#2.3 Sample (arrival and service) distribution times ####
#two functions to extract distribution names and parameters from the inputs
#copied from the previous model
#n.b. this cont to ind is used again in the new version - keep it ####
cont.to.int<-function(a) floor(a)+rbinom(1,size=1,prob=a-floor(a))
# extr.rate<-function(type,fac) {
#   t.dist<-inputs[fac,which(names(inputs)==paste0(type,".dist"))]
#   t.par<-as.character(inputs[fac,which(names(inputs)==paste0(type,".par"))])
#   if (t.dist=="const") {
#     return(cont.to.int(as.numeric(t.par)))
#   } else if (substr(t.dist,1,4)=="epmf") {
#     epmf<-as.numeric(strsplit(t.par,split=";")[[1]])
#     epmf<-epmf/sum(epmf)
#     names(epmf)<-0:(length(epmf)-1)
#     return(as.numeric(names(sample(epmf,1,prob=epmf))))
#   } else {
#     # standard parametric distribution
#     return(max(cont.to.int(do.call(get(paste0("r",t.dist)),as.list(c(1,as.numeric(strsplit(t.par,split=";")[[1]]))))),0))
#   }
# }

#data.table version
#change this to remove reliance on character strings
#'@param current_node character - node patient is at
#'@param lookup character - the name of a data.table lookup for arrival or service time distributions
#modified extr.rate function
#adding time filter parameter
extr.rate <- function(current_node,
                      current_time="none",
                      lookup){
  
  lookup <- get(lookup)
  
  if(current_time=="none"){
    t.dist <- lookup[node==current_node,dist]
    t.par <- lookup[node==current_node,pars][[1]]
  } else {
    
    t.dist <- lookup[node==current_node & time == current_time, dist]
    t.par <- lookup[node==current_node & time == current_time, pars][[1]]
    
  }
  
  if (t.dist=="const") {
    return(cont.to.int(t.par)[-1])
  } else if (substr(t.dist,1,4)=="epmf") {
    
    # epmf<-t.par[-1]
    # epmf<-epmf/sum(epmf)
    # names(epmf)<-0:(length(epmf)-1)
    # return(as.numeric(names(sample(epmf,1,prob=epmf))))
    
    #temporary change to allow input in
    #LOS in days/proportion of patients with that LOS in days
    #as los1;prop1;los2;prop2; etc. vector in input
    #since when consultations etc. are weekly then the input vectors
    #will be very long and confusing with a lot of zeros in them
    #if using the previous method
    epmf <- t.par[-1]
    epmf_values <- epmf[seq(1,length(epmf),by=2)]
    epmf_proportions <- epmf[seq(2,length(epmf),by=2)]
    return(sample(epmf_values,1,prob=epmf_proportions))
    
  } else {
    # standard parametric distribution
    return(
      max(
        cont.to.int(
          do.call(
            get(
              paste0("r",t.dist)
            ),
            as.list(t.par)
          )
        ),
        0)
    )
  }
  
}
#does this need to be vectorised?
extr.rate <- Vectorize(extr.rate)



#2.4 renege risk function ####

#only do this bit if there is a renge function for at least one service point
if(!all(is.na(inputs$wt_fn))){

renege_funs <- copy(inputs$wt_fn)
names(renege_funs) <- nodes
renege_params <- lapply(seq_along(nodes),function(x){as.numeric(strsplit(inputs$wt_params[x],split=";")[[1]])})
names(renege_params) <- nodes

renege_risk_fun <- function(node,time){
  
  tmpfun <- eval(parse(text=paste0("function(x,p,node){",renege_funs[node],"}")))
  params <- renege_params[[node]]
  res <- tmpfun(x=time,p=params,node=node)
  return(ifelse(is.numeric(res) & !is.na(res),res,0))
}

renege_risk_fun <- Vectorize(renege_risk_fun)

}

#3 Process data ####


#lookups for arrival/service distributions ####
#names and parameters
#referneced in the function which samples those rates (extr.rate)

arrival_lookup <- 
  lapply(nodes,
         function(y){
           
           times <- lapply(arr_pars[node==y,period,],
                           function(x){c(as.numeric(strsplit(x,";")[[1]][1]):as.numeric(strsplit(x,";")[[1]][2]))}
           )
           
           period_lengths <- sapply(times,length)
           
           times <- unlist(times)
           
           dists <- unlist(lapply(1:length(period_lengths),function(x){
             rep(arr_pars[node==y,arr.dist,][x],period_lengths[x])
           }))
           
           pars <- unlist(sapply(1:length(period_lengths),function(x){
             rep(arr_pars[node==y,arr.par,][x],period_lengths[x])
           }))
           
           pars <- lapply(pars,function(x){c(1,x)})
           
           
           node_arrivals <- data.table(time=times,
                                       node=y,
                                       dist=dists,
                                       pars=pars)
         })

arrival_lookup <- rbindlist(arrival_lookup,use.names=FALSE)


#capacity lookup (time varying) ####
capacity_lookup <- 
  lapply(nodes,
         function(y){
           
           times <- lapply(arr_pars[node==y,period,],
                           function(x){c(as.numeric(strsplit(x,";")[[1]][1]):as.numeric(strsplit(x,";")[[1]][2]))}
           )
           
           period_lengths <- sapply(times,length)
           
           times <- unlist(times)
           
           #capacities
           caps <- unlist(lapply(1:length(period_lengths),function(x){
             rep(arr_pars[node==y,capacity,][x],period_lengths[x])
           }))
           
           node_capacity <- data.table(time=times,
                                       node=y,
                                       capacity=caps)
         })

capacity_lookup <- rbindlist(capacity_lookup,use.names=FALSE)


#length of servcie lookup (time varying) ####
#los lookup (time varying) ####
service_lookup <- 
  lapply(nodes,
         function(y){
           
           times <- lapply(arr_pars[node==y,period,],
                           function(x){c(as.numeric(strsplit(x,";")[[1]][1]):as.numeric(strsplit(x,";")[[1]][2]))}
           )
           
           period_lengths <- sapply(times,length)
           
           times <- unlist(times)
           
           dists <- unlist(lapply(1:length(period_lengths),function(x){
             rep(arr_pars[node==y,srv.dist,][x],period_lengths[x])
           }))
           
           pars <- unlist(sapply(1:length(period_lengths),function(x){
             rep(arr_pars[node==y,srv.par,][x],period_lengths[x])
           }))
           
           pars <- lapply(pars,
                          function(x){
                            as.numeric(
                              c(1,strsplit(x,";")[[1]])
                            )
                          }
           )
             #lapply(pars,function(x){c(1,x)})
           
           
           node_los <- data.table(time=times,
                                  node=y,
                                  dist=dists,
                                  pars=pars)
         })

service_lookup <- rbindlist(service_lookup,use.names=FALSE)

# #length of stay lookup - non-time varying ####
# serv_pars <- as.character(copy(inputs[V1 %in% nodes,
#                      srv.par,
#                      ]))
# 
# service_lookup <- data.table(node=nodes,
#                               dist=inputs[V1 %in% nodes,
#                                            srv.dist,
#                                            ],
#                               pars=
#                                 lapply(serv_pars,
#                                        function(x){
#                                   as.numeric(
#                                     c(1,strsplit(x,";")[[1]])
#                                   )
#                                        }
#                                 )
#                                 )

#transition matrices for ordinary tranfers, and for reneging ####
#this is not strictly necessary
#the code could just point to the relevant subset of the inptus directly
#but it is extracted here for transparency and convenience

#data.table versions ####
transition_matrix <- copy(inputs[V1 %in% nodes,
                              c("V1",nodes,exits),
                              with=FALSE])
setnames(transition_matrix,"V1","node")

renege_matrix <- copy(inputs[V1 %in% nodes,
                          c("V1", paste0("wt_",c(nodes,exits))),
                          with=FALSE])
setnames(renege_matrix,c("V1",paste0("wt_",c(nodes,exits))),c("node",nodes,exits))



#postcov transition matrix
postcov_transition_matrix <- copy(postcov_flow[V1 %in% nodes,
                                         c("V1",nodes,exits),
                                         with=FALSE])
setnames(postcov_transition_matrix,"V1","node")

#lookup table for chances of transition

transition_lookup <- trans_lookup(trans_matrix = "transition_matrix",
                                   nodes=nodes)

#lookup table for chances of reneging
renege_lookup <- trans_lookup(trans_matrix = "renege_matrix",
                               nodes=nodes)


#postcov transition lookup
postcov_transition_lookup <- trans_lookup(trans_matrix = "postcov_transition_matrix",
                                  nodes=nodes)


#pre-calaculate risk of reneging at all times for each node
#do this outside the loop - since the risks are fixed
#then use as lookup inside loop at each time step
#and do die roll inside loop to see if risk is realised


#data.table versions

#only do this bit if there is a renge function for at least one service point
if(!all(is.na(inputs$wt_fn))){

renege_risk_lookup <- expand.grid(nodes,0:max_time)
# renege_risk_lookup <- expand.grid(names(renege_funs[renege_funs!="" & !is.na(renege_funs)]),0:max_time)
renege_risk_lookup <- as.data.table(renege_risk_lookup)
colnames(renege_risk_lookup) <- c("node","duration_in_state")
renege_risk_lookup[,
                   `:=`(renege_risk=as.numeric(renege_risk_fun(node=node,time=duration_in_state))),
                                           ]
renege_risk_lookup[,
                   `:=`(renege_risk=ifelse(is.na(renege_risk),0,renege_risk))
                   ]

}

