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

#1 Read data ####

inputs <- fread(input_file)


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
destination <- Vectorize(destination)



#2.3 Sample (arrival and service) distribution times ####
#two functions to extract distribution names and parameters from the inputs
cont.to.int<-function(a) floor(a)+rbinom(1,size=1,prob=a-floor(a))
#change this to remove reliance on character strings
#'@param current_node character - node patient is at
#'@param lookup character - the name of a data.table lookup for arrival or service time distributions
extr.rate <- function(current_node,
                      lookup){
  
  lookup <- get(lookup)
  t.dist <- lookup[node==current_node,dist]
  t.par <- lookup[node==current_node,pars][[1]]
  
  #constant rate - same every day
  if (t.dist=="const") {
    return(cont.to.int(t.par)[-1])
    #empirical probability mass function
  } else if (substr(t.dist,1,4)=="epmf") {
    

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
#referenced in the function which samples those rates (extr.rate)

#data.table versions ####
arr_pars <- as.character(copy(inputs[V1 %in% nodes,
                    arr.par,
                    ]))
  
arrival_lookup <- data.table(node=nodes,
                              dist=inputs[V1 %in% nodes,
                                           arr.dist,
                                           ],
                              pars=
                                lapply(arr_pars,
                                       function(x){
                                  as.numeric(
                                    c(1,strsplit(x,";")[[1]])
                                  )
                                       }
                                  )
                              )

serv_pars <- as.character(copy(inputs[V1 %in% nodes,
                     srv.par,
                     ]))
  
service_lookup <- data.table(node=nodes,
                              dist=inputs[V1 %in% nodes,
                                           srv.dist,
                                           ],
                              pars=
                                lapply(serv_pars,
                                       function(x){
                                  as.numeric(
                                    c(1,strsplit(x,";")[[1]])
                                  )
                                       }
                                )
                                )

#transition matrices for ordinary tranfers, and for reneging ####
#this is not strictly necessary
#the code could just point to the relevant subset of the inptus directly
#but it is extracted here for transparency and convenience

transition_matrix <- copy(inputs[V1 %in% nodes,
                              c("V1",nodes,exits),
                              with=FALSE])
setnames(transition_matrix,"V1","node")

renege_matrix <- copy(inputs[V1 %in% nodes,
                          c("V1", paste0("wt_",c(nodes,exits))),
                          with=FALSE])
setnames(renege_matrix,c("V1",paste0("wt_",c(nodes,exits))),c("node",nodes,exits))

#lookup table for chances of transition
transition_lookup <- trans_lookup(trans_matrix = "transition_matrix",
                                   nodes=nodes)

#lookup table for chances of reneging
renege_lookup <- trans_lookup(trans_matrix = "renege_matrix",
                               nodes=nodes)


#pre-calaculate risk of reneging at all times for each node
#do this outside the loop - since the risks are fixed
#then use as lookup inside loop at each time step
#and do die roll inside loop to see if risk is realised


#only do this bit if there is a renge function for at least one service point
if(!all(is.na(inputs$wt_fn))){

renege_risk_lookup <- expand.grid(nodes,0:max_time)
renege_risk_lookup <- as.data.table(renege_risk_lookup)
colnames(renege_risk_lookup) <- c("node","duration_in_state")
renege_risk_lookup[,
                   `:=`(renege_risk=as.numeric(renege_risk_fun(node=node,time=duration_in_state))),
                                           ]
renege_risk_lookup[,
                   `:=`(renege_risk=ifelse(is.na(renege_risk),0,renege_risk))
                   ]

}

