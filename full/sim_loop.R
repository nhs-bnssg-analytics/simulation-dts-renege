start <- Sys.time()

for(i in 1:max_time){
  
  
  #to compare orig and data.table
  seed <- seed_start+i
  
  #n.b. throughout the code the current time is i
  #but the element of the node_current record which corresponds to this time is element i+1
  #because the record is initilised at time=0
  #but R indexes from 1, not zero
  
    #use the copy function to make a new copy of the previous tables
    #or else the previous version will be updated by reference
    #and everything will go wrong
      #subsequently, the new table will be updated by reference within the loop

  node_current[[i+1]] <- copy(node_current[[i]])
  node_current[[i+1]][,
                       time := i,
                       ]
  
  #update time-dependent capacity
  change_in_capacity <- capacity_lookup[time==i,capacity,]-node_current[[i+1]][,maximum_capacity]
  new_availability <- node_current[[i+1]][,maximum_capacity,] +
    change_in_capacity -
    node_current[[i+1]][,occupancy,]
  new_availabity <- ifelse(new_availability>0,new_availability,0)
    
  node_current[[i+1]][,
                      maximum_capacity := capacity_lookup[time==i,capacity,],
                      ]
  
  #and availability
  node_current[[i+1]][,
                      available := new_availability,
                      ]
  
  
  
  #4.2 end service (and downstream transfer) ####
  
  #find all patients due to end current active service at this time
  #service end is unconditional
  #so we can do it all at once, instead of by service point

  
  #data.table version ####
  tmp <- copy(schedule[time==i & event == "service_end",
                    ,
                    ])
  
  #data.table version ####
  if(nrow(tmp)>0){
    #get one sample for each row from the uniform distribution
    #use as the "die roll" against transition probabilities to deterimine next destination
    
    #seed 1 data.table ####
    set.seed(seed)
    
    die_roll <- runif(n=nrow(tmp),min=0,max=1)
    #create the new rows - arrival at/transition to new unit at this time
    #or end of treatment/exit system
      
    new_rows <- copy(tmp)
    
    #update by reference
    new_rows[,
         `:=`(die_roll=die_roll,
              time=i)]
    
    new_rows[,
         `:=`(node=destination(current_node=node,
                                chance=die_roll,
                                lookup=ifelse(i<surge_time,
                                  "transition_lookup",
                                  "postcov_transition_lookup"))
                               ),
         ]
    
    new_rows[,
         `:=`(event=ifelse(node %in% nodes,"transfer","treatment_ended"))]
    
    new_rows[,
         `:=`(die_roll=NULL),
         ] 
    
    
    #add the new rows to the schedule
    schedule <- rbindlist(list(schedule,
                                 new_rows),
                            use.names = TRUE)
    
    tmp[,
         `:=`(event=NULL,
              patient_id=NULL),
         ]
    
    tmp[,
         n:=.N,
         by=node]
    
    
    #for the nodes in spaces_freed,
    #subtract those values from the node_current occupancy
    #and add those values to the node_current available
    
    
    #occ goes down
    #available goes up
    node_current[[i+1]][unique(tmp),
                         on="node",
                         `:=`(occupancy=occupancy-n,
                              available=available+n)]
    
    
    
    #remove temporary objects
    rm(die_roll)
    rm(new_rows)
    
  }
  
  #remove the temporary dataframe of current time service ends
  #so that we can reuse the label "tmp" without confusion
  #n.b. needs to be done outside the if statement above, since it's used as the condition
  rm(tmp)
  
  #4.4 start service ####
  # - only if waiting time is > 0 (so transfers and reneges cannot start the same day)
  #i.e. patients are only drawn from queues
  #not from immediate arrivals/transfers/reneges
  #look at the current queue for patients queueing for this node
  #then check if there is free capacity
  #if so, change their status to "active_service"
  #else they continue queueing
  #This needs to be done by node, since it is conditional on available capacity
  #Which patients are selected in the case of queueing patients > available capacity is conditional on time waited
  
  
  for(x in nodes){
  
    #data.table version ####
    
    #check that there are both patients waiting to queue
    #and capacity for at least some of them to start active service
    if(nrow(current_queue[node==x &
                  time<i,
                  ,
                  ])>0 && 
       node_current[[i+1]][node==x,
                           available,
                           ] > 0){
      
      #the number of patients queuing at the node who can now start
      number_of_starts <- min(node_current[[i+1]][node==x,
                              available,
                              ],
          nrow(current_queue[node==x &
                               time<i])
      )
      
      node_queue <- copy(current_queue[node==x &
                                         time<i,
                                       ,
                                       ])
      #arrange queueing patients in descending order of waiting time
      setorder(node_queue,time)
      
      #pull of as many as there are available spaces for at the node, from the top
      #n.b. need to specify the minimum of the available capacity and the number of rows in new_starts
      #otherwise R will replicate extra empty rows
      #which will then be partially populated (but without patient ids, because they're empty rows)
      #when new_rows is created below
      
      
      #update schedule
      new_starts <- data.table(time=i,
                               node=x,
                               event="service_start",
                               patient_id=node_queue[1:number_of_starts,
                                                     patient_id,
                                                     ])
      
      new_ends <- data.table(time=i,
                             node=x,
                             event="service_end",
                             patient_id=node_queue[1:number_of_starts,
                                                   patient_id,
                                                   ])

      
      new_ends[,
               time := time + extr.rate(current_node = node,
                                        current_time = i,
                                         lookup = "service_lookup"),
               ]
      
      schedule <- rbindlist(list(schedule,
                                   new_starts,
                                   new_ends),
                              use.names = TRUE)
      
      
      #update node_current
      #occupancy goes up
      
      node_current[[i+1]][node==x,
                           occupancy := occupancy + nrow(new_starts),
                           ]
      
      #availabilty goes down
      node_current[[i+1]][node==x,
                          available := maximum_capacity-occupancy,
                          ] 
      
      #queue goes down
      node_current[[i+1]][node==x,
                           queue := queue - nrow(new_starts),
                           ]
      
      #remove these patients from the queue
        #they will be added back into their new queues at the end
      current_queue <- current_queue[!patient_id %in% new_starts$patient_id,,]
      
      #remove temporary objects
      rm(new_ends)
      rm(new_starts)
      rm(node_queue)
      rm(number_of_starts)
    }
     
  }
  rm(x)
  
  
  #4.5 reneges (including transfers) ####
  #event="renege_to", node=name of node/exit reneging to
  #if reneging to exit, update patient record
  #else, treat as per arrival
  
  #calculate renege risk at this time
  
  #find all patients currently queueing

  #data.table version ####
  
  if(exists("renege_risk_lookup")){
  
  if(nrow(current_queue)>0){
    
    #seed2 data.table ####
    set.seed(seed+1)

    die_roll_1 <- runif(n=nrow(current_queue),min=0,max=1)
    
    #use renege risk funs[node](x=duration_in_state,p=renege_params[node]) to get risk score for each row
    #use unif die roll to decide if risk is realised for each row
    #subset to those rows where renege risk is realised
    
    current_queue[,
          duration_in_state:=i-time,
          ]
    
    setkey(renege_risk_lookup,node,duration_in_state)
    setkey(current_queue,node,duration_in_state)
    
    current_queue[,
         `:=`(renege_risk=ifelse(duration_in_state==0,
                                 0,
                                 renege_risk_lookup[.SD,on=c("node","duration_in_state"),renege_risk]),
              die_roll_1=die_roll_1),
         ]
    
    current_queue[,
         renege_realised:=ifelse(!is.na(renege_risk) & die_roll_1<renege_risk,1,0),
         ]
    
    
    if(nrow(current_queue[renege_realised==1,,])>0){
    
       
      #remove the renegers from their current queue
      #(they will be added to their new queue in the arrivals/tranfers section 4.6.2)
      #(n.b. note that reneges to outside the system will not be added to a new queue
      #there, and they have already left here)
      
      tmp <- current_queue[renege_realised==1,
           list(n=.N),
           by=node]

      node_current[[i+1]][unique(tmp),
                         on="node",
                         queue:=queue-n
        
      ]
      
      rm(tmp)
      
      #sample onward direction for those rows in manner directly analagous to service completion,
      #but using the renege matrix
      #assign them a "renege_to" label
      
      #seed3 data.table ####
      set.seed(seed+2)
      
      die_roll_2 <- runif(n=nrow(current_queue[renege_realised==1,,]),min=0,max=1)
      
      #update the uncondtional event schedule, to indicate these patients are now reneging
      schedule <- rbindlist(list(schedule,
                                  current_queue[renege_realised==1,
                                        list(time=i,
                                             node=destination(current_node = node,
                                                              chance=die_roll_2,
                                                              lookup="renege_lookup"),
                                             event="renege_to",
                                             patient_id=patient_id),
                                        ],
                                  current_queue[renege_realised==1,
                                        list(time=i,
                                             node=node,
                                             event="renege_from",
                                             patient_id=patient_id),
                                        ])
                              )
      
      
      rm(die_roll_2)
      
    }
    
    #remove the renegers from the queue ####
    current_queue <- current_queue[renege_realised!=1,
                                   ,
                                   ]
    
    #restore the queue to its original three column state
    current_queue[,
          `:=`(duration_in_state=NULL,
               die_roll_1=NULL,
               renege_risk=NULL,
               renege_realised=NULL),
          ]
    
    rm(die_roll_1)
  }
  
  }
  
  #4.6 (external) arrivals 
  #need to treat both "arrival" and "transfer" the same
  #need to also deal with "treatment ended" - change their status to that, and their node to their exit node, and start duration from zero
  #they are labelled differently for interpretability
  #but the only difference is arrivals are external and transfers are internal
  #all that needs to happen here is to update the patient_current
  #[and maybe to node_current, although queue size is not being tracked there as it is]
  
  #4.6.1 
  
  #4.6.2 queue starts ####
  
  queue_joiners <- copy(schedule[time==i & 
                                event %in% c("arrival","transfer","renege_to") & 
                                !node %in% exits,
                              ,
                              ])
  
  #data.table version ####
  if(nrow(queue_joiners)>0){
    
    queue_joiners[,
                   `:=`(time=time,
                        node=node,
                        patient_id=patient_id,
                        event=NULL),
                   ]
    
    
    #check ####
    if(length(intersect(queue_joiners$patient_id,current_queue$patient_id))>0){
      
      stop(paste(intersect(queue_joiners$patient_id,current_queue$patient_id)))
    }
    
    current_queue <- rbindlist(list(current_queue,
                            queue_joiners))
    

    #update queue size column of node record

    queue_joiners[,
                  patient_id:=NULL,
                  ]
    
    queue_joiners[,
                   n:=.N,
                   by=node]
    
    node_current[[i+1]][unique(queue_joiners),
                         on="node",
                         queue:=queue+n]
  }
  
  rm(queue_joiners)
  
  
  #4.6.3 patients who have left the system ####
  system_leavers <- copy(schedule[time==i & 
                                 event %in% c("treatment_ended","renege_to") & 
                                 node %in% exits,
                               ,])
  

  #data.table version ####
  if(nrow(system_leavers)>0){
    
    current_queue <- current_queue[!patient_id %in% system_leavers$patient_id,
                   ,
                   ]
    
  }
  
  rm(system_leavers)
  
  #end of simulation loop ####

}

end <- Sys.time()

loop_duration <- end-start
print(loop_duration)