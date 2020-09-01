#occupancy and maximum capacity at current time
#n.b. as it stands, the model assumes capacity is fixed
#but changes in capacity over time could be achieved by updating
#the maxiumum capacity field of the table for a given time step during the run
#initialised for time zero
#if you wanted some of the service points to be occupied at simulation start
#you would need to add some corresponding rows to the patient_current
#and schedule data.tables
#so that you knew when they would leave
#easier to achieve this through warmup period
#and then if you want rates to change after that, have a separate time dependent
  #arrival rates table
#n.b. this should be done in the setup.R and schedule.R scripts
#(separate version of scripts does this)

node_current <- list()
node_current[[1]] <- data.table(
  time = 0,
  node = nodes,
  occupancy = 0,
  available = inputs[V1%in%nodes,capacity],
  maximum_capacity = inputs[V1%in%nodes,capacity],
  queue = 0
)


current_queue <- data.table(time=integer(),
                    node=character(),
                    patient_id=integer())
