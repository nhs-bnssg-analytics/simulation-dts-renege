library(dplyr)
library(ggplot2)

#1. formatting data ####
#results which need to be processed before dropping the warmup perdiod from the schedule output

#time in queue at point of leaving queue ####
queue_wait_at_end <- schedules %>% filter(
  event %in% c("arrival",
               "transfer",
               "service_start",
               "renege_from",
               "renege_to")) %>%
  mutate(queue_event = case_when(event %in% c("arrival", "transfer","renege_to") & node %in% nodes ~ "join_queue",
                                 event %in% c("service_start","renege_from") ~ "leave_queue",
                                 TRUE ~ "not_a_queue_event")) %>% 
  distinct(rep,
           sim_time,
           time,
           node,
           patient_id,
           queue_event) %>% 
  group_by(rep,
           node,
           patient_id) %>% 
  arrange(sim_time) %>% 
  mutate(lagtime=lag(sim_time),
         total_time_in_queue=sim_time-lagtime) %>%
  ungroup() %>% 
  select(-lagtime,
         -sim_time) %>% 
  filter(queue_event=="leave_queue",
         time>0)


#now subset schedules to post-warmup only
schedules <- schedules[time>0,
                       c("rep","time","node","event","patient_id"),
                       ]


#some other pre-processing - after subsetting to post-warmup
node_data <- node_trackers %>% 
  group_by(time,node) %>% 
  summarise_all(.funs=list(mean=mean,median=median,min=min,max=max,IQR=IQR))


events <- schedules %>% 
  filter(time<=max_time) %>% 
  group_by(event,
           node,
           time,
           rep) %>% 
  tally() %>%
  ungroup()

events_summary <- events %>% 
  group_by(event,
           node,
           time) %>%
  summarise_at(.vars = "n",
               .funs=list(mean=mean,median=median,min=min,max=max,IQR=IQR))

#2. plots ####

#queue size at each node at each time - mean and range over reps ####
queue_plot <- node_data %>% 
  ggplot(aes(x=time,
             y=queue_mean,
             colour=node)) +
  geom_ribbon(aes(ymin=queue_median-queue_IQR,ymax=queue_median+queue_IQR),colour="grey") +
  geom_line(aes(x=time,
                y=queue_mean,
                colour=node)) +
  geom_line(aes(x=time,
                y=queue_median,
                group=1),colour="black",linetype="dotted",alpha=0.5) +
  facet_wrap(.~node,
             scales="free_y") +
  theme(legend.position = "none")

#occupancy - mean and range over reps - for each node at each time ####
occ_plot <- node_data %>% 
  ggplot(aes(x=time,
             y=occupancy_mean,
             colour=node)) +
  geom_line(aes(x=time,
                y=occupancy_mean,
                colour=node)) +
  geom_line(aes(x=time,
                y=maximum_capacity_max),
            colour="blue",
            size=1,
            linetype="dashed") +
  geom_line(aes(x=time,
                y=occupancy_median,
                group=1),colour="black",linetype="dotted",alpha=0.5) +
  facet_wrap(.~node,
             scales="free_y") +
  theme(legend.position = "none")

#mean number of available service channels at each node, at each time ####
available_plot <- node_data %>% 
  ggplot(aes(x=time,
             y=available_mean,
             colour=node)) +
  geom_line() +
  facet_wrap(.~node,
             scales="free_y") +
  theme(legend.position = "none")


#for each time and each node, the mean total wait of patients who left the queue for the node at that time, for whatever reason ####
wait_plot <- queue_wait_at_end %>% 
  group_by(time,
           node) %>% 
  summarise_at(.vars="total_time_in_queue",
               .funs=c("mean","median","IQR","max","min")) %>% 
  ungroup() %>% 
  ggplot(aes(x=time,
             y=mean,
             colour=node)) +
  geom_line() +
  facet_wrap(.~node,
             scales="free_y") +
  theme(legend.position = "none") +
  labs(title="Time spend in queue (on leaving it)")


#event summary nodes ####
node_event_plot <- events_summary %>% 
  filter(node %in% nodes,
         time<=max_time-warmup) %>% 
  ggplot(aes(x=time,
             y=mean,
             colour=event)) +
  geom_line() +
  facet_wrap(.~node,
             scales="free_y") +
  theme(legend.position="bottom")


#even summary exits ####
exit_event_plot <- events_summary %>% 
  filter(node %in% exits,
         time<=max_time-warmup) %>% 
  ggplot(aes(x=time,
             y=mean,
             colour=event)) +
  geom_line() +
  facet_wrap(.~node,
             scales="free_y") +
  theme(legend.position="bottom")



#copes of the tables and plots which have been created above ####

#collect all the processed data in one list
plot_data <- list(node_data=node_data,
                         events=events,
                         events_summary=events_summary,
                         queue_wait_at_end=queue_wait_at_end)

#collect the plots in one list
plots <- list(queue_plot=queue_plot,
                     occ_plot=occ_plot,
                     available_plot=available_plot,
                     wait_plot=wait_plot,
                     node_event_plot=node_event_plot,
                     exit_event_plot=exit_event_plot)

#save copies of the processed data and plots ####
saveRDS(plot_data,"./plot_data.rds")
saveRDS(plots,"./plots.rds")
