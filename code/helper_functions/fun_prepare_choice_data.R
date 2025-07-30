prepare_choice_data <- function(file){ 
  
  # get model and problem details  
  model <- regmatches(file,  gregexpr("summary|summary_decreasing|roundwise|roundwise_decreasing", file))[[1]]
  problem <- regmatches(file,  gregexpr("SR_small|SR_large|RR|RR2", file))[[1]]
  
  newfile <- paste0("data/", "choices/", "choices_", model,"_",problem,".rds.bz2")
  if(file.exists(newfile)){
    file.remove(newfile)
    print(paste0("Removed existing (old) file: ", newfile, ". New file will be prepared."))
  } else{ 
    print(paste0("File ", newfile, " will be prepared."))}

  simulation <-  read_rds(file)
  problems <- read_rds(paste0("data/problems/",problem,".rds"))
  if(model=='summary'){
    problems <- problems |> 
      mutate(# scaling
             o1_1 = .01*o1_1 , 
             o1_2 = .01*o1_2 , 
             o2_1 = .01*o2_1 , 
             o2_2 = .01*o2_2
             )
    }
  data <- left_join(simulation, problems, by=join_by(id)) # add problem features
  
  params <- grepl("decreasing", model)
  
  if(params){ 
    data <- data |>  
      group_by(base, rate, theta, id, agent)
  } else{
    data <- data |>  
      group_by(psi, theta, id, agent)
    }
  
  # compute summaries of choice trials
  data <- data |>  
    mutate(n_smp = n() , # number of samples
           o1_smp = sum(is.na(out_2)) , # sample size option 1
           o2_smp = n_smp - o1_smp , # sample size option 2
           o1_sp1 = round(sum(if_else(out_1 == o1_1, 1, 0), na.rm = TRUE)/o1_smp, 2), # sampled probability option 1 outcome 1
           o1_sp2 = round(1 - o1_sp1, 2), # sampled probability option 1 outcome 2
           o2_sp1 = round(sum(if_else(out_2 == o2_1, 1, 0), na.rm = TRUE)/o2_smp, 2), # sampled probability option 2 outcome 1
           o2_sp2 = round(1 - o2_sp1, 2), # sampled probability option 2 outcome 2
           o1_avg = round(mean(out_1, na.rm = TRUE), 2) , # sampled average option 1  
           o2_avg = round(mean(out_2, na.rm = TRUE), 2) # sampled average option 2
    ) |> 
    ungroup() |> 
    filter(!is.na(choice)) |>  # discard samples without choice
    mutate(model = model) 
  
  if(model=='summary'){
    data <- data |> 
      mutate(# reverse scaling
        o1_1 = 100*o1_1 , 
        o1_2 = 100*o1_2 , 
        o2_1 = 100*o2_1 , 
        o2_2 = 100*o2_2
      )
  }
  
  
  if(params){
    data <- data |> 
      select(model, base, rate, theta, id, agent, n_smp, o1_smp, o2_smp, o1_sp1, o1_sp2, o2_sp1, o2_sp2, o1_avg, o2_avg, choice, 
             o1_1:o2_rare)
  }else{
    data <- data |> 
      select(model, psi, theta, id, agent, n_smp, o1_smp, o2_smp, o1_sp1, o1_sp2, o2_sp1, o2_sp2, o1_avg, o2_avg, choice, 
             o1_1:o2_rare)
  }
  
  #data <- data |> rename(smp = "n_smp")
  #checksum_choices <- digest(choices, "sha256")
  
  write_rds(data, newfile, compress = "bz2")
  
  if(file.exists(newfile)){
    return(paste("File ", newfile, "was succesfully generated."))
  } else{ 
      return("No file was generated.")}
}
