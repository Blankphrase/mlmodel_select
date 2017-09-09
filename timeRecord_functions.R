# A function to create a log file and record the time

# This function must be used before timeRecordB()


timeRecordA=function(){
  
  ## create a log file to add time to if the file does not exist
  
  # create a file name based on time, file location to avoid possible overlap
  
  file_name=paste(format(Sys.time(), "%F_%T"),".log",sep="")
  
  file.create(file_name)
  # make the insanely long name a global variable
  extremely_long_name_i_do_not_think_anyone_would_be_sanely_to_use<<-file_name
  
  time_variable= proc.time() 
  write(time_variable,file=file_name,append=TRUE)
  if(file.exists(file_name)){
    
    print("Fun time log has been created")
    
  }else{
    print("unsuccessful in creating a time log file")
  }
  
  
}




# Adding current time to the log. 
# Must be used after creating the log file with timeRecordA()


timeRecordB=function(){
  
  # append to file the current time if the file is already there.  
  if(file.exists(extremely_long_name_i_do_not_think_anyone_would_be_sanely_to_use)){
    file_name=extremely_long_name_i_do_not_think_anyone_would_be_sanely_to_use
    write(proc.time(),file=file_name,append=TRUE)
  }else{
    print("Please use function timeRecordA to create a file first.")
  }
}





# Function to read the log file
# Ability to choose to between output raw log file or file with execution time 

timeRecordR=function(output="job",unit="s"){
  
  # If the file needed to read is there. Then, we could began to read it into R. 
  if(file.exists(extremely_long_name_i_do_not_think_anyone_would_be_sanely_to_use)){
    
    time_dataframe=read.table(extremely_long_name_i_do_not_think_anyone_would_be_sanely_to_use)
    colnames(time_dataframe)=names(proc.time())
    if(output=="raw"){
      time_dataframe=time_dataframe
    }else if(output=="job"){
      time_interval=c(0,diff(time_dataframe[,"elapsed"]))
      if(unit=="s"){
        time_dataframe=cbind(time_dataframe,run_time=time_interval)
      }else if (unit=="min"){
        time_dataframe=cbind(time_dataframe,run_time=time_interval/60)
      }else if (unit=="hr"){
        time_dataframe=cbind(time_dataframe,run_time=time_interval/3600)
      }else{
        print("please enter a valid unit type like s, min, or hr")
      }
    }else{
      print("Enter a valid output type like job or raw")
    }
    
    
  }else{
    print("Please use function timeRecordA to create a file first.")
  }
  
  
  
  
  
  return(time_dataframe)
  
}


