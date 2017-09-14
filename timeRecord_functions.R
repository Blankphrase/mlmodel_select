# A function to create a log file and record the time


# Adding current time to the log. 
# This function will create a log if there is no current log in the Rsession. 

timeRecordB=function(output_message="None"){
  
  
  # append to file the current time if the file is already there.  
  if(exists("extremely_long_name_i_do_not_think_anyone_would_be_sanely_to_use")){
    
    file_name=extremely_long_name_i_do_not_think_anyone_would_be_sanely_to_use 
    
    write(c(output_message,proc.time()),ncolumns = 6 ,file=file_name,append=TRUE,sep=",")
  }else{
    file_name=paste(format(Sys.time(), "%F_%T"),".log",sep="")
    
    file.create(file_name)
    # make the insanely long name a global variable
    extremely_long_name_i_do_not_think_anyone_would_be_sanely_to_use<<-file_name
    
    time_variable=c(output_message,proc.time())
    
    write(time_variable,file=file_name,ncolumns = 6 ,append=TRUE,sep=",")
    if(file.exists(file_name)){
      
      print("Fun time log has been created")
      
    }else{
      print("unsuccessful in creating a time log file")
    }
  }
}





# Function to read the log file
# Ability to choose to between output raw log file or file with execution time 




timeRecordR=function(unit="s",ignore=1){
  
  # If the file needed to read is there. Then, we could began to read it into R. 
  if(file.exists(extremely_long_name_i_do_not_think_anyone_would_be_sanely_to_use)){
    
    time_dataframe=read.table(extremely_long_name_i_do_not_think_anyone_would_be_sanely_to_use,sep=",")
    
    colnames(time_dataframe)=c("output_message",names(proc.time()) )
    
    
    time_interval=c(0,diff(time_dataframe[,"elapsed"]))
    
    # for different time unit, we will give values for differnt run_time.
    switch(unit,
           s={time_dataframe=cbind(time_dataframe,run_time=time_interval)}
           ,min={time_dataframe=cbind(time_dataframe,run_time=time_interval/60)}
           ,hr={time_dataframe=cbind(time_dataframe,run_time=time_interval/3600)} )
    library(dplyr)
    library(magrittr)
    time_dataframe=time_dataframe%>%filter(run_time>ignore)
    
    
  }else{
    print("Please use function timeRecordB to create a log file first.")
  }
  
  
  
  
  
  return(time_dataframe)
  
}


