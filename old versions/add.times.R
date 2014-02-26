add.times<-function(df){
  require('stringr')
  
  dash.pattern.short='[0-9]{2}[-][0-9]{2}[-][0-9]{2}' # yy/mm/dd verify
  dash.pattern.long='[0-9]{4}[-][0-9]{2}[-][0-9]{2}' # yyyy/mm/dd verify
  slash.pattern.short='[0-9]{2}[/][0-9]{2}[/][0-9]{2}' # mm/dd/yy verify
  slash.pattern.long='(1[0-2]|[1-9])[/]([1-3][0-9]|[1-9])[/][0-9]{2}' # mm/dd/yyyy
  
  if(!is.null(df$X_started_at[1])){
    # mm/dd/yyyy
    if(str_detect(df$X_started_at[1],slash.pattern.long)){ # WORKS
      if(str_detect(df$X_started_at[1],'[0-9]{2}:[0-9]{2}:[0-9]{2}')){ # with seconds
        df$time_start<-as.numeric(strptime(df$X_started_at,"%m/%d/%Y %H:%M:%S",tz='PST'))/60
        df$time_finish<-as.numeric(strptime(df$X_created_at_scambot,"%m/%d/%Y %H:%M:%S",tz='PST'))/60
      }
      else{ # w/o seconds
        df$time_start<-as.numeric(strptime(df$X_started_at,"%m/%d/%Y %H:%M",tz='PST'))/60
        df$time_finish<-as.numeric(strptime(df$X_created_at_scambot,"%m/%d/%Y %H:%M",tz='PST'))/60
      }
    }
    else{ # WORKS
      if(str_detect(df$X_started_at[1],'[0-9]{2}:[0-9]{2}:[0-9]{2}')){ # with seconds
        df$time_start<-as.numeric(strptime(df$X_started_at,"%m/%d/%y %H:%M:%S",tz='PST'))/60
        df$time_finish<-as.numeric(strptime(df$X_created_at_scambot,"%m/%d/%y %H:%M:%S",tz='PST'))/60
      }
      else{ # w/o seconds
        df$time_start<-as.numeric(strptime(df$X_started_at,"%m/%d/%y %H:%M",tz='PST'))/60
        df$time_finish<-as.numeric(strptime(df$X_created_at_scambot,"%m/%d/%y %H:%M",tz='PST'))/60
      }
    }
  }
  else{ # $started_at cases
    df$time_start<-as.numeric(strptime(df$started_at,"%m/%d/%y %H:%M",tz='PST'))/60
    df$time_finish<-as.numeric(strptime(df$created_at,"%m/%d/%y %H:%M",tz='PST'))/60
  }
  
  # rescaling/duration computations
  start.time<-min(min(df$time_finish),min(df$time_start)) # first time stamp in the job
  df$time_start<-df$time_start-start.time
  df$time_finish<-df$time_finish-start.time
  df$time_duration<-df$time_finish-df$time_start
  if(min(df$time_duration)<=0){ print("There are anomalous timestamps, coercing durations to 0.")}
  df$time_duration<-ifelse(df$time_duration>0,df$time_duration,0)
  df$time_duration_log<-log10(df$time_duration)
  
  return(df)
}
