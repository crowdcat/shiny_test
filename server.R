###contributors
###outstanding: comparison graphs, broaden last submit options
###more contrib grouping options, DB pulls.
### Last updated 02/13/2014

require('shiny')
require('datasets')
require('data.table')
require('plyr')
require('rCharts')
require('ggplot2')
require('devtools')

options(stringsAsFactors=F)
#increase max to 150 MB
options(shiny.maxRequestSize=150*1024^2)

source('add.times.R')
source('trust_buckets.R')
source('reject_user_function.R')


shinyServer(function(input, output, session) {
  ### Render Image
  output$futurama <- renderText ({
    image_path = "http://cf-public-view.s3.amazonaws.com/coolstuff/fry_not_sure_if.png"
    html_image = paste("<img src=", image_path, " width=\"75%\"/>", sep="")
    paste(html_image)
    
  })
  # the picture of Scamlock Holmes to go into tis tab
  output$scamlock <- renderText ({
    image_path = "http://cf-public-view.s3.amazonaws.com/catherine/Scamlock%20Holmes.png"
    html_image = paste("<img src=", image_path, " width=\"20%\" align=\"right\"/>", sep="")
    
    paste(html_image)
    
  })
  
  ### read in file 
  full_file_raw <- reactive({
    #     if (is.null(input$files[1]) && input$job_id==0) {
    #       # User has not uploaded a file yet
    #       return(NULL)
    #     } else {
    #       inFile <- input$files
    #       full_file = read.csv(inFile$datapath, na.strings="NaN", stringsAsFactors=FALSE)
    #       return(full_file)
    
    if (is.null(input$files) && input$job_id==0) {
      # User has not uploaded a file yet
      return(NULL)
    } else if (input$job_id > 0) {
      print("In job id")
      inFile = input$job_id
      #tryCatch(
      system(paste('s3cmd --force get s3://crowdflower_prod/f',
                   inFile,'.csv.zip /tmp/f', inFile, '.zip',sep=''),
             intern=T)
      
      system(paste('unzip -o /tmp/f', inFile, '.zip -d /tmp', sep=''))
      val <- try(read.csv(paste('/tmp/f',inFile,'.csv',sep='')), silent=TRUE)
      if (inherits(val, "try-error")) {
        stop(paste("Sorry, we could not find your file. Either job", input$job_id,
                   "does not exist or its Full Report has not been generated yet.",
                   "Check", paste("http://crowdflower.com/jobs/",input$job_id,"/reports.",sep=""),
                   "\nRefresh this page before uploading a csv."))
      } else {
        full_file = read.csv(paste('/tmp/f',inFile,'.csv',sep=''))
        log_id = inFile
      }
    } else {
      
      full_file = read.csv(input$files$datapath, na.strings="NaN", stringsAsFactors=FALSE)
      log_id = input$files$name
    }
    return(full_file)
  })
  
  # add new columnds to full file like assignment durection and last submission
  full_file <- reactive({
    if ((is.null(input$files[1]) || is.na(input$files[1])) && input$job_id==0) {
      # User has not uploaded a file yet
      return(NULL)
    } else {
      full_file = full_file_raw()
      # add a tainted column if it's missing
      if (!("X_tainted" %in% names(full_file))) {
        full_file$X_tainted = "false"
      }
      # add a _golden column if it's missing
      if (!("X_golden" %in% names(full_file))) {
        full_file$X_golden = "false"
      }
      # add index to be able to subset data
      full_file$X_index = 1:nrow(full_file)
      # new created at for scambot to use
      full_file$X_created_at_scambot = full_file$X_created_at
      # convert the real _created_at for all other functions
      full_file$X_created_at = as.POSIXct(full_file$X_created_at,
                                          format='%m/%d/%Y %H:%M:%S')
      # add last_submit and num_judgments as columns
      full_file = ddply(full_file, .(X_worker_id), mutate,
                        last_submit = X_created_at[length(X_created_at)],
                        num_judgments = length(X_worker_id))
      
      full_file = add.times(full_file)
      View(full_file)
      return(full_file)
    }
  })
  
  # this function subsets full_file by left bar controls (Selector for left panel!)
  subsetted_file <- reactive({
    if ((is.null(input$files[1]) || is.na(input$files[1])) && input$job_id==0) {
      # User has not uploaded a file yet
      return(NULL)
    } else {    
      full_by_worker = full_file()
      
      print("DF from subsetted_file")
      print(head(full_by_worker))
      
      if (input$country_chosen != "all_countries") {
        print("Subsetting by country")
        full_by_worker = full_by_worker[full_by_worker$X_country == input$country_chosen,]
        
      }
      
      if (input$channel_chosen != "all_channels") {
        print("Subsetting by channel")
        full_by_worker = full_by_worker[full_by_worker$X_channel == input$channel_chosen,]
      }
      
      if(input$time_chosen != "none"){
        print("Subsetting by time")
        current_time = as.POSIXlt(Sys.time(), "GMT")
        print(current_time)
        if(input$time_chosen == "thirty_mins"){
          #subtracting 30 mins from current time
          time = current_time - 1800
          full_by_worker = full_by_worker[full_by_worker$last_submit >= time, ]
          
        }
        else if (input$time_chosen == "last_hr"){
          time = current_time - 3600
          full_by_worker = full_by_worker[full_by_worker$last_submit >= time, ]
        }
        else if (input$time_chosen == "six_hrs"){
          time = current_time - 21600
          full_by_worker = full_by_worker[full_by_worker$last_submit >= time, ]
        }
        else if (input$time_chosen == "twelve_hrs"){
          time = current_time - 43200
          full_by_worker = full_by_worker[full_by_worker$last_submit >= time, ]
        }
        
        else if (input$time_chosen == "last_day"){
          time = current_time - 86400
          full_by_worker = full_by_worker[full_by_worker$last_submit >= time, ]
        }
        else if (input$time_chosen == "last_week"){
          time = current_time - 604800
          full_by_worker = full_by_worker[full_by_worker$last_submit >= time, ]
        }
      }
      
      if (!is.null(input$num_judgs)) {
        print("Subsetting by judgments")
        full_by_worker = full_by_worker[full_by_worker$num_judgments <= max(input$num_judgs) &
                                          full_by_worker$num_judgments >= min(input$num_judgs),]
        
      }
      
      if (!is.null(input$trust_chosen)) {
        print("Subsetting by trust")
        full_by_worker = full_by_worker[full_by_worker$X_trust <= max(input$trust_chosen) &
                                          full_by_worker$X_trust >= min(input$trust_chosen),]
      }
      full_by_worker
    }
  })
  
  ##Get Job ID from name of input file
  job_id <- reactive({
    if ((is.null(input$files[1]) || is.na(input$files[1])) && input$job_id==0) {
      # User has not uploaded a file yet
      return(NULL)
    } else if (input$job_id > 0) {
      return(input$job_id)
    } else {
      inFile <- input$files$name
      job_id = gsub(inFile, pattern="^f", replacement="")
      job_id = str_extract(job_id, "\\d{6}")
      #job_id = gsub(job_id, pattern="\\.csv", replacement="")
      return(job_id)
    }
  })
  
  ### this function gets a single row for every worker to display in contributor table by id
  agg_by_worker <- reactive({
    if ((is.null(input$files[1]) || is.na(input$files[1])) && input$job_id==0) {
      # User has not uploaded a file yet
      return(NULL)
    } else {
      full_file = subsetted_file()
      print("Before ddply in agg_by_worker")
      agg_by_worker = ddply(full_file, .(X_worker_id), summarize,
                            channel = X_channel[1],
                            country = X_country[1],
                            ip = paste(unique(X_ip), collapse="<br>"),
                            num_ips = length(unique(X_ip)),
                            trust = X_trust[1],
                            untrusted = X_tainted[1],
                            last_submit = last_submit[1],
                            num_judgments = num_judgments[1])
      print("After ddply in agg_by_worker")
      print(names(agg_by_worker))
      agg_by_worker
    }
  })
  
  # matches the functionality of agg_by_worker; note that the actual subsetting is done later
  agg_by_ip <- reactive({
    if ((is.null(input$files[1]) || is.na(input$files[1])) && input$job_id==0) {
      # User has not uploaded a file yet
      return(NULL)
    } else {
      full_file_ip = full_file()
      full_by_ip = ddply(full_file_ip[], .(X_ip), summarize,
                         channel_ip = X_channel[1],
                         country_ip = X_country[1],
                         #worker_ids = unique(X_worker_id),
                         worker_ids = paste(unique(X_worker_id),collapse="<br>"),
                         num_work_ids = length(unique(X_worker_id)),
                         trust_ip = X_trust[1],
                         untrusted = X_tainted[1],
                         last_submit_ip = X_created_at[length(X_created_at)],
                         num_judgments_ip = length(X_ip))
      #test_qs_seen_ip = length(X_ip[X_golden=='true']))
      
      full_by_ip
    }
    
  })
  
  # obtains the slice of the data frame that refers to a particular id
  full_file_contrib_id <- reactive({
    if ((is.null(input$files[1]) || is.na(input$files[1])) && input$job_id==0 || input$id_chosen_profiles=="") {
      # User has not uploaded a file yet
      return(NULL)
    } else {
      full_file_contrib_id = full_file()
      id = input$id_chosen_profiles
      full_file_contrib_id = full_file_contrib_id[full_file_contrib_id$X_worker_id == id,]
      full_file_contrib_id
    }
    
  })
  
  
  
  ###Used to record golds and units worked on, as well as contributor location info
  distros <-reactive({
    if ((is.null(input$files[1]) || is.na(input$files[1])) && input$job_id==0){
      return(NULL)
    } else{
      full_file_distros = full_file()
      full_by_distros = ddply(full_file_distros, .(X_worker_id), summarize,
                              num_ips = length(unique(X_ip)),
                              units = paste(unique(X_unit_id), collapse=", "),
                              num_units = length(X_unit_id),
                              golds = paste(X_unit_id[X_golden == "true"], collapse=", "),
                              num_golds = length(X_unit_id[X_golden == "true"]),
                              num_non_golds = length(X_unit_id[X_golden != "true"])
      )
      full_by_distros    
      
    }
    
  })
  
  ### To Collect Gold Cols and Answers
  # grabs names of gold columns, removes "_gold" and returns question names
  full_file_gold_answers <- reactive({
    if ((is.null(input$files[1]) || is.na(input$files[1])) && input$job_id==0) {
      # User has not uploaded a file yet
      return(NULL)
    } else {
      full_file = subsetted_file()
      gold_cols = grepl(".\\gold$", names(full_file)) & !grepl(".\\golden",names(full_file))
      gold_cols_names = names(full_file)[gold_cols]    
      gold_cols_names
    }
    
  })
  
  #### Selector for left panel! #### 
  ##SIDEBAR PANEL###
  output$trustSelector <- renderUI({
    print(input$files)
    if ((is.null(input$files[1]) || is.na(input$files[1])) && input$job_id==0) {
      # User has not uploaded a file yet
      return(NULL)
    } else {
      workers = full_file()
      trust_levels = range(workers$X_trust)
      sliderInput(inputId = "trust_chosen",
                  label = "Overall Trust",
                  min = trust_levels[1] - .001, max = trust_levels[2] + .001,
                  value = c(trust_levels[1] - .001, trust_levels[2] + .001), step=.001)
    }
  })
  
  output$countrySelector <- renderUI({
    if ((is.null(input$files[1]) || is.na(input$files[1])) && input$job_id==0) {
      # User has not uploaded a file yet
      return(NULL)
    } else {
      workers = full_file()
      country_list = unique(workers$X_country)
      alpha_countries = sort(country_list)
      countries <- c("all_countries", alpha_countries)
      selectInput(inputId="country_chosen", label="Subset by Country:", countries)
    }
  })
  
  output$channelSelector <- renderUI({
    if ((is.null(input$files[1]) || is.na(input$files[1])) && input$job_id==0) {
      # User has not uploaded a file yet
      return(NULL)
    } else {
      
      workers = full_file()
      workers_list = unique(workers$X_channel)
      alpha_workers = sort(workers_list)
      channels <- c("all_channels", alpha_workers)
      selectInput(inputId="channel_chosen", label="Subset by Channel:", channels)
    }
  })
  
  output$timeSelector <- renderUI({
    if ((is.null(input$files[1]) || is.na(input$files[1])) && input$job_id==0) {
      # User has not uploaded a file yet
      return(NULL)
    } else {
      worker_times = full_file()
      #worker_times = add.times(worker_times)
      print(head(worker_times$last_submit))
      print(min(worker_times$last_submit))
      selectInput(inputId = "time_chosen", label="Latest Submissions",
                  c("Select One" = "none",
                    "Last 30 Minutes" = "thirty_mins",
                    "Last Hour" = "last_hr",
                    "Last 6 Hours" = "six_hrs",
                    "Last 12 Hours" = "twelve_hrs",
                    "Last Day" = "last_day",
                    "Last Week" = "last_week"))
    }
  })
  
  output$judgSelector <- renderUI({
    if ((is.null(input$files[1]) || is.na(input$files[1])) && input$job_id==0) {
      # User has not uploaded a file yet
      return(NULL)
    } else {
      
      workers = subsetted_file()
      workers_ip = agg_by_ip()
      judg_levels = c()
      print("IP min")
      print(min(workers_ip$num_judgments_ip))
      print("ID min")
      print(min(workers$num_judgments))      
      if (min(workers_ip$num_judgments_ip) < min(workers$num_judgments)){
        judg_levels[1] = min(workers_ip$num_judgments_ip)
      } else {
        judg_levels[1] = min(workers$num_judgments)
      }
      
      if(max(workers_ip$num_judgments_ip) > max(workers$num_judgments)){
        judg_levels[2] = max(workers_ip$num_judgments_ip)
      } else {
        judg_levels[2] = max(workers$num_judgments)
      }
      
      print("J levels")
      print(judg_levels)
      sliderInput(inputId = "num_judgs",
                  label = "Number of Judgments",
                  min = judg_levels[1] - 1, max = judg_levels[2] + 1, 
                  step = 10, value = c(judg_levels[1] - 1, judg_levels[2] + 1)) 
    }
  })
  
  
  output$scambotSelector <- renderUI({
    if ((is.null(input$files[1]) || is.na(input$files[1])) && input$job_id==0) {
      # User has not uploaded a file yet
      return(NULL)
    } else {
      full_file_with_times = full_file()
      print("In scambot selector about to print a column")
      print(head(full_file_with_times$time_duration_log))
      print("Printing min time")
      min_time = min(full_file_with_times$time_duration_log,na.rm=T) - 0.1
      print(min_time)
      print("Printing max time")
      max_time = max(full_file_with_times$time_duration_log,na.rm=T)
      print(max_time)
      sliderInput("threshold", "Scambot's log-duration threshold:", 
                  min = min_time, max = max_time, value = min_time, step= 0.1)
    }
  })
  
  #### END OF Selector for left panel! #### 
  
  ###Contributor ID Table  
  
  output$titleTextContributors <- renderUI({
    if ((is.null(input$files[1]) || is.na(input$files[1])) && input$job_id==0) {
      # User has not uploaded a file yet
      return(NULL)
    } else {
      workers_text = agg_by_worker()
      new_workers_text = live_worker_table()
      total_workers_trust = length(workers_text$trust)
      new_workers_trust = length(new_workers_text$trust)
      if (new_workers_trust > 50){
        max_count = min(50, nrow(new_workers_trust))
        new_workers_trust = 50
      }
      puts <- c("Displaying", new_workers_trust, "out of", total_workers_trust, "workers")
      h4(puts)
    }
  })
  
  
  ### Worker Graph 
  output$titleTextGraph <- renderUI({
    if ((is.null(input$files[1]) || is.na(input$files[1])) && input$job_id==0) {
      # User has not uploaded a file yet
      return(NULL)
    } else {
      workers_text = agg_by_worker()
      total_workers_trust = length(workers_text$trust)
      
      if (input$num_chosen == 'fiddy'){
        num_shown = min(total_workers_trust, 50)
        puts <- c("Graph showing",num_shown, "out of", total_workers_trust, "workers")
      }
      if (input$num_chosen == 'hund'){
        num_shown = min(total_workers_trust, 100)
        puts <- c("Graph showing",num_shown, "out of", total_workers_trust, "workers")
      }
      if (input$num_chosen == 'all' || total_workers_trust > 50){
        puts <- c("Graph showing", total_workers_trust, "total workers")
      }
      h4(puts)
    }
  })
  
  output$titleTextIp <- renderUI({
    if ((is.null(input$files[1]) || is.na(input$files[1])) && input$job_id==0) {
      # User has not uploaded a file yet
      return(NULL)
    } else {
      new_worker_by_ip = live_worker_table_ip()
      new_total_ips = length(new_worker_by_ip$trust)
      if (new_total_ips > 50){
        max_count = min(50, nrow(new_total_ips))
        new_total_ips = 50
      }
      worker_by_ip = agg_by_ip()
      total_ips = length(worker_by_ip$trust)
      puts <- c("Displaying", new_total_ips, "out of", total_ips, "IPs")
      h4(puts)
      
    }
  })
  
  ### Answer Distros
  
  output$questionSelector <- renderUI({
    if ((is.null(input$files[1]) || is.na(input$files[1])) && input$job_id==0) {
      # User has not uploaded a file yet
      return(NULL)
    } else {
      questions = full_file_gold_answers()
      questions = gsub(questions, pattern=".\\gold", replacement="")
      selectInput(inputId="question_chosen", label="Select question to display:", 
                  questions)
    }
  })
  
  output$titleDistrosUnits <- renderUI({
    if ((is.null(input$files[1]) || is.na(input$files[1])) && input$job_id==0) {
      # User has not uploaded a file yet
      return(NULL)
    } else {
      distro_units = full_file()
      total_units = length(unique(distro_units$X_unit_id))
      gold_units = length(unique(distro_units$X_unit_id[distro_units$X_golden == "true"]))
      non_golds_units = length(unique(distro_units$X_unit_id[distro_units$X_golden != "true"]))
      
      if (input$state_chosen == "golden"){
        text <- c("Number of Golds:", gold_units)
        h4(text)
      }
      else if (input$state_chosen == "normal"){
        text <- c("Number of Non Golds:", non_golds_units)
        h4(text)
      }
      else{
        text <- c("Number of Total Units:", total_units)
        h4(text)
      }
    }
  })
  
  output$titleDistrosJudgs <- renderUI({
    if ((is.null(input$files[1]) || is.na(input$files[1])) && input$job_id==0) {
      # User has not uploaded a file yet
      return(NULL)
    } else {
      distro_judgs = full_file()
      total_judgs = length(distro_judgs$X_unit_id)
      gold_judgs = length(distro_judgs$X_unit_id[distro_judgs$X_golden == "true"])
      non_golds_judgs = length(distro_judgs$X_unit_id[distro_judgs$X_golden != "true"])
      
      if (input$state_chosen == "golden"){
        text <- c("Number of Gold Judgments:", gold_judgs)
        h4(text)
      }
      else if (input$state_chosen == "normal"){
        text <- c("Number of Non Gold Judgments:", non_golds_judgs)
        h4(text)
      }
      else{
        text <- c("Number of Total Judgments:", total_judgs)
        h4(text)
      }
    }
  })
  
  ###Individual Contributor Profile Page
  ###Give total units worked on
  output$profileUnitCount <- renderUI({
    if ((is.null(input$files[1]) || is.na(input$files[1])) && input$job_id==0) {
      # User has not uploaded a file yet
      return(NULL)
    } else {
      full_by_worker = distros()
      
      if(input$id_chosen_profiles != ""){
        worker_profile = full_by_worker[full_by_worker$X_worker_id == input$id_chosen_profiles,]
        units_total = worker_profile$num_non_gold
        p(units_total)
      }
    }
    
  })
  
  ###Show which golds they worked on
  profile_golds <- reactive({
    if ((is.null(input$files[1]) || is.na(input$files[1])) && input$job_id==0) {
      # User has not uploaded a file yet
      return(NULL)
    } else {
      full_by_worker = distros()
      
      if(input$id_chosen_profiles != ""){
        worker_profile = full_file_contrib_id()
        golds_seen = unique(worker_profile$X_unit_id[worker_profile$X_golden == "true"])
        if(length(golds_seen ) > 100){
          golds_seen = golds_seen[1:100]
          return(golds_seen)
        } else{
          return(golds_seen)
        }
      }
    }
  })    
  
  ##Gold Distros
  output$goldAnswers <- renderUI({
    if ((is.null(input$files[1]) || is.na(input$files[1])) && input$job_id==0) {
      # User has not uploaded a file yet
      return(NULL)
    } else {
      gold_answers = full_file_gold_answers()
      p(gold_answers)
      
    }
  })
  
  ###Give total golds worked on
  output$profileGoldCount <- renderUI({
    if ((is.null(input$files[1]) || is.na(input$files[1])) && input$job_id==0) {
      # User has not uploaded a file yet
      return(NULL)
    } else {
      full_by_worker = distros()
      
      if(input$id_chosen_profiles != ""){
        print("collecting golds seen")
        worker_profile = full_by_worker[full_by_worker$X_worker_id == input$id_chosen_profiles,]
        golds_total = worker_profile$num_golds
        p(golds_total)
      }
    }
  })
  
  ###Show which units they worked on
  profile_units <- reactive({
    if ((is.null(input$files[1]) || is.na(input$files[1])) && input$job_id==0) {
      # User has not uploaded a file yet
      return(NULL)
    } else {
      if(input$id_chosen_profiles != ""){
        print("collecting units seen")
        worker_profile = full_file_contrib_id()
        units_seen = unique(worker_profile$X_unit_id[worker_profile$X_golden != "true"])
        if(length(units_seen) > 100){ 
          units_seen = units_seen[1:100]
          return(units_seen)
          
        } else{
          return(units_seen)
        }
        
        print(units_seen)
        print("L503")
      }
    }
  })
  
  ###Select Which Distributions To look at
  output$profileQuestionSelector <- renderUI({
    if ((is.null(input$files[1]) || is.na(input$files[1])) && input$job_id==0 || input$id_chosen_profiles=="") {
      # User has not uploaded a file yet
      return(NULL)
    } else {
      questions = full_file_gold_answers()
      questions = gsub(questions, pattern=".\\gold", replacement="")
      selectInput(inputId="profile_question_chosen", label="Select question to display:",
                  questions)
    }
  })
  ###Select Which Distributions to Look at for Golds 
  output$profileQuestionSelectorGolds <- renderUI({
    if ((is.null(input$files[1]) || is.na(input$files[1])) && input$job_id==0 || input$id_chosen_profiles =="") {
      # User has not uploaded a file yet
      return(NULL)
    } else {
      questions = full_file_gold_answers()
      questions = gsub(questions, pattern=".\\gold", replacement="")
      selectInput(inputId="profile_question_chosen_golds", label="Select question to display:",
                  questions)
    }
  })
  
  ###Create Output Tables 
  ##By Contributor ID
  output$create_html_table <- renderText({
    if ((is.null(input$files[1]) || is.na(input$files[1])) && input$job_id==0) {
      # User has not uploaded a file yet
      return(NULL)
    } else {
      job_id = job_id()
      worker_table= live_worker_table()
      if (length(worker_table$X_worker_id) > 50){
        max_count = min(50, nrow(worker_table))
        worker_table = worker_table[1:max_count,]
      }
      html_table = "<table border=1>"
      worker_table$last_submit = as.character(worker_table$last_submit)
      worker_table = rbind(names(worker_table),
                           worker_table)
      for (i in 1:nrow(worker_table)) {
        this_row = worker_table[i,]
        html_table = paste(html_table, '<tr>', sep="\n")
        if (i == 1) {
          for (value in this_row) {
            html_table = paste(html_table, '<td>', sep="\n")
            html_table = paste(html_table,
                               paste("<b>",value, "</b>"),
                               sep="\n") # pastes value!
            html_table = paste(html_table, '</td>', sep="\n")
          }
        } else {
          for (value_id in 1:length(this_row)) {
            value = this_row[value_id]
            html_table = paste(html_table, '<td>', sep="\n")
            if (value_id == 1) {
              value_link = paste("https://crowdflower.com/jobs/",
                                 job_id,
                                 "/contributors/",
                                 value,
                                 sep=""
              )
              value_to_paste= paste("<a href=\"",
                                    value_link,
                                    "\" target=\"_blank\">",
                                    value,
                                    "</a>")
              html_table = paste(html_table, value_to_paste, sep="\n") # pastes value!
            } else {
              html_table = paste(html_table, value, "&nbsp;&nbsp;", sep="\n") # pastes value!
            }
            html_table = paste(html_table, '</td>', sep="\n")
          }
        }
        html_table = paste(html_table, '</tr>', sep="\n")
      }
      html_table = paste(html_table,"</table>", sep="\n")
      paste(html_table)
    }
  })
  
  ##By worker IP
  output$create_html_table_ip <- renderText({
    if ((is.null(input$files[1]) || is.na(input$files[1])) && input$job_id==0) {
      # User has not uploaded a file yet
      return(NULL)
    } else {
      worker_table_ip= live_worker_table_ip()
      if (length(worker_table_ip$channel_ip) > 50){
        max_count = min(50, nrow(worker_table_ip))
        worker_table_ip = worker_table_ip[1:max_count,]
      }
      html_table = "<table border=1>"
      worker_table_ip$last_submit_ip = as.character(worker_table_ip$last_submit_ip)
      worker_table_ip = rbind(names(worker_table_ip),
                              worker_table_ip)
      for (i in 1:nrow(worker_table_ip)) {
        this_row = worker_table_ip[i,]
        html_table = paste(html_table, '<tr>', sep="\n")
        if (i == 1) {
          for (value in this_row) {
            html_table = paste(html_table, '<td>', sep="\n")
            html_table = paste(html_table,
                               paste("<b>",value, "</b>"),
                               sep="\n") # pastes value!
            html_table = paste(html_table, '</td>', sep="\n")
          }
        } else {
          for (value_id in 1:length(this_row)) {
            value = this_row[value_id]
            html_table = paste(html_table, '<td>', sep="\n")
            html_table = paste(html_table, value, "&nbsp;&nbsp;", sep="\n") # pastes value!
            html_table = paste(html_table, '</td>', sep="\n")
          }
        }
        html_table = paste(html_table, '</tr>', sep="\n")
      }
      html_table = paste(html_table,"</table>", sep="\n")
      paste(html_table)
    }
  })
  
  ###Create Single Profile Table of a contributor
  output$create_profile_table <- renderText({
    job_id = job_id()
    if ((is.null(input$files[1]) || is.na(input$files[1])) && input$job_id==0 || input$id_chosen_profiles == "") {
      # User has not uploaded a file yet
      return(NULL)
    } else {
      worker_table= agg_by_worker()
      profile_id = input$id_chosen_profiles
      worker_table = worker_table[worker_table$X_worker_id == profile_id,]
      html_table = "<table border=1>"
      worker_table$last_submit = as.character(worker_table$last_submit)
      worker_table = rbind(names(worker_table),
                           worker_table)
      for (i in 1:nrow(worker_table)) {
        this_row = worker_table[i,]
        html_table = paste(html_table, '<tr>', sep="\n")
        if (i == 1) {
          for (value in this_row) {
            html_table = paste(html_table, '<td>', sep="\n")
            html_table = paste(html_table,
                               paste("<b>",value, "</b>"),
                               sep="\n") # pastes value!
            html_table = paste(html_table, '</td>', sep="\n")
          }
        } else {
          for (value_id in 1:length(this_row)) {
            value = this_row[value_id]
            html_table = paste(html_table, '<td>', sep="\n")
            if (value_id == 1) {
              value_link = paste("https://crowdflower.com/jobs/",
                                 job_id,
                                 "/contributors/",
                                 value,
                                 sep=""
              )
              value_to_paste= paste("<a href=\"",
                                    value_link,
                                    "\" target=\"_blank\">",
                                    value,
                                    "</a>")
              html_table = paste(html_table, value_to_paste, sep="\n") # pastes value!
            } else {
              html_table = paste(html_table, value, "&nbsp;&nbsp;", sep="\n") # pastes value!
            }
            html_table = paste(html_table, '</td>', sep="\n")
          }
        }
        html_table = paste(html_table, '</tr>', sep="\n")
      }
      html_table = paste(html_table,"</table>", sep="\n")
      paste(html_table)
    }
  })
  
  ##Create Clickable links to units on the profile page, under units seen.
  output$create_unit_links <- renderText({
    job_id = job_id()
    if ((is.null(input$files[1]) || is.na(input$files[1])) && input$job_id==0 || input$id_chosen_profiles=="") {
      # User has not uploaded a file yet
      return(NULL)
    } else {
      units = profile_units()
      print("L701")
      html_table = "<table border=1>"
      html_table = paste(html_table, '<tr>', sep='')
      print(length(units))
      for (i in 1:length(units)){
        value = units[i]
        value_link = paste("https://crowdflower.com/jobs/",
                           job_id,
                           "/units/",
                           value, sep="")
        value_to_paste= paste("<a href=\"",
                              value_link,
                              "\" target=\"_blank\">",
                              value,
                              "</a>")
        html_table = paste(html_table, value_to_paste, sep="|")
      }       
      
      html_table= paste(html_table, '</tr>', sep="\n")
      html_table= paste(html_table, "</table>", sep="\n")
      paste(html_table)
    }
  })
  
  ###Create Clickable links for golds on profile page. 
  output$create_gold_links <- renderText({
    job_id = job_id()
    if ((is.null(input$files[1]) || is.na(input$files[1])) && input$job_id==0 || input$id_chosen_profiles=="") {
      # User has not uploaded a file yet
      return(NULL)
    } else {
      units = profile_golds()
      #print("L695")
      #print("L697")
      html_table = "<table border=1>"
      html_table = paste(html_table, '<tr>', sep='')
      print(length(units))
      for (i in 1:length(units)){
        value = units[i]
        value_link = paste("https://crowdflower.com/jobs/",
                           job_id,
                           "/units/",
                           value, sep="")
        value_to_paste= paste("<a href=\"",
                              value_link,
                              "\" target=\"_blank\">",
                              value,
                              "</a>")
        html_table = paste(html_table, value_to_paste, sep="|")
      }       
      
      html_table= paste(html_table, '</tr>', sep="\n")
      html_table= paste(html_table, "</table>", sep="\n")
      paste(html_table)
    }
  })
  
  profile_similar_workers <- reactive({
    if (is.na(input$files[1])) {
      # User has not uploaded a file yet
      return(NULL)
    } else {
      profile_id = input$id_chosen_profiles
      workers = agg_by_worker()
      profile = workers[workers$X_worker_id == profile_id,]
      workers = workers[workers$X_worker_id != profile_id,]
      print(head(profile))
      print(head(workers))
      match = {}
      
      for(i in 1:length(workers$X_worker_id)){
        
        match[i] = "false" 
        #print("How are we doing")
        
        if(workers$country[i] == profile$country){
          if(workers$channel[i] == profile$channel){
            match[i] = "true"
            #print("Made it to 589")
          }
          if(workers$trust[i] == profile$trust){
            #  match[i] = "true"
            print("Made it to 593")
          }
        } 
        
        if(workers$num_judgments[i] == profile$num_judgments){
          if(workers$trust[i] == profile$trust){
            match[i] = "true"
            #print("Made it to 600")
          } 
        }
      }
      
      similar_workers = cbind(workers, match)
      
      similar_workers = 
        similar_workers[similar_workers$match == "true",]
      similar_workers
      
    }
  })
  
  
  
  #### Contributor Table Reacts to Functions Inputs is loaded in create_html_table #### 
  live_worker_table <- reactive({
    if ((is.null(input$files[1]) || is.na(input$files[1])) && input$job_id==0) {
      # User has not uploaded a file yet
      return(NULL)
    } else {    
      
      full_by_worker = agg_by_worker()
      
      if(input$id_chosen != ""){
        print("Searching by ID")
        full_by_worker = full_by_worker[full_by_worker$X_worker_id == input$id_chosen,]        
      }
      
      if (input$sortby_chosen != "none"){
        if (input$sortby_chosen == "sortby_trust"){
          print("Sorting By Trust")
          if (input$ascending != "ascending"){
            full_by_worker = full_by_worker[order(full_by_worker$trust, decreasing = T),]
            print("trust ascending")
          }
          if (input$ascending == "ascending") {
            full_by_worker = full_by_worker[order(full_by_worker$trust, decreasing = F),]
            print("trust descending")
          }
        }
        if (input$sortby_chosen == "sortby_judgments"){
          print("Sorting By Judgments")
          if (input$ascending != "ascending"){
            full_by_worker = full_by_worker[order(full_by_worker$num_judgments, decreasing = T),]
            print("judgs ascending")
          }
          if (input$ascending == "ascending") {
            full_by_worker = full_by_worker[order(full_by_worker$num_judgments, decreasing = F),]
            print("judgs descending")
          }
        }
        if (input$sortby_chosen == "sortby_submit"){
          print("Sorting By Submission")
          if (input$ascending != "ascending"){
            full_by_worker = full_by_worker[order(full_by_worker$last_submit, decreasing = T),]
            print("times ascending")
          }
          if (input$ascending == "ascending") {
            full_by_worker = full_by_worker[order(full_by_worker$last_submit, decreasing = F),]
            print("times descending")
          }
        }
        if (input$sortby_chosen == "sortby_ips"){
          print("Sorting By Ips")
          if (input$ascending != "ascending"){
            full_by_worker = full_by_worker[order(full_by_worker$num_ips, decreasing = T),]
            print("ip descending")
          }
          if (input$ascending == "ascending") {
            full_by_worker = full_by_worker[order(full_by_worker$num_ips, decreasing = F),]
            print("ip ascending")
          }
        }
      }
      full_by_worker
    }
    
  })
  
  ### IP Table Reacts to Same Inputs As Contributor Table loaded into create_html_table_ip 
  live_worker_table_ip <- reactive({
    if ((is.null(input$files[1]) || is.na(input$files[1])) && input$job_id==0) {
      # User has not uploaded a file yet
      return(NULL)
    } else { 
      worker_ips = agg_by_ip()
      
      if (input$country_chosen != "all_countries") {
        print("Subsetting by country")
        worker_ips = worker_ips[worker_ips$country_ip == input$country_chosen,]
      }
      if (input$channel_chosen != "all_channels") {
        print("Subsetting by country")
        worker_ips = worker_ips[worker_ips$channel_ip == input$channel_chosen,]
      }
      
      if(input$time_chosen != "none"){
        current_time = as.POSIXlt(Sys.time(), "GMT")
        print(current_time)
        if(input$time_chosen == "thirty_mins"){
          #subtracting 30 mins from current time
          time = current_time - 1800
          worker_ips = worker_ips[worker_ips$last_submit_ip >= time, ]
        }
        else if (input$time_chosen == "last_hr"){
          time = current_time - 3600
          worker_ips = worker_ips[worker_ips$last_submit_ip >= time, ]
        }
        else if (input$time_chosen == "six_hrs"){
          time = current_time - 21600
          worker_ips = worker_ips[worker_ips$last_submit_ip >= time, ]
        }
        else if (input$time_chosen == "twelve_hrs"){
          time = current_time - 43200
          worker_ips = worker_ips[worker_ips$last_submit_ip >= time, ]
        }
        
        else if (input$time_chosen == "last_day"){
          time = current_time - 86400
          worker_ips = worker_ips[worker_ips$last_submit_ip >= time, ]
        }
        else if (input$time_chosen == "last_week"){
          time = current_time - 604800
          worker_ips = worker_ips[worker_ips$last_submit_ip >= time, ]
        }
      }
      
      if (!is.null(input$num_judgs)) {
        print("Subsetting by judgments")
        worker_ips = worker_ips[worker_ips$num_judgments_ip <= max(input$num_judgs) &
                                  worker_ips$num_judgments_ip >= min(input$num_judgs),]   
      }
      
      if (!is.null(input$trust_chosen)) {
        print("Subsetting by trust")
        worker_ips = worker_ips[worker_ips$trust_ip <= max(input$trust_chosen) &
                                  worker_ips$trust >= min(input$trust_chosen),]
      }
      
      if (input$sortby_chosen_ip != "none_ip"){
        if (input$sortby_chosen_ip == "sortby_trust_ip"){
          print("Sorting By Trust")
          if (input$ascending_ip != "ascending_ip"){
            worker_ips = worker_ips[order(worker_ips$trust_ip, decreasing = T),]
            print("trust ascending")
          }
          if (input$ascending_ip == "ascending_ip") {
            worker_ips = worker_ips[order(worker_ips$trust_ip, decreasing = F),]
            print("trust descending")
          }
        }
        if (input$sortby_chosen_ip == "sortby_judgments_ip"){
          print("Sorting By Judgments")
          if (input$ascending != "ascending_ip"){
            worker_ips = worker_ips[order(worker_ips$num_judgments_ip, decreasing = T),]
            print("judgs ascending")
          }
          if (input$ascending_ip == "ascending_ip") {
            worker_ips = worker_ips[order(worker_ips$num_judgments_ip, decreasing = F),]
            print("judgs descending")
          }
        }
        if (input$sortby_chosen_ip == "sortby_submit_ip"){
          print("Sorting By Submission")
          if (input$ascending_ip != "ascending_ip"){
            worker_ips = worker_ips[order(worker_ips$last_submit_ip, decreasing = T),]
            print("times ascending")
          }
          if (input$ascending_ip == "ascending_ip") {
            worker_ips = worker_ips[order(worker_ips$last_submit_ip, decreasing = F),]
            print("times descending")
          }
        }
        if (input$sortby_chosen_ip == "sortby_ids"){
          print("Sorting By Ips")
          if (input$ascending_ip != "ascending_ip"){
            worker_ips = worker_ips[order(worker_ips$num_work_ids, decreasing = T),]
            print("id descending")
          }
          if (input$ascending == "ascending_ip") {
            worker_ips = worker_ips[order(worker_ips$num_work_ids, decreasing = F),]
            print("id ascending")
          }
        }
        
        
      }
      if(input$ip_chosen != ""){
        print("Searching by ID")
        worker_ips = worker_ips[worker_ips$X_ip == input$ip_chosen,]        
      }  
      
      worker_ips
    }
  })
  
  ##Graphs and Plots  
  ##General Plot of contributor table, sortable.
  output$create_similar_table <- renderText({
    job_id = job_id()
    if (is.na(input$files[1])) {
      # User has not uploaded a file yet
      return(NULL)
    } else {
      worker_table= profile_similar_workers()
      if (length(worker_table$X_worker_id) > 50){
        max_count = min(50, nrow(worker_table))
        worker_table = worker_table[1:max_count,]
      }
      html_table = "<table border=1>"
      worker_table$last_submit = as.character(worker_table$last_submit)
      worker_table = rbind(names(worker_table),
                           worker_table)
      for (i in 1:nrow(worker_table)) {
        this_row = worker_table[i,]
        html_table = paste(html_table, '<tr>', sep="\n")
        if (i == 1) {
          for (value in this_row) {
            html_table = paste(html_table, '<td>', sep="\n")
            html_table = paste(html_table,
                               paste("<b>",value, "</b>"),
                               sep="\n") # pastes value!
            html_table = paste(html_table, '</td>', sep="\n")
          }
        } else {
          for (value_id in 1:length(this_row)) {
            value = this_row[value_id]
            html_table = paste(html_table, '<td>', sep="\n")
            if (value_id == 1) {
              value_link = paste("https://crowdflower.com/jobs/",
                                 job_id,
                                 "/contributors/",
                                 value,
                                 sep=""
              )
              value_to_paste= paste("<a href=\"",
                                    value_link,
                                    "\" target=\"_blank\">",
                                    value,
                                    "</a>")
              html_table = paste(html_table, value_to_paste, sep="\n") # pastes value!
            } else {
              html_table = paste(html_table, value, "&nbsp;&nbsp;", sep="\n") # pastes value!
            }
            html_table = paste(html_table, '</td>', sep="\n")
          }
        }
        html_table = paste(html_table, '</tr>', sep="\n")
      }
      html_table = paste(html_table,"</table>", sep="\n")
      paste(html_table)
    }
  })
  
  get_worker_table <- reactive({
    if ((is.null(input$files[1]) || is.na(input$files[1])) && input$job_id==0) {
      # User has not uploaded a file yet
      return(NULL)
    } else { 
      workers = agg_by_worker()
      workers = workers[order(workers$num_judgments, decreasing = T),]
      
      if (input$num_chosen == "fiddy"){
        cutoff = min(50,nrow(workers))
        workers = workers[1:cutoff,]
      } 
      
      if (input$num_chosen == "hund"){
        cutoff = min(100,nrow(workers))
        workers = workers[1:cutoff,]
      }
    }
    workers
    
  })
  
  output$plot_workers <- renderChart({
    print("Calling the Plot Workers Chart")
    if ((is.null(input$files[1]) || is.na(input$files[1])) && input$job_id==0) {
      # User has not uploaded a file yet
      return(NULL)
    } else { 
      print('Starting the Plot Workers Chart')
      workers = get_worker_table()
      print(paste("Input chosen",input$group_chosen))
      if(input$group_chosen != "trust"){
        print(names(workers))
        p6 <- nPlot(num_judgments ~ X_worker_id, group=input$group_chosen, data = workers, type='multiBarChart', 
                    dom='plot_workers', width=800)
        p6$xAxis(rotateLabels=45)
        p6$chart(reduceXTicks = FALSE)
        p6
      } else{
        print("Going into fiver in Plot Workers Chart")
        this_fiver = fivenum(workers$trust, na.rm=T)
        if (sum(duplicated(this_fiver))>0) {
          this_fiver = unique(this_fiver)
        }
        print("Printing fiver from which_bucket")
        print(this_fiver)
        print("Head of trust scores for which_bucket")
        print(head(workers$trust))
        workers$trust_buckets = sapply(workers$trust,
                                       function(k) which_bucket(y=k,
                                                                fiver=this_fiver))
        
        p6 <- nPlot(num_judgments ~ X_worker_id, group='trust_buckets', 
                    data = workers, type='multiBarChart', dom='plot_workers', width=800)
        
        p6$xAxis(axisLabel='Worker Ids')
        p6$xAxis(rotateLabels=45)
        p6$chart(reduceXTicks = FALSE)
        p6
      }       
    }    
  })
  
  output$plot_distros <- renderChart({
    if ((is.null(input$files[1]) || is.na(input$files[1])) && input$job_id==0) {
      # User has not uploaded a file yet
      return(NULL)
    } else {
      full_file = subsetted_file()
      answer_cols = grepl(pattern=".\\gold$", names(full_file)) &
        !grepl(pattern=".\\golden",names(full_file))
      answer_cols_names = names(full_file)[answer_cols]
      answer_cols_names = gsub(answer_cols_names, pattern=".\\gold", replacement="")
      # return(answer_cols_names)
      
      chosen_q = input$question_chosen
      question_index = which(answer_cols_names == chosen_q)
      
      chosen_state = input$state_chosen
      
      if ("X_golden" %in% names(full_file)) {
        if (chosen_state == "golden") {
          full_file = full_file[full_file$X_golden == 'true',]
        } else if (chosen_state == "normal") {
          full_file = full_file[full_file$X_golden != 'true',]
        }
      }
      
      responses = lapply(answer_cols_names, function(x) {
        responses = table(full_file[,names(full_file)==x])
        responses/sum(responses)
      }
      )
      
      responses_table = responses[[question_index]]
      
      responses_table_transformed = data.frame(questions = names(responses_table),
                                               numbers = as.numeric(responses_table),
                                               group = chosen_q)
      
      responses_table_transformed = responses_table_transformed[order(-responses_table_transformed$numbers),]
      
      if (nrow(responses_table_transformed) > 9 ) {
        responses_table_transformed1 = responses_table_transformed[1:9,]
        responses_table_transformed1[10,] =c("Other Values", sum(responses_table_transformed$numbers[10:length(responses_table_transformed)]), chosen_q)
        responses_table_transformed = responses_table_transformed1
      }
      
      responses_table_transformed$questions[responses_table_transformed$questions==""] = "\"\""
      
      
      p3 <- nPlot(numbers ~ questions, data=responses_table_transformed,
                  group = 'group', type='multiBarChart', dom='plot_distros', width=800, margin=60, overflow="visible") 
      
      p3$xAxis(rotateLabels=45)
      p3$xAxis(axisLabel='Worker Responses')
      p3$chart(reduceXTicks = FALSE)
      p3
    }
    
  })
  
  ###Individual Contributor Unit Distributions
  output$profile_units_distros <- renderChart({
    #full_file_unit_answers <- reactive(function(){
    if ((is.null(input$files[1]) || is.na(input$files[1])) && input$job_id==0) {
      # User has not uploaded a file yet
      #return(NULL)
      
      return(NULL)
    } else {
      
      if(input$id_chosen_profiles == "") {
        return(NULL)
      } else {
        full_file = full_file_contrib_id()
        answer_cols = grepl(pattern=".\\gold$", names(full_file)) &
          !grepl(pattern=".\\golden",names(full_file))
        answer_cols_names = names(full_file)[answer_cols]
        answer_cols_names = gsub(answer_cols_names, pattern=".\\gold", replacement="")
        
        profile_chosen_q = input$profile_question_chosen
        question_index = which(answer_cols_names == profile_chosen_q)
        
        
        if ("X_golden" %in% names(full_file)) {
          full_file = full_file[full_file$X_golden != 'true',]
        }
        
        responses = lapply(answer_cols_names, function(x) {
          responses = table(full_file[,names(full_file)==x])
          responses/sum(responses)
        }
        )
        print("Question chosen")
        print(profile_chosen_q)
        print("Index of question")
        print(question_index)
        print("df to subset")
        print(responses)
        responses_table = responses[[question_index]]
        responses_table = responses_table[order(responses_table)]
        
        responses_table_transformed = data.frame(questions = names(responses_table),
                                                 numbers = as.numeric(responses_table),
                                                 group = profile_chosen_q)
        
        responses_table_transformed = responses_table_transformed[1:3]
        
        p2 <- nPlot(numbers ~ questions, data=responses_table_transformed, group = 'group',
                    type='multiBarChart', dom='profile_units_distros') 
        x_label = paste('Answers from', input$id_chosen_profiles, sep=" ")
        p2$xAxis(rotateLabels=45)
        p2$xAxis(axisLabel=x_label)
        p2$chart(reduceXTicks = FALSE)
        p2
        
      }
    }
  })
  
  
  ###Individual Contributor Gold Distributions
  output$profile_golds_distros <- renderChart({
    #full_file_unit_answers <- reactive(function(){
    if ((is.null(input$files[1]) || is.na(input$files[1])) && input$job_id==0) {
      # User has not uploaded a file yet
      return(NULL)
    } else {
      full_file = full_file_contrib_id()
      answer_cols = grepl(pattern=".\\gold$", names(full_file)) &
        !grepl(pattern=".\\golden",names(full_file))
      answer_cols_names = names(full_file)[answer_cols]
      answer_cols_names = gsub(answer_cols_names, pattern=".\\gold", replacement="")
      
      profile_chosen_q = input$profile_question_chosen_golds
      question_index = which(answer_cols_names == profile_chosen_q)
      
      
      if ("X_golden" %in% names(full_file)) {
        full_file = full_file[full_file$X_golden == 'true',]
      }
      
      responses = lapply(answer_cols_names, function(x) {
        responses = table(full_file[,names(full_file)==x])
        responses/sum(responses)
      }
      )
      
      responses_table = responses[[question_index]]
      responses_table = responses_table[order(responses_table)]
      
      responses_table_transformed = data.frame(questions = names(responses_table),
                                               numbers = as.numeric(responses_table),
                                               group = profile_chosen_q)
      
      p4 <- nPlot(numbers ~ questions, data=responses_table_transformed, group = 'group',
                  type='multiBarChart', dom='profile_golds_distros') 
      
      x_label = paste('Answers on Golds from', input$id_chosen_profiles, sep=" ")
      
      p4$xAxis(rotateLabels=45)
      
      p4$xAxis(axisLabel= x_label)
      p4$chart(reduceXTicks = FALSE)
      p4
    }
    
  }) 
  
  ############### this part controls displaying the stats ###############
  output$summary_message <- renderText({
    if ((is.null(input$files[1]) || is.na(input$files[1])) && input$job_id==0) {
      # User has not uploaded a file yet
      return("<p>You have not uploaded any files yet</p>")
    } else {
      full_file = full_file()
      num_contributors = length(unique(full_file$X_worker_id))
      num_gold_units = length(unique(full_file$X_unit_id[full_file$X_golden=='true']))
      num_nongold_units = length(unique(full_file$X_unit_id)) - num_gold_units
      num_trusted_judgments = sum(full_file$X_tainted != 'true')
      num_untrusted_judgments = sum(full_file$X_tainted == 'true')
      if (num_untrusted_judgments == 0) {
        tainted_message = "<p style='color:red;'>It looks like you don't have any <b>tainted judgments</b> in your file. Change reporting settings and regenerate the report if this is not intentional.</p>"
      } else {
        tainted_message = ""
      }
      if (num_gold_units == 0) {
        gold_message = "<p style='color:red;'>It looks like you don't have any <b>Gold units</b> in yout file. Change reporting settings and regenerate the report if this is not intentional.</p>"
      } else {
        gold_message = ""
      }
      overall_message = paste("<p>The report you uploaded has:<br>",
                              num_contributors, " contributors,<br>",
                              num_gold_units, " gold units,<br>",
                              num_nongold_units, " ordinary units,<br>",
                              num_trusted_judgments, " trusted judgments,<br>",
                              num_untrusted_judgments, " untrusted judgments.<br>",
                              "</p>", sep="")
      paste(overall_message, tainted_message, gold_message)
    } 
  })
  
  ###SCAMBOT AND BURNINATOR
  ################################ here comes scambot ##########################################
  
  premade_scambot_plot <- reactive({
    if ((is.null(input$files[1]) || is.na(input$files[1])) && input$job_id==0) {
      # User has not uploaded a file yet
      return(NULL)
    } else {
      df = full_file()
      qnt=quantile(df$time_duration_log,probs=c(0.5,0.10,0.05))
      p = ggplot(df, aes_string(x="time_start", y="time_duration_log", color="X_trust")) +
        geom_point(color="gainsboro") +
        scale_size_manual(values=c(2,10)) +
        theme(legend.position="bottom")+
        guides(col = guide_legend(ncol = 20))+
        geom_hline(yintercept=qnt[1],linetype='solid',alpha=0.5)+
        geom_hline(yintercept=qnt[2],linetype='dashed',alpha=0.5)+
        geom_hline(yintercept=qnt[3],linetype='dotted')+
        annotate("text", x=0, qnt[1], label="Mean")+
        annotate("text", x=0, qnt[2], label="10%")+
        annotate("text", x=0, qnt[3], label="5%")
      p
    }
  })
  
  offenders_table <- reactive({
    if ((is.null(input$files[1]) || is.na(input$files[1])) && input$job_id==0) {
      # User has not uploaded a file yet
      return(NULL)
    } else {
      df=subsetted_file()
      print("not broken")
      threshold = input$threshold
      print("Defined threshold, about to subset in Burminator")
      df_under=df[which(df$time_duration_log<threshold),c("X_worker_id","X_ip", "time_duration")]
      if (nrow(df_under)==0) {
        return(df_under)
      }
      df = df[df$X_worker_id %in% df_under$X_worker_id,]
      df$is_under_line = df$time_duration_log < threshold
      df
    }
  })
  
  output$plot <- renderPlot({
    if ((is.null(input$files[1]) || is.na(input$files[1])) && input$job_id==0) {
      # User has not uploaded a file yet
      return(NULL)
    } else {
      new_df = subsetted_file()
      p = premade_scambot_plot()
      # add new data
      if (nrow(new_df) >0) {
        new_df$X_trust <- cut(new_df$X_trust,6)
        p = p + geom_point(data=new_df,aes_string(color="X_trust")) +
          scale_colour_brewer(palette="PiYG")
      }
      # add the red line
      if (!is.null(input$threshold) || !is.na(input$threshold)) {
        p = p + geom_hline(yintercept=input$threshold,color="darkorange")
      }
      # add offenders
      offenders = offenders_table()
      if (nrow(offenders) > 0) {
        p = p + geom_point(data = offenders, colour = "darkorange")
      }  
      print(p)
    }
  },height=1000)
  
  output$offenders <- renderText({
    df=offenders_table()
    if (nrow(df) > 0) {
      print("probs about to break")
      thou_dost_offend_me = ddply(df, .(X_worker_id), summarize,
                                  ip = X_ip[1],
                                  location = paste(X_city[1], X_country[1], sep=", "),
                                  channel = X_channel[1],
                                  min_time = min(time_duration),
                                  max_time = max(time_duration),
                                  num_judgments = length(time_duration),
                                  num_offenses = sum(is_under_line==TRUE))
      thou_dost_offend_me$min_time = round(thou_dost_offend_me$min_time,2)
      thou_dost_offend_me$max_time = round(thou_dost_offend_me$max_time,2)
      
      job_id = job_id()
      html_offenders = "<table border=1>"
      #worker_table$last_submit = as.character(worker_table$last_submit)
      thou_dost_offend_me = rbind(names(thou_dost_offend_me),
                                  thou_dost_offend_me)
      for (i in 1:nrow(thou_dost_offend_me)) {
        this_row = thou_dost_offend_me[i,]
        html_offenders = paste(html_offenders, '<tr>', sep="\n")
        if (i == 1) {
          for (value in this_row) {
            html_offenders = paste(html_offenders, '<td>', sep="\n")
            html_offenders = paste(html_offenders,
                                   paste("<b>",value, "</b>"),
                                   sep="\n") # pastes value!
            html_offenders = paste(html_offenders, '</td>', sep="\n")
          }
          html_offenders = paste(html_offenders, '<td>', sep="\n")
          html_offenders = paste(html_offenders, 'Reject!', sep="\n")
          html_offenders = paste(html_offenders, '</td>', sep="\n")
        } else {
          for (value_id in 1:length(this_row)) {
            value = this_row[value_id]
            html_offenders = paste(html_offenders, '<td>', sep="\n")
            if (value_id == 1) {
              value_link = paste("https://crowdflower.com/jobs/",
                                 job_id,
                                 "/contributors/",
                                 value,
                                 sep=""
              )
              value_to_paste= paste("<a href=\"",
                                    value_link,
                                    "\" target=\"_blank\">",
                                    value,
                                    "</a>")
              html_offenders = paste(html_offenders, value_to_paste, sep="\n") # pastes value!
            } else {
              html_offenders = paste(html_offenders, value, "&nbsp;&nbsp;", sep="\n") # pastes value!
            }
            html_offenders = paste(html_offenders, '</td>', sep="\n")
          }
          html_offenders = paste(html_offenders, '<td>', sep="\n")
          html_offenders = paste(html_offenders, '<button class="btn btn-danger action-button shiny-bound-input" data-toggle="button" id="get', this_row[1], '" type="button">Reject</button>' , sep="")
          html_offenders = paste(html_offenders, '</td>', sep="\n")
        }
        
        html_offenders = paste(html_offenders, '</tr>', sep="\n")
      }
      html_offenders = paste(html_offenders,"</table>", sep="\n")
      paste(html_offenders)
    } else {
      paste("<b>Looks like nobody fell under the red line!</b>")
    }
  })
  
  
  flag_individual_workers <- reactive({
    if ((is.null(input$files[1]) || is.na(input$files[1])) && input$job_id==0) {
      # User has not uploaded a file yet
      return(NULL)
    } else {
      df=offenders_table()
      if (nrow(df) == 0) {
        return(NULL)
      } else {
        potential_offenders = unique(df$X_worker_id)
        print(potential_offenders)
        potential_gets = paste("get", potential_offenders, sep="")
        print(potential_gets)
        ids_to_reject = c()
        for (i in 1:length(potential_gets)) {
          if(input[[potential_gets[i]]] == 0) {
            print(paste("this get", potential_gets[i], "has not been clicked yet"))
            # int he future: remove from the flagged list
          } else {
            print(paste("this get", potential_gets[i], "got CLICKED. Yayyyyyy!"))
            ids_to_reject = c(ids_to_reject, potential_offenders[i])
          }
        }
        return(ids_to_reject)
      }
    }
  })
  
  output$flag_some_workers <- renderText({
    ids_to_reject = flag_individual_workers()
    lapply(ids_to_reject, function(x) reject_user(job_id = job_id(), x=x))
    paste("")
  })
  
  output$downloadData <- downloadHandler(
    filename = function() { paste('offenders.csv', sep='') },
    content = function(file) {
      df=offenders_table()
      if (nrow(df) == 0) {
        return(NULL)
      } else {
        ids_to_reject = unique(df$X_worker_id)
        lapply(ids_to_reject, function(x) reject_user(job_id = job_id(), x=x))
      }
      write.csv(df, paste(file,sep=''), row.names=F, na="")
    }
  )  
  
  
})