flag_user <- function(job_id, x) {
  flag_head=paste("curl -X PUT \'https://crowdflower.com/jobs/", job_id,"/contributors/",sep='')
  
  flag_tail=
  "/flag?key=b0ddc5a1840a88c5c57be01fef888970ce9bbe08&reason=flagged_from_coolstuff_caught_in_scambot_speed_trap\' -d \'\'"
  system(paste(flag_head,x,flag_tail,sep=''))
}