reject_user <- function(job_id, x) {
  flag_head=paste("curl -X PUT \'https://crowdflower.com/jobs/", job_id,"/contributors/",sep='')
  
  flag_tail="/reject?key=b0ddc5a1840a88c5c57be01fef888970ce9bbe08&reason=scambot_speed_violation_punch_these_guys_in_the_face\' -d \'\'"
  system(paste(flag_head,x,flag_tail,sep=''))
}
