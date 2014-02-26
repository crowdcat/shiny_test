prepare_html_table <- function(worker_table, job_id){
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
  return(html_table)
}