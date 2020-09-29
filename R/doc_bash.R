#' doc_exec
#' @export
doc_exec <- function(container_name,
                     command,
                     intern = F,
                     ignore.stdout = F,
                     ignore.stderr = T,
                     cmd = F,
                     ...){
  container_name <- check_container_name(container_name)
  out <- system(glue::glue("docker exec -t {container_name} {command}"),
              intern = intern,
              ignore.stdout = ignore.stdout,
              ignore.stderr = ignore.stderr,
              cmd = cmd,
              ...)


  if(!intern){return(invisible(T))}
  if(intern){return(out)}
}

#' doc_remove_folder
#' @export
doc_remove_folder <- function(container, folder, ...){
  container <- check_container_name(container)
  doc_exec(container, glue::glue("rm -Rf {folder}"), ...)
}

#' doc_list_file
#' @export
doc_list_file <- function(container, folder){
  container <- check_container_name(container)
  silently(
    try(
      doc_exec(container, glue::glue("ls {folder}"),
               intern = T) %>%
        stringr::str_trim()
    )
  )
}
