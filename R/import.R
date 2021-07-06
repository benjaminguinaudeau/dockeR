#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL

#' sudo
#' @description This function executes the given command as sudo
#' @param command A bash command to execute as sudo
#' @param env_var Should the password be saved for further use
#' @export

sudo <- function(command,
                 intern = F,
                 ignore.stdout = F,
                 ignore.stderr = T,
                 env_var = T,
                 cmd = F){

  if(os() == "Windows"){
    if(cmd){
      out <- command
    } else {
      out <- exec(glue::glue("{ command }"),
                  intern = intern,
                  ignore.stdout = ignore.stdout,
                  ignore.stderr = ignore.stderr)
    }
  } else {
    if(class(try(keyring::key_get("SUDO_PASS"), silent = T))[1] == "try-error"){
      keyring::key_set("SUDO_PASS")
    }

    if(cmd){
      out <- glue::glue("echo { keyring::key_get('SUDO_PASS') } | sudo -S { command }")
    } else {

      out <- exec(glue::glue("echo { keyring::key_get('SUDO_PASS') } | sudo -S { command }"),
                  intern = intern,
                  ignore.stdout = ignore.stdout,
                  ignore.stderr = ignore.stderr)
    }

    if(!env_var) keyring::key_delete("SUDO_PASS")
  }



  return(out)
}

#' os
#' @export

os <- function() Sys.info()['sysname']

#' exec
#' @export
exec <- function(string, cmd = F, ...){

  if(cmd){
    return(string)}
  else{
    if(os() == "Windows"){
      return(shell(string, ...))
    }else {
      return(suppressWarnings(system(string, ...))) # suppress Warnings to avoid the print of the sudo password
    }
  }
}

#' simule_map
#' @export
simule_map <- function(.list, index = 1, env = .GlobalEnv){
  env$.x <- .list[[index]]
  env$.y <- names(.list)[index]
  return(.x)
}


