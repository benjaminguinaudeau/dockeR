#' list_container
#' @description This function lists all running containers. If image_src is specified, it only return containers, of which the building image matches the given string.
#' @param image_src A String providing a specific image to filter
#' @param not_running a logical vector specifying whether not_running containers should be returned
#' @return a tibble listing containers with their information
#' @export

list_container <- function(image_src = NULL, not_running = T){

  if(not_running){
    raw_list <- system("docker ps -a --no-trunc", intern = T)
  } else {
    raw_list <- system("docker ps --no-trunc", intern = T)
  }

  col_names <- raw_list[1] %>% str_split("\\s{2,}") %>% unlist

  structured_list <- raw_list %>%
    tail(-1) %>%
    str_split("\\s{2,}")

  if(length(structured_list) == 0){
    return(
      tibble("container id" = NA_character_,
             "image" = NA_character_,
             "command" = NA_character_,
             "created" = NA_character_,
             "status" = NA_character_,
             "ports" = NA_character_,
             "names" = NA_character_)
    )
  }

  containers <- structured_list %>%
    map(~{
      if(str_detect(.x[5], "Exited")){
        .x[7] <- .x[6]
        .x[6] <- NA
      }
      .x
    }) %>%
    reduce(cbind) %>%
    t %>%
    as_tibble
  names(containers) <- col_names %>% tolower

  if(!is.null(image_src)){
    containers <- containers %>%
      filter(image %>% str_detect(image_src))

    if(nrow(containers) == 0){
      message(glue("No container build from image \"{ image_src }\" has been found"))
    }
  }

  return(containers)
}

#' existing_containers
#' @export

existing_containers <- function() list_container() %>% pull(names)

#' running_containers
#' @export

running_containers <- function() list_container(not_running = F) %>% pull(names)

#' stopped_containers
#' @export

stopped_containers <- function() setdiff(existing_containers(), running_containers())


#' is_running
#' @description This functions allows to check whether a container is running. One can either look by name or by image. If name and image are specified, name condition is prioritized.
#' @param name A string providing the name of a container, whose state should be checked
#' @param image_src A string providing an image, from which derived containers should be checked
#' @param quiet Should the function return a logical in case a container matching the condition is running
#' @return If a name is given as input, the function returns a message indicating whether a container named according to the input is running.
#' @return If an image is given as input, the fucntion returns a message indicating the number of containers built on the input images are running.
#' @return If quiet is False, the function will also return a logical vector indicating whether the condition is fulfilled.
#' @export

is_running <- function(name = NULL,
                       image_src = NULL,
                       quiet = T,
                       return_logical = F){

  if(!is.null(name)){
    trig <- container_list(not_running = F) %>%
      filter(names == name) %>%
      nrow

    if(trig != 0){
      if(!quiet) message(glue("Container { name } is running"))
      if(return_logical) return(T)
    } else {
      if(!quiet) message(glue("Container { name } is not running"))
      if(return_logical) return(F)
    }
  } else {
    if(!is.null(image_src)){
      trig <- container_list(not_running = F) %>%
        filter(str_detect(image, image_src)) %>%
        nrow

      if(trig == 1){
        if(!quiet) message(glue("{ trig } container built on { image_src } is running"))
        if(return_logical) return(T)
      } else {
        if(trig > 1 ){
          if(!quiet) message(glue("{ trig } container built on { image_src } are running"))
          if(return_logical) return(T)
        } else {
          if(trig == 0){
            if(!quiet) message(glue("No container built on { image_src } is running"))
            if(return_logical) return(F)
          }
        }
      }
    }
  }
}

#' create_container
#' @description This function allows to create container based on an image
#' @export

create_container <- function(image_src = NULL,
                             container_name = NULL,
                             expose_port = NULL,
                             port = NULL){

  if(container_name %in% existing_containers()){
    stop(glue("A container is already named { container_name }.\n
              Please choose another name"))
  }

  name <- ifelse(is.null(container_name), "", glue("--name { container_name }"))
  expose_port <- ifelse(is.null(expose_port), "", glue("--expose { expose_port }"))
  port <- ifelse(is.null(port), "P", glue(" -p { port }"))

  system(glue("docker run -dt{ port} {expose_port} { name } {image_src}"), ignore.stdout = T)

  if(container_name %in% running_containers()){
    message(glue("{ container_name } was successfully started"))
  }
}


#' start_container
#' @description This function allows to start an existing container
#' @export

start_container <- function(container_name){

  if(container_name %in% running_containers()){
    message(glue("{ container_name } is already running"))
  } else {

    if(!container_name %in% existing_containers()){
      stop(glue("There is no container named { container_name }\n
               Please check whether the container has been properly created."))
    }

    if(container_name %in% stopped_containers()){
      system(glue("docker start { container_name }"), ignore.stdout = T)
    }
    if(is_running(name = container_name, return_logical = T)){
      message(glue("{ container_name } was successfully started"))
    }
  }
}


#' stop_container
#' @description This function allows to stop a running container
#' @export

stop_container <- function(container_name, remove = F){
  if(!container_name %in% existing_containers()){
    stop(glue("{ container_name } does not exist"))
  }

  if(container_name %in% stopped_containers()){
    message(glue("{ container_name } is already stopped"))
  }

  if(container_name %in% running_containers()){
    system(glue("docker stop { container_name }"), ignore.stdout = T)

    if(container_name %in% stopped_containers()){
      message(glue("{ container_name } was successfully stopped"))
    }
  }

  if(remove) system(glue("docker rm { container_name }"), ignore.stdout = T)
  if(!container_name %in% existing_containers()){
    message(glue("{ container_name } was succesfully removed"))
  }
}

#' remove_container
#' @description This function allows to remove a stopped container
#' @export

remove_container <- function(container_name){
  if(!container_name %in% existing_containers()){
    stop(glue("{ container_name } does not exist"))
  }

  if(container_name %in% running_containers()){
    stop(glue("{ container_name } cannot be removed, because it is running.\n
              Stop it first using: stop_container(\'{ container_name }\') "))
  }

  if(container_name %in% stopped_containers()){
    system(glue("docker rm { container_name }"), ignore.stdout = T)

    if(!container_name %in% existing_containers()){
      message(glue("{ container_name } was successfully removed"))
    }
  }
}

#' get_port
#' @export

get_port <- function(container_name, filter_port = NULL){

  ports <- container_list() %>%
      filter(names == container_name) %>%
      pull(ports) %>%
      str_split(", ") %>%
      unlist %>%
      map(~{
        .x %>%
          str_extract("\\d+->\\d+") %>%
          str_split("->") %>%
          unlist %>%
          as.integer %>%
          set_names("origin", "target")
      }) %>%
      reduce(bind_rows)

  if(is.null(filter_port)){
    return(ports)
  }

  if(length(filter_port) == 1){
    return(
      ports %>%
             filter(target == filter_port) %>%
             pull(origin)
      )
  }

  if(length(filter_port) > 1){
    return(ports %>% filter(target %in% filter_port))
  }
}

#' view_container
#' @export

view_container <- function(container_name ,
                           viewer = "vnc"){

  if(viewer == "vnc"){
    port <- get_port(container_name, 5900)
    system(glue("sudo open vnc://root:secret@localhost:{ port }"))
  }

}
