#' prune_containers
#' @export

is_docker_running <- function(){length(list_container()) != 0}

#' prune_containers
#' @export

prune_containers <- function() purrr::walk(existing_containers(), stop_container, remove = T)

#' list_container
#' @description This function lists all running containers. If image_src is specified, it only return containers, of which the building image matches the given string.
#' @param image_src A String providing a specific image to filter
#' @param not_running a logical vector specifying whether not_running containers should be returned
#' @return a tibble listing containers with their information
#' @export

list_container <- function(image_src = NULL, not_running = T){

  if(not_running){
    raw_list <- sudo("docker ps -a --no-trunc", intern = T)
  } else {
    raw_list <- sudo("docker ps --no-trunc", intern = T)
  }

  if(length(raw_list) == 1){
    return(
      tibble::tibble("container id" = NA_character_,
                     "image" = NA_character_,
                     "command" = NA_character_,
                     "created" = NA_character_,
                     "status" = NA_character_,
                     "ports" = NA_character_,
                     "names" = NA_character_) %>%
        janitor::clean_names(.)
    )
  }

  col_names <- raw_list[1]  %>%
    stringr::str_extract_all("(?<=\\s{2}|^).*?(\\s{2,}|$)") %>% .[[1]] %>%
    stringr::str_trim(.)

  border <- raw_list[1]  %>%
    stringr::str_locate_all("(?<=\\s{2}|^).*?(\\s{2,}|$)") %>% .[[1]]

  containers <- raw_list %>%
    tail(-1) %>%
    purrr::map_dfr(~{
      border[nrow(border),2] <- stringr::str_length(.x)

      .x %>%
        stringr::str_sub(start = border[,1], end = border[,2]) %>%
        t %>%
        tibble::as_tibble(.) %>%
        purrr::set_names(col_names)}) %>%
    janitor::clean_names(.)

  if(!is.null(image_src)){
    containers <- containers %>%
      dplyr::filter(image %>% stringr::str_detect(image_src))

    if(nrow(containers) == 0){
      message(glue::glue("No container build from image \"{ image_src }\" has been found"))
    }
  }

  return(containers)
}

#' existing_containers
#' @export

existing_containers <- function() list_container() %>% dplyr::pull(names) #%>% c(., "")

#' running_containers
#' @export

running_containers <- function() list_container(not_running = F) %>% dplyr::pull(names) #%>% c(., "")

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
    trig <- list_container(not_running = F) %>%
      dplyr::filter(names == name) %>%
      nrow

    if(trig != 0){
      if(!quiet) message(glue::glue("Container { name } is running"))
      if(return_logical) return(T)
    } else {
      if(!quiet) message(glue::glue("Container { name } is not running"))
      if(return_logical) return(F)
    }
  } else {
    if(!is.null(image_src)){
      trig <- list_container(not_running = F) %>%
        dplyr::filter(stringr::str_detect(image, image_src)) %>%
        nrow

      if(trig == 1){
        if(!quiet) message(glue::glue("{ trig } container built on { image_src } is running"))
        if(return_logical) return(T)
      } else {
        if(trig > 1 ){
          if(!quiet) message(glue::glue("{ trig } container built on { image_src } are running"))
          if(return_logical) return(T)
        } else {
          if(trig == 0){
            if(!quiet) message(glue::glue("No container built on { image_src } is running"))
            if(return_logical) return(F)
          }
        }
      }
    }
  }
}

#' list_images
#' @export

list_images <- function(){
  sudo("docker images", intern = T) %>%
    purrr::map(stringr::str_split, "\\s{2,}") %>%
    purrr::map(unlist) %>%
    purrr::map_dfc(tibble::as_tibble) %>%
    t %>%
    as.data.frame %>%
    tibble::as_tibble(.) %>%
    purrr::set_names(as.character(t(.[1,]))) %>%
    tail(-1) %>%
    janitor::clean_names(.)

}

#' create_container
#' @description This function allows to create container based on an image
#' @export

create_container <- function(image_src = NULL,
                             container_name = NULL,
                             other_arguments = NULL,
                             expose_port = NULL,
                             port = NULL){

  if(!is_docker_running()){stop("Docker daemon is not running, please start it and try again")}

  if(!is.null(container_name)){
    if(container_name %in% existing_containers()){
      stop(glue("A container is already named { container_name }.\n
                Please choose another name"))
    }
  }

  name <- ifelse(is.null(container_name), "", glue::glue("--name { container_name }"))
  expose_port <- ifelse(is.null(expose_port), "", glue::glue_collapse(glue::glue("--expose { expose_port }"), " "))
  port <- ifelse(is.null(port), "P", glue::glue(" -p { port }"))
  arg <- ifelse(is.null(other_arguments), "", other_arguments)

  sudo(glue::glue("docker run -dt{ port} { arg } {expose_port} { name } {image_src}"), ignore.stdout = T)

  if(container_name %in% running_containers()){
    message(glue::glue("{ container_name } was successfully started"))
  }
}



#' load_container
#' @export

load_container <- function(container_name){
  if(!is_docker_running()){stop("Docker daemon is not running, please start it and try again")}
  chrome <- docker$new(container_name = "chrome")
}

#' start_container
#' @description This function allows to start an existing container
#' @export

start_container <- function(container_name){

  if(!is_docker_running()){stop("Docker daemon is not running, please start it and try again")}

  container_name <- check_container_name(container_name)

  if(container_name %in% running_containers()){
    message(glue::glue("{ container_name } is already running"))
  } else {

    if(!container_name %in% c(existing_containers(), "")){
      stop(glue::glue("There is no container named { container_name }\n
               Please check whether the container has been properly created."))
    }

    if(container_name %in% stopped_containers()){
      sudo(glue::glue("docker start { container_name }"), ignore.stdout = T)
    }
    if(is_running(name = container_name, return_logical = T)){
      message(glue::glue("{ container_name } was successfully started"))
    }
  }
}


#' stop_container
#' @description This function allows to stop a running container
#' @export

stop_container <- function(container_name, remove = F){
  if(!is_docker_running()){stop("Docker daemon is not running, please start it and try again")}
  container_name <- check_container_name(container_name)

  if(!container_name %in% existing_containers()){
    stop(glue::glue("{ container_name } does not exist"))
  }

  if(container_name %in% stopped_containers()){
    message(glue::glue("{ container_name } is already stopped"))
  }

  if(container_name %in% running_containers()){
    sudo(glue::glue("docker stop { container_name }"), ignore.stdout = T)

    if(container_name %in% stopped_containers()){
      message(glue::glue("{ container_name } was successfully stopped"))
    }
  }

  if(remove) sudo(glue::glue("docker rm { container_name }"), ignore.stdout = T)
  if(!container_name %in% existing_containers()){
    message(glue::glue("{ container_name } was succesfully removed"))
  }
}

#' remove_container
#' @description This function allows to remove a stopped container
#' @export

remove_container <- function(container_name){
  if(!is_docker_running()){stop("Docker daemon is not running, please start it and try again")}
  container_name <- check_container_name(container_name)

  if(!container_name %in% existing_containers()){
    stop(glue::glue("{ container_name } does not exist"))
  }

  if(container_name %in% running_containers()){
    stop(glue::glue("{ container_name } cannot be removed, because it is running.\n
              Stop it first using: stop_container(\'{ container_name }\') "))
  }

  if(container_name %in% stopped_containers()){
    sudo(glue::glue("docker rm { container_name }"), ignore.stdout = T)

    if(!container_name %in% existing_containers()){
      message(glue::glue("{ container_name } was successfully removed"))
    }
  }
}

#' get_port
#' @description This function return the port exposed on a specific container and their allocated port on the main computer.
#' @param container_name The name of the container
#' @param filter_port If one value, it returs the corresponding port on the main computer. If several values, the function returns a tibble matching ports of the main computer with ports from the container.
#' @return A tibble or a single value depending on filter_port
#' @export


get_port <- function(container_name, filter_port = NULL){

  container_name <- check_container_name(container_name)

  if(!container_name %in% running_containers()){stop(container_name, " is not running. Please create or start the container.")}

  ports <- list_container() %>%
    dplyr::filter(names == container_name) %>%
    dplyr::pull(ports) %>%
    stringr::str_split(", ") %>%
    unlist %>%
    purrr::map(~{
      .x %>%
        stringr::str_extract("\\d+->\\d+") %>%
        stringr::str_split("->") %>%
        unlist %>%
        as.integer %>%
        purrr::set_names("origin", "target") %>%
        dplyr::bind_rows(.)
    }) %>%
    purrr::reduce(dplyr::bind_rows)

  if(is.null(filter_port)){
    return(ports)
  }

  if(length(filter_port) == 1){
    return(
      ports %>%
        dplyr::filter(target == filter_port) %>%
        dplyr::pull(origin)
    )
  }

  if(length(filter_port) > 1){
    return(ports %>% dplyr::filter(target %in% filter_port))
  }
}

#' view_container
#' @description This function allows to visualize what is happening inside a container. Note that the container must expose the port 5900
#' @param container_name The name of a container to visualize
#' @param viewer the device to use to visualize the container. So far only vncviewer is supported.
#' @export

view_container <- function(container_name ,
                           server = "",
                           port = "",
                           viewer = "vnc"){
  server <- ifelse(server == "", "localhost", server)
  port <- ifelse(port == "", get_port(container_name, 5900), port)

  if(viewer == "vnc"){
    sudo(glue::glue("sudo open vnc://root:secret@{server}:{ port }"))
  }

}

#' doc_copy
#' @export

doc_copy <- function(container, from = NULL, from_cont = NULL, to = NULL, to_cont = NULL){
  container <- check_container_name(container)

  if(!is.null(from)){src <- from %>% clean_url}
  if(!is.null(from_cont)){src <- paste(container, from_cont, sep = ":") %>% clean_url}
  if(!is.null(to)){dest <- to}
  if(!is.null(to_cont)){dest <- paste(container, to_cont, sep = ":") %>% clean_url}

  sudo(glue::glue("docker cp {src} {dest}"))
}






