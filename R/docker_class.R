#' docker
#' @export

docker <- R6::R6Class(
  private = list(
    image_src = NULL
    #other_arguments = NULL,
    #expose_port = NULL
  ),
  public = list(
    container_name = NULL,
    status = NULL,
    start = function(){
      start_container(self$container_name)
      self$status <- "Running"
    },
    stop = function(remove = F){
      stop_container(self$container_name, remove = remove)
      self$status <- "Stopped"
    },
    port = function(filter_port = NULL){
      get_port(container_name = self$container_name, filter_port = filter_port)
    },
    exec = function(command, sudo = F, ...){
      bashR::exec(
        glue::glue(
          'docker exec -t --privileged { self$container_name } /bin/bash -c "{ command }"'
        ), ...
      )
    },
    initialize = function(image_src = NULL,
                          container_name = NULL,
                          other_arguments = NULL,
                          expose_port = NULL,
                          port = NULL){

      if(!is.null(container_name)){
        if(container_name %in% existing_containers()){
          if(!is.null(image_src)){
            stop(glue::glue("A container is already named { container_name }.\n
                            Please choose another name"))
          } else {

            container <- list_container() %>%
              filter(names == container_name)

            private$image_src <- container$image
            self$container_name <- container_name

            container_name %>% start_container(.)

            if(container_name %in% running_containers()) self$status <- "Running"


          }

        } else {

          name <- ifelse(is.null(container_name), "", glue::glue("--name { container_name }"))
          expose_port <- ifelse(is.null(expose_port), "", glue::glue_collapse(glue("--expose { expose_port }"), " "))
          port <- ifelse(is.null(port), "P", glue::glue(" -p { port }"))
          arg <- ifelse(is.null(other_arguments), "", other_arguments)

          private$image_src <- image_src
          #private$other_arguments <- other_arguments
          #private$export_port <- expose_port
          #private$port <- port

          self$container_name <- container_name


          bashR::sudo(
            glue::glue("docker run -dt{ port} { arg } {expose_port} { name } {image_src}"),
            ignore.stdout = T)

          if(container_name %in% running_containers()){
            self$status <- "Running"
            message(glue::glue("{ container_name } was successfully started"))
          }
        }
      } else {
        stop("Please provide a container name")
      }
      invisible(self)
    }

  )
)
