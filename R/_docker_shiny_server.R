#' docker_shiny_server
#' @export

docker_shiny_server <- R6::R6Class(
  inherit = dockeR::docker,
  public = list(
    app_dir = NULL,
    initialize = function(container_name, app_dir){
      self$app_dir <- app_dir
      self$status <- "Running"
      self$container_name <- container_name

      self <- docker$new(
        other_arguments = glue::glue("-v { app_dir }:/srv/shiny-server/apps"),
        container_name = container_name,
        image_src = "guiguiplot/shiny_server_base"
      )
    },
    browse = function(app = NULL){
      app <- ifelse(is.null(app), "", glue::glue("/{ app }"))

      port <- self$port(filter_port = 3838)
      browseURL(glue::glue("http://localhost:{ port }/apps{ app }"))
    },
    install_packages = function(...){
      packages <- as.character(match.call(expand.dots = FALSE)[[2]]) %>%
        glue::glue_collapse('\\\", \\\"')

      self$exec(
        paste0(
          "Rscript -e ",
          "\'.libPaths(c(\\\"/usr/local/lib/R/site-library\\\",\\\"/usr/local/lib/R/library\\\"));",
          "library(pacman) ;",
          glue::glue("purrr::map(c(\\\"{ packages }\\\"), ~p_load(char = .x))\'")
        )
      )
    }
  )
)
