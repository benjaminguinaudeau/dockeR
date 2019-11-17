#' doc_mouse_position
#' @export

doc_mouse_position <- function(){
  pos <- doc_exec("chrome", "python -c 'from pyautogui import * ; print(position())'", intern = T)
  x <- stringr::str_extract(pos, "(?<=x=)\\d+")
  y <- stringr::str_extract(pos, "(?<=y=)\\d+")
  return(tibble(x, y))
}

#' doc_mouse_moveTo
#' @export

doc_mouse_moveTo <- function(x, y){
  doc_exec("chrome", glue::glue("python -c 'from pyautogui import * ; moveTo({x}, {y})'"), intern = T)
}

#' doc_mouse_move
#' @export

doc_mouse_move <- function(x, y){
  doc_exec("chrome", glue::glue("python -c 'from pyautogui import * ; move({x}, {y})'"), intern = T)
}

#' nullify
#' @export

nullify <- function(x, pre = "x = ", suf = ", "){
  ifelse(is.null(x), " ", paste0(pre, x, suf))
  }

#' doc_mouse_click
#' @export

doc_mouse_click <- function(x = NULL, y = NULL, button = "left"){

  if(!is.null(x)){
    x <- sample(x, 1)
  }
  if(!is.null(y)){
    y <- sample(y, 1)
  }

  x <- nullify(x, "x = ", ", ")
  y <- nullify(y, "y = ", ", ")

  doc_exec("chrome", glue::glue("python -c 'from pyautogui import * ; click({x}{y}button = \"{button}\")'"))
}

#' doc_mouse_type
#' @export

doc_mouse_type <- function(message, interval = NULL){
  message <- nullify(message, "message = \'", "\', ")
  interval <- nullify(interval, "interval = ", ", ")
  doc_exec("chrome", glue::glue("python -c \"from pyautogui import * ; typewrite({message} {interval})\""))
}

# doc_mouse_display <- function(message, interval = NULL){
#   doc_exec("chrome", glue::glue("sudo python -c \"from pyautogui import * ; displayMousePosition()\""))
# }

#' doc_scroll
#' @export

doc_scroll <- function(x){
  doc_exec("chrome", glue::glue("python -c \"from pyautogui import * ; scroll({x})\""))
}

#' doc_locate_on_screen
#' @export

doc_locate_on_screen <- function(x){
  out <- doc_exec("chrome", glue::glue("sudo python -c \"from pyautogui import * ; locateOnScreen({x})\""), intern = T)

  x <- stringr::str_extract(out, "(?<=x=)\\d+")
  y <- stringr::str_extract(out, "(?<=y=)\\d+")
}

#' doc_screenshot
#' @export

doc_screenshot <- function(file = NULL, region = NULL){
  file <- nullify(file, "imageFilename = ")
  region <- nullify(region, "region = ")
  doc_exec("chrome", glue::glue("sudo python -c \"from pyautogui import * ; screenshot({file} {region})\""))
}

#' doc_press
#' @export

doc_press <- function (key) {
  doc_exec("chrome", glue::glue("python -c \"from pyautogui import * ; press('{key}' )\""))
}

