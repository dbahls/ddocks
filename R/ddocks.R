#' My awesome function
#'
#' @param name The name that is to be printed
#' @return void
#'
#' @examples
#' hello_world("Max")
#'
#' @author max
#' @export
hello_world <- function(name){
	print(sprintf("Hello %s!", name))
}

#' My awesome function number 2
#'
#' @param name The name that is to be printed
#' @return void
#'
#'
#' @author max
#' @export
hello_world_alternative <- function(name){
	print(sprintf("Cool to meet you, %s!", name))
}