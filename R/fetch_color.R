#' @title Fetch color
#' @description Fetches colors from color dictionary.
#'
#' @param num number of colors to output (up to 18 is allowed).
#' @param color_source optional color source/palette in the dictionary ("AU1", "AU2", "Aublue", ect. - see README.md). If not defined "AU1" will be used for num less than or equal to 9. Otherwise the default is "AU2".
#'
#' @return a vector with the HEX values for the color code.
#' @export
#'
#' @examples
#' fetch_color(num = 3, color_source = "AU1")
#'
fetch_color <- function(num, color_source = NULL){

  # ------------#
  # Check inputs
  # ------------#

  if (!is.numeric(num) | as.integer(num) != num){
    stop("`num` must be an integer")
  }

  if (num > 18){
    stop("The number of distinct colors exceeds the allowed threshold of 18", call. = FALSE)
  }

  if (!is.null(color_source) && !(color_source %in% c("AU1", "AU2", "AUblue", "AUturquoise", "AUorange", "AUpurple", "AUgreen", "AUred", "AUcyan", "AUyellow", "AUpink"))) {
    stop("Invalid color source. Supported: 'AU1', 'AU2', 'AUblue', 'AUturquoise', 'AUorange', 'AUpurple', 'AUgreen', 'AUred', 'AUcyan', 'AUyellow', 'AUpink'", call. = FALSE)
  }

  # ------------#

  if (!is.null(color_source)){
    if (color_source %in% c("AU1", "AU2")){
      color_vec <- color_dict[[color_source]][1:num]
    } else {
      color_index <- round(seq(from = 1, to = 18, length.out = num))
      color_vec <- color_dict[[color_source]][color_index]
    }
  }
  else if (num <= 9){
    color_vec <- color_dict[["AU1"]][1:num]
  }
  else{
    color_vec <- color_dict[["AU2"]][1:num]
  }

  return(color_vec)
}
