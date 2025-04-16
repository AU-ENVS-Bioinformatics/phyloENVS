#' @title Fetch shape
#' @description Fetches shapes from shape dictionary.
#'
#' @param num number of shapes to output (up to 10 is allowed).
#'
#' @return a vector with the shape values.
#' @export
#'
#' @examples
#' fetch_shape(num = 3)
#'
fetch_shape <- function(num){

  # ------------#
  # Check inputs
  # ------------#

  if (!is.numeric(num) | as.integer(num) != num){
    stop("`num` must be an integer")
  }

  if (num > 10){
    stop("The number of distinct shapes exceeds the allowed threshold of 10", call. = FALSE)
  }

  # ------------#

  if (num <= 5){
    shape_vec <- shape_dict[["small"]][1:num]
  }
  else{
    shape_vec <- shape_dict[["large"]][1:num]
  }

  return(shape_vec)
}
