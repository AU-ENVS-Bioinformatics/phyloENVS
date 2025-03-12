#' Fetches shapes from shape dictionary
#'
#' @param num number of shapes to output.
#'
#' @return a vector with the shape values.
#' @export
#'
#' @examples
fetch_shape <- function(num){

  if (num > 10){
    stop("Error: The number of distinct shape exceeds the allowed threshold of 10.")
  }
  if (num <= 5){
    shape_vec <- shape_dict[["small"]][1:num]
  }
  else{
    shape_vec <- shape_dict[["large"]][1:num]
  }

  return(shape_vec)
}
