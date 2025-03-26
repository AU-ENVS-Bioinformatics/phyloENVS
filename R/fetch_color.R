#' @title Fetch color
#' @description Fetches colors from color dictionary.
#'
#' @param color_source color source/palette in the dictionary (main1, main2, ect.).
#' @param num number of colors to output.
#'
#' @return a vector with the HEX values for the color code.
#' @export
#'
#' @examples
fetch_color <- function(num, color_source = NULL){

  if (num > 18){
    stop("Error: The number of distinct colors exceeds the allowed threshold of 18.")
  }

  if (!is.null(color_source)){
    color_vec <- color_dict[[color_source]][1:num]
  }
  else if (num <= 9){
    color_vec <- color_dict[["AU1"]][1:num]
  }
  else{
    color_vec <- color_dict[["AU2"]][1:num]
  }

  return(color_vec)
}
