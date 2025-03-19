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
fetch_color <- function(color_source, num){

  color_vec <- color_dict[[color_source]][1:num]

  return(color_vec)
}
