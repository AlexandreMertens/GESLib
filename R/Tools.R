library(stringr)
library(xts)

#' Read the greenfeed visit data.
#'
#' \code{getAnimalPosition} returns the position of the animal with given ID at the requested datetime.
#'
#' Details
#'
#' @export
getAnimalPosition <- function(animal, date){

  moves <- getMoves(animal)
  movesbefore <- moves[paste0("/",as.character(date))]
  previousmove <- last(movesbefore)
  test <- as.character(previousmove$Moves)

  return(test)
}
