# Several functions to read information from the journals

library(xts)

#' Read the race of an animal.
#'
#' \code{readRace} returns the race of the animal with the given id, as written in the file.
#'
#' Details
#'
#' @export

readRace <- function(id, file){

  # Read the file
  data.df <- read.csv(file, stringsAsFactors=FALSE, sep = ";")
  # Get the row and check that there is only one
  row.df <- data.df[as.numeric(data.df$Boucle) == id, ]
  try(if(nrow(row.df) !=1) stop(paste("Several rows with same ID in ", file)))
  #return the race
  return(row.df$Race)
}

#' Read the state of an animal.
#'
#' \code{readState} returns the state of the animal with the given id, as written in the file.
#'
#' Details
#'
#' @export

readState <- function(id, file){

  # Read the file
  data.df <- read.csv(file, stringsAsFactors=FALSE, sep = ";")
  # Get the row and check that there is only one
  row.df <- data.df[as.numeric(data.df$Boucle) == id, ]
  try(if(nrow(row.df) !=1) stop(paste("Several rows with same ID in ", file)))
  #return the state
  return(row.df$État)
}

#' Read the gender of an animal.
#'
#' \code{readGender} returns the gender of the animal with the given id, as written in the file.
#'
#' Details
#'
#' @export

readGender <- function(id, file){

  # Read the file
  data.df <- read.csv(file, stringsAsFactors=FALSE, sep = ";")
  # Get the row and check that there is only one
  row.df <- data.df[as.numeric(data.df$Boucle) == id, ]
  try(if(nrow(row.df) !=1) stop(paste("Several rows with same ID in ", file)))
  #return the gender
  return(row.df$Genre)
}

#' Read the moves of an animal.
#'
#' \code{readMoves} returns the moves (xts) of the animal with the given id, as written in the file.
#'
#' Details
#'
#' @export

readMoves <- function(id, file){

  # Read the file
  data.df <- read.csv(file, stringsAsFactors=FALSE, sep = ";", check.names=FALSE)
  # Get the movement rows
  moves.df <- data.df[(data.df$Opération == "Étable" | data.df$Opération == "Pâture"), ]

  # Select the collumn with Date, Hour and the column of the animal and remove empty rows
  moves.df <- moves.df[, c("Date", "Heure", id)]
  colnames(moves.df) <- c("Date", "Hour", "Moves")
  moves.df <- moves.df[moves.df$Moves != "", ]

  moves.xts <- xts(moves.df["Moves"], order.by=as.POSIXct(paste(moves.df$Date, moves.df$Hour), format = "%d/%m/%Y %H:%M:%S"))

  # Check that the date is realistic
  checkDate(min(index(moves.xts)))
  checkDate(max(index(moves.xts)))

  #return the xts
  return(moves.xts)
}

#' Read the weighing of an animal.
#'
#' \code{readWeighing} returns the weighing values (xts) of the animal with the given id, as written in the file.
#'
#' Details
#'
#' @export

readWeighing <- function(id, file){
  # Read the file
  weights.df <- read.csv(file, stringsAsFactors=FALSE, sep = ";", check.names=FALSE)

  # Select the column with Date, Hour and the column of the animal and remove empty rows
  weights.df <- weights.df[, c("Date", "Heure", id)]
  colnames(weights.df) <- c("Date", "Hour", "Weight")
  weights.df <- weights.df[!(weights.df$Weight == "" | is.na(weights.df$Weight)), ]

  # transform into a time serie
  weights.xts <- xts(weights.df["Weight"], order.by=as.POSIXct(paste(weights.df$Date, weights.df$Hour), format = "%d/%m/%Y %H:%M:%S"))

  checkDate(min(index(weights.xts)))
  checkDate(max(index(weights.xts)))

  #return the xts with Weights
  return(weights.xts)
}

#' Check the date format.
#'
#' \code{checkDate} apply several test on the Date and stop if problem detected
#'
#' Details

checkDate <- function(date){
  if(year(date) < 2000 || year(date) > 3000) stop("Date format not adapted: year shoould be > 2000 and < 3000")
  return(TRUE)
}

# examples on how to use:
# readRace(id="6806", file="./../../HerdData/Infos/BBMLIM.csv")
# readState(id="6806", file="./../../HerdData/Infos/BBMLIM.csv")
# readGender(id="6806", file="./../../HerdData/Infos/BBMLIM.csv")
# readMoves(id="6806", file="./../../HerdData/Positions/BBMLIM.csv")
# readWeighing(id="6806", file="./../../HerdData/Weighing/BBMLIM.csv")
