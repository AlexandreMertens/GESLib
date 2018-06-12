library(xts)
library(lubridate)

setOldClass("xts")

#' Animal Object.
#'
#' Animal Object
#'
#' Details
#'
#' @export
Animal <- setClass(
  "Animal",
  slots = c(
    id = "character",
    gfid = "list",
    race = "character",
    state = "character",
    stateInfo = "character",
    birthDate = "Date",
    weighing = "xts",
    moves = "xts",
    gf_em = "xts",
    data = "xts",
    gfaccess = "logical",
    weeklydata = "xts",
    pasturebegin = "POSIXct",
    pastureend = "POSIXct"
  ),
  prototype = list(
    id = "UNKNOWN",
    gfid = list(),
    race = "UNKNOWN",
    state = "UNKNOWN",
    stateInfo = "",
    birthDate = as.Date("1/1/1900"),
    weighing = xts(),
    moves = xts(),
    gf_em = xts(),
    data = xts(),
    weeklydata = xts(),
    gfaccess = FALSE,
    pasturebegin = as_datetime("31/12/1987 10:00"),
    pastureend = as_datetime("31/12/1987 10:00")
  )
)

#' Set the race of an animal object.
#'
#' \code{setRace} sets the race of the animal object to the race value.
#'
#' Details
#'
#' @export

setGeneric(name="setRace",
           def=function(theObject,race)
           {
             standardGeneric("setRace")
           }
)
setMethod(f="setRace",
          signature = "Animal",
          definition = function(theObject,race)
          {
            theObject@race = race
            return(theObject)
          }
)

#' Get the race of an animal object.
#'
#' \code{getRace} returns the race of the animal object to the race value.
#'
#' Details
#'
#' @export

setGeneric(name="getRace",
           def=function(theObject)
           {
             standardGeneric("getRace")
           }
)
setMethod(f="getRace",
          signature = "Animal",
          definition = function(theObject)
          {
            return(theObject@race)
          }
)

#' Set the state of an animal object.
#'
#' \code{setState} sets the state of the animal object to the race value.
#'
#' Details
#'
#' @export

setGeneric(name="setState",
           def=function(theObject,state)
           {
             standardGeneric("setState")
           }
)
setMethod(f="setState",
          signature = "Animal",
          definition = function(theObject,state)
          {
            theObject@state = state
            return(theObject)
          }
)

#' Get the state of an animal object.
#'
#' \code{getState} returns the state of the animal object to the race value.
#'
#' Details
#'
#' @export

setGeneric(name="getState",
           def=function(theObject)
           {
             standardGeneric("getState")
           }
)
setMethod(f="getState",
          signature = "Animal",
          definition = function(theObject)
          {
            return(theObject@state)
          }
)

#' Set the state info of an animal object.
#'
#' \code{setStateInfo} sets the state info of the animal object to the stateInfo value.
#'
#' Details
#'
#' @export
setGeneric(name="setStateInfo",
           def=function(theObject,stateInfo)
           {
             standardGeneric("setStateInfo")
           }
)
setMethod(f="setStateInfo",
          signature = "Animal",
          definition = function(theObject,stateInfo)
          {
            theObject@stateInfo = stateInfo
            return(theObject)
          }
)

#' Get the state info of an animal object.
#'
#' \code{getStateInfo} returns the state info of the animal object to the stateInfo value.
#'
#' Details
#'
#' @export

setGeneric(name="getStateInfo",
           def=function(theObject)
           {
             standardGeneric("getStateInfo")
           }
)
setMethod(f="getStateInfo",
          signature = "Animal",
          definition = function(theObject)
          {
            return(theObject@stateInfo)
          }
)

#' Set the birth date of an animal object.
#'
#' \code{setBirthDate} sets the birth date of the animal object to the given value.
#'
#' Details
#'
#' @export

setGeneric(name="setBirthDate",
           def=function(theObject,birthDate)
           {
             standardGeneric("setBirthDate")
           }
)
setMethod(f="setBirthDate",
          signature = "Animal",
          definition = function(theObject,birthDate)
          {
            theObject@birthDate = birthDate
            return(theObject)
          }
)

#' Get the birth date of an animal object.
#'
#' \code{getBirthDate} returns the birthDate of the animal object.
#'
#' Details
#'
#' @export

setGeneric(name="getBirthDate",
           def=function(theObject)
           {
             standardGeneric("getBirthDate")
           }
)
setMethod(f="getBirthDate",
          signature = "Animal",
          definition = function(theObject)
          {
            return(theObject@birthDate)
          }
)


#' Set the GreenFeed ID of an animal object.
#'
#' \code{setGFID} sets the greenfeed ID of the animal object to the gfid value.
#'
#' Details
#'
#' @export

setGeneric(name="setGFId",
           def=function(theObject,gfid)
           {
             standardGeneric("setGFId")
           }
)
setMethod(f="setGFId",
          signature = "Animal",
          definition = function(theObject,gfid)
          {
            theObject@gfid = gfid
            return(theObject)
          }
)

#' Get the GreenFeed ID of an animal object.
#'
#' \code{getGFID} returns the greenfeed ID of the animal object to the gfid value.
#'
#' Details
#'
#' @export

setGeneric(name="getGFId",
           def=function(theObject)
           {
             standardGeneric("getGFId")
           }
)
setMethod(f="getGFId",
          signature = "Animal",
          definition = function(theObject)
          {
            return(theObject@gfid)
          }
)

#' Set the Weighing values of an animal object.
#'
#' \code{setWeighing} sets the weights of the animal object to the weighing value.
#'
#' Details
#'
#' @export

setGeneric(name="setWeighing",
           def=function(theObject,weighing)
           {
             standardGeneric("setWeighing")
           }
)
setMethod(f="setWeighing",
          signature = "Animal",
          definition = function(theObject,weighing)
          {
            theObject@weighing = weighing
            return(theObject)
          }
)

#' Get the Weighing values of an animal object.
#'
#' \code{getWeighing} returns the weights of the animal object to the weighing value.
#'
#' Details
#'
#' @export

setGeneric(name="getWeighing",
           def=function(theObject)
           {
             standardGeneric("getWeighing")
           }
)
setMethod(f="getWeighing",
          signature = "Animal",
          definition = function(theObject)
          {
            return(theObject@weighing)
          }
)

#' Set the moves of an animal object.
#'
#' \code{setMoves} sets the moves of the animal object to the moves value.
#'
#' Details
#'
#' @export

setGeneric(name="setMoves",
           def=function(theObject,moves)
           {
             standardGeneric("setMoves")
           }
)
setMethod(f="setMoves",
          signature = "Animal",
          definition = function(theObject,moves)
          {
            theObject@moves = moves
            return(theObject)
          }
)
#' Get the moves of an animal object.
#'
#' \code{getMoves} returns the moves of the animal object to the moves value.
#'
#' Details
#'
#' @export

setGeneric(name="getMoves",
           def=function(theObject)
           {
             standardGeneric("getMoves")
           }
)
setMethod(f="getMoves",
          signature = "Animal",
          definition = function(theObject)
          {
            return(theObject@moves)
          }
)
