

library(stringr)
library(xts)

#' Read the greenfeed visit data.
#'
#' \code{readGFVisitData} returns the visit data from an animal with RFID gf_rfid in a given greenfeed csv file
#'
#' Details
#'
#' @export

readGFVisitData <- function(gf_rfid, gffile){

  data <- read.table(gffile, stringsAsFactors=FALSE, header=TRUE, sep=";", dec=",")

  # adapt the RFID format and import the file as a data frame
  RFID = str_c("GF_",str_replace(gf_rfid, "246000", ""))
  gf_em.df <- data[data$RFID_Number==RFID, c("Measure_Time", "CO2_gperd", "CH4_gperd")]
  colnames(gf_em.df) <- c("Measure_Time", "gf_co2", "gf_ch4")

  # change into a time serie
  gf_em.xts <- xts(gf_em.df[-1], as.POSIXct(gf_em.df$Measure_Time, format="%m/%d/%Y %H:%M"))

  # add ch4 ratio column
  gf_em.xts <- as.xts(transform(gf_em.xts, gf_ch4ratio =  gf_ch4 / (gf_co2 + gf_ch4)))

  return(gf_em.xts)
}

#' merge weight and gf visit data.
#'
#' \code{mergeData} returns the visit data from an animal with the interpolated weights info added
#'
#' Details
#'
#' @export

mergeData <- function(weights.xts, gf_em.xts){

  try(if (nrow(gf_em.xts) == 0) stop( "No GF Data !!!"))

  data.xts <- merge(gf_em.xts, weight.xts)

  # Add interpolated weights
  if (DEBUG){print("Add interpolated weights")}
  data.xts$int_weight <- na.approx(data.xts$weight)

  # remove weight column and weighing rows (with NAs)
  data.xts$weight <- NULL
  data.xts <- data.xts[complete.cases(data.xts), ]

  # Adding emmission over weight and GQM variables in weekly data
  data.xts <- as.xts(transform(data.xts, gf_co2weight_ratio = gf_co2 / int_weight))
  data.xts <- as.xts(transform(data.xts, gf_ch4weight_ratio = gf_ch4 / int_weight))
  data.xts <- as.xts(transform(data.xts, gf_co2metweight_ratio = gf_co2 / (int_weight^0.75)))
  data.xts <- as.xts(transform(data.xts, gf_ch4metweight_ratio = gf_ch4 / (int_weight^0.75)))

  return(data.xts)
}

#aggregateToWeeklydata <- function(data.xts){
#  # agregating every week and recompute pasture variable
#  weeklydata.xts <- xts(aggregate(data.xts, nextmonnoon, mean))
#  return(theObject)
#}

# TO DO: Handle pasture periods
#data.xts <- as.xts(transform(data.xts, pasture = ifelse(index(data.xts) > theObject@pasturebegin &  index(data.xts) < theObject@pastureend, TRUE, FALSE )))
#weeklydata.xts <- as.xts(transform(weeklydata.xts, pasture = ifelse(index(weeklydata.xts) > theObject@pasturebegin &  index(weeklydata.xts) < theObject@pastureend, "pasture", "barn" )))

#readGFVisitData(gf_rfid = "246000039525", gffile = "./../../GHGData/RawData/GreenFeed/ResultsFiles/GF_Data_Trait_02.csv")
