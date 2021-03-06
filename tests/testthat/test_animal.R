library(testthat)
library(GESLib)
library(xts)
context("Animal simple tests")

Sys.setenv(TZ='Europe/Brussels')

animal <- new("Animal", id="X6831")
animal <- setRace(animal, readRace(id="6831", file="../../data/test_infos.csv"))
animal <- setState(animal, readState(id="6831", file="../../data/test_infos.csv"))
animal <- setBirthDate(animal, readBirthDate(id="6831", file="../../data/test_infos.csv"))
animal <- setRepetition(animal, readRepetition(id="6831", file="../../data/test_infos.csv"))
animal <- setTreatment(animal, readTreatment(id="6831", file="../../data/test_infos.csv"))

test_that("IDs, race and state are correctly saved",{
  expect_equal(animal@id, "X6831")
  expect_equal(getRace(animal), "BBM")
  expect_equal(getState(animal), "castré")
})

test_that("Repetition and treatment are correctly saved",{
  expect_equal(getRepetition(animal), "R1")
  expect_equal(getTreatment(animal), "T1")
})


test_that("GF ids are well saved",{
  expect_equal(length(readGFId(id='6831', file="../../data/test_gfid.csv")), 2)
})

animal <- setMoves(animal, readMoves(id="6831", file="../../data/test_moves.csv"))

test_that("Moves is xts with realistic dates",{
  expect_s3_class(getMoves(animal), "xts")
})

test_that("Position is correct",{
  expect_equal(getAnimalPosition(animal,as_datetime(dmy("10/05/2017"))), "NE")
  expect_equal(getAnimalPosition(animal,as_datetime(dmy("30/05/2017"))), "P")
})


animal2 <- new("Animal", id="X0234")
animal2 <- setWeighing(animal, readWeighing(id="234", file="../../data/test_weighing.csv"))

test_that("Weighing is xts with realistic dates",{
  expect_s3_class(getWeighing(animal2), "xts")
})

GF_data <- readGFVisitData(gf_rfids=list("246000039525"), gffile="../../data/test_GF_data.csv")

merged_GF_data <- mergeData(getWeighing(animal2), GF_data)
test_that("readGFVisitData returns a xts",{
  expect_s3_class(GF_data, "xts")
})


#animal@weighing
#animal@moves

#animal <- importGreenFeedData(animal, greenfeedData)

#print(animal@data)
#print(animal@weeklydata)
