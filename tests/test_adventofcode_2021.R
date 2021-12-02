# test_adventofcode_2021.R

library(testthat)
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  #library(gmp)
})

rdir <- "../R"
#rdir <- "R"
source( file.path( rdir, "adventofcode_2021.R" ) )

# Day 1 ----

test_that( "count_increases", {
  ex1 <- c( 199, 200, 208, 210, 200
          , 207, 240, 269, 260, 263 )
  ans <- count_increases( ex1 )
  expect_equal( 7, ans )
})

