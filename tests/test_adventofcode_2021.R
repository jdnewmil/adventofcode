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

# Day 2 ----

test_that( "calc_pos_2a", {
  ex2a <-
"forward 5
down 5
forward 8
up 3
down 8
forward 2"
  ans <- calc_pos_2a( ex2a )
  expect_equal( ans$dir_x, 15 )
  expect_equal( ans$dir_y, 10 )
})

test_that( "calc_pos_2b", {
  ex2a <-
    "forward 5
down 5
forward 8
up 3
down 8
forward 2"
  ans <- calc_pos_2b( ex2a )
  expect_equal( ans$x, 15 )
  expect_equal( ans$y, 60 )
})
