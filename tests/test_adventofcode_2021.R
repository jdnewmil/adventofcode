# test_adventofcode_2021.R

library(testthat)
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(gmp)
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

# Day 3 ----

test_that( "read_binary_vec", {
  lns <-
"00100
11110
10110
10111
10101"
  ans <- (  scan( text = lns, what = "character", quiet = TRUE )
         |> parse_bin()
         )
  expect_equal( ans, c( 4, 30, 22, 23, 21 ) )
})

test_that( "calc_gamma_rate", {
  lns <-
"00100
11110
10110
10111
10101
01111
00111
11100
10000
11001
00010
01010"
  ans <- (  scan( text = lns, what = "character", quiet = TRUE )
         |> parse_lgl_columns()
         |> calc_gamma_rate()
         )
  expect_equal( ans, 22 )
})

test_that( "calc_epsilon_rate", {
  lns <-
"00100
11110
10110
10111
10101
01111
00111
11100
10000
11001
00010
01010"
  ans <- (  scan( text = lns, what = "character", quiet = TRUE )
         |> parse_lgl_columns()
         |> calc_epsilon_rate()
  )
  expect_equal( ans, 9 )
})

test_that( "test_oxy_gen_criteria", {
  ans1 <- test_oxy_gen_criteria( c( TRUE, FALSE, FALSE, FALSE ) )
  expect_equal( ans1, c( FALSE, TRUE, TRUE, TRUE ) )
  ans2 <- test_oxy_gen_criteria( c( TRUE, TRUE, FALSE, FALSE ) )
  expect_equal( ans2, c( TRUE, TRUE, FALSE, FALSE ) )
  ans3 <- test_oxy_gen_criteria( c( TRUE, TRUE, TRUE, FALSE ) )
  expect_equal( ans3, c( TRUE, TRUE, TRUE, FALSE ) )
})

test_that( "test_CO2_scrubber_criteria", {
  ans1 <- test_CO2_scrubber_criteria( c( TRUE, FALSE, FALSE, FALSE ) )
  expect_equal( ans1, c( TRUE, FALSE, FALSE, FALSE ) )
  ans2 <- test_CO2_scrubber_criteria( c( TRUE, TRUE, FALSE, FALSE ) )
  expect_equal( ans2, c( FALSE, FALSE, TRUE, TRUE ) )
  ans3 <- test_CO2_scrubber_criteria( c( TRUE, TRUE, TRUE, FALSE ) )
  expect_equal( ans3, c( FALSE, FALSE, FALSE, TRUE ) )
})

test_that( "calc_CO2_scrubber_rating", {
  lns <-
"00100
11110
10110
10111
10101
01111
00111
11100
10000
11001
00010
01010"
  ans <- (  scan( text = lns, what = "character", quiet = TRUE )
         |> parse_lgl_columns_m()
         |> calc_CO2_scrubber_rating()
         )
  expect_equal( ans, 10L )
})

test_that( "calc_oxy_gen_rating", {
  lns <-
    "00100
11110
10110
10111
10101
01111
00111
11100
10000
11001
00010
01010"
  ans <- (  scan( text = lns, what = "character", quiet = TRUE )
            |> parse_lgl_columns_m()
            |> calc_oxy_gen_rating()
  )
  expect_equal( ans, 23L )
})
