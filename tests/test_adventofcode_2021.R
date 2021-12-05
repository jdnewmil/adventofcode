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

# Day 4 ----

test_day4a <-
"7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1

22 13 17 11  0
 8  2 23  4 24
21  9 14 16  7
 6 10  3 18  5
 1 12 20 15 19

 3 15  0  2 22
 9 18 13 17  5
19  8  7 25 23
20 11 10 24  4
14 21 16 12  6

14 21 17 24  4
10 16 15  9 19
18  8 23 26 20
22 11 13  6  5
 2  0 12  3  7
"

test_that( "read_day4a", {
  lns <- strsplit( test_day4a, "\n", fixed = TRUE )[[1]]
  ans <- read_day4a( lns )
  expect_equal( ans$draws, c(  7, 4, 9, 5, 11, 17, 23, 2, 0, 14, 21, 24, 10
                            , 16, 13, 6, 15, 25, 12, 22, 18, 20, 8, 19, 3
                            , 26, 1) )
  expect_equal( length( ans$boards ), 3L )
})

test_that( "mark_a_card", {
  bd <- structure( c( 22L, 8L, 21L, 6L, 1L, 13L, 2L, 9L, 10L, 12L, 17L
                    , 23L, 14L, 3L, 20L, 11L, 4L, 16L, 18L, 15L, 0L
                    , 24L, 7L, 5L, 19L
                    )
                 , .Dim = c(5L, 5L)
                 , .Dimnames = list( NULL
                                   , c("V1", "V2", "V3", "V4", "V5"))
                 )
  card <- matrix( c( rep( FALSE, 10L )
                   , TRUE
                   , rep( FALSE, 14L )
                   )
                , ncol = 5
                )
  refcard <- matrix( c( rep( FALSE, 10L )
                      , TRUE
                      , rep( FALSE, 12L )
                      , TRUE
                      , FALSE
                      )
                   , ncol = 5L
                   )
  ans <- mark_a_card( card, bd, 5L )
  expect_equal( ans, refcard )
  ans2 <- mark_a_card( card, bd, 555L )
  expect_equal( ans2, card )
})

test_that( "is_card_winning", {
  card <- matrix( c( rep( FALSE, 10L )
                   , TRUE
                   , rep( FALSE, 9L )
                   , rep( TRUE, 5L )
                   )
                , ncol = 5
                )
  ans <- is_card_winning( card )
  expect_equal( ans, TRUE )
  card[ 5, 5 ] <- FALSE
  ans2 <- is_card_winning( card )
  expect_equal( ans2, FALSE )
})

test_that( "play_draws", {
  lns <- strsplit( test_day4a, "\n", fixed = TRUE )[[1]]
  inp <- read_day4a( lns )
  ans <- play_draws( inp$boards, inp$draws )
  expect_equal( ans, 4512L )
})

test_that( "play_draws_last", {
  lns <- strsplit( test_day4a, "\n", fixed = TRUE )[[1]]
  inp <- read_day4a( lns )
  ans <- play_draws_last( inp$boards, inp$draws )
  expect_equal( ans, 1924L )
})


# Day 5 ----

test_that( "parse_day5a", {
  s <- 
"0,9 -> 5,9
8,0 -> 0,8
9,4 -> 3,4
2,2 -> 2,1
7,0 -> 7,4
6,4 -> 2,0
0,9 -> 2,9
3,4 -> 1,4
0,0 -> 8,8
5,5 -> 8,2
"
  lns <- strsplit( s, "\n", fixed = TRUE )[[1]]
  ans <- parse_day5a( lns )
  expect_s3_class( ans, "data.frame" )
  expect_length( ans, 4 )
  expect_named( ans, c( "x1", "y1", "x2", "y2" ) )
})

test_that( "build_map_day5a", {
  s <- 
"0,9 -> 5,9
8,0 -> 0,8
9,4 -> 3,4
2,2 -> 2,1
7,0 -> 7,4
6,4 -> 2,0
0,9 -> 2,9
3,4 -> 1,4
0,0 -> 8,8
5,5 -> 8,2
"
  lns <- strsplit( s, "\n", fixed = TRUE )[[1]]
  dta <- parse_day5a( lns )
  ans <- build_map_day5a( dta )
  expect_equal( ans
              , structure(c( 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 0, 1, 0, 0
                           , 0, 0, 2, 0, 1, 1, 0, 1, 0, 0, 0, 0, 2, 0, 0, 0, 0
                           , 2, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0
                           , 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0
                           , 0, 0, 1, 1, 1, 1, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1
                           , 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0
                           )
                         , .Dim = c(10L, 10L)
                         , .Dimnames = list( c( "0", "1", "2", "3", "4", "5"
                                              , "6", "7", "8", "9")
                                           , c( "0", "1", "2", "3", "4", "5"
                                              , "6", "7", "8", "9" )
                                           )
                         )
              )
})

test_that( "count_danger_day5a", {
  s <- 
"0,9 -> 5,9
8,0 -> 0,8
9,4 -> 3,4
2,2 -> 2,1
7,0 -> 7,4
6,4 -> 2,0
0,9 -> 2,9
3,4 -> 1,4
0,0 -> 8,8
5,5 -> 8,2
"
  lns <- strsplit( s, "\n", fixed = TRUE )[[1]]
  dta <- parse_day5a( lns )
  map <- build_map_day5a( dta )
  ans <- count_danger_day5a( map )
  expect_equal( ans, 5 )
})

test_that( "count_danger_day5b", {
  s <- 
"0,9 -> 5,9
8,0 -> 0,8
9,4 -> 3,4
2,2 -> 2,1
7,0 -> 7,4
6,4 -> 2,0
0,9 -> 2,9
3,4 -> 1,4
0,0 -> 8,8
5,5 -> 8,2
"
  lns <- strsplit( s, "\n", fixed = TRUE )[[1]]
  dta <- parse_day5a( lns )
  map <- build_map_day5b( dta )
  ans <- count_danger_day5a( map )
  expect_equal( ans, 12L )
})

test_that( "build_ixy_df", {
  ans1 <- build_ixy_df( 0, 0, 8, 2 )
  expect_equal( ans1
              , structure( list( iy = integer(0)
                               , ix = integer(0)
                               )
                         , class = "data.frame"
                         , row.names = integer(0)
                         )
              )
  ans2 <- build_ixy_df( 0, 0, 8, 8 )
  expect_equal( ans2
              , structure( list( iy = 0:8
                               , ix = 0:8
                               )
                         , class = "data.frame"
                         , row.names = c(NA, -9L)
                         )
              )
  ans3 <- build_ixy_df( 0, 9, 5, 9 )
  expect_equal( ans3
              , structure( list( iy = c(9, 9, 9, 9, 9, 9)
                               , ix = 0:5
                               )
                         , class = "data.frame"
                         , row.names = c(NA, -6L)
                         )
              )
  ans4 <- build_ixy_df( 7, 0, 7, 4 )
  expect_equal( ans4
              , structure( list( iy = 0:4
                               , ix = c(7, 7, 7, 7, 7)
                               )
                         , class = "data.frame"
                         , row.names = c(NA, -5L)
                         )
              )
})

test_that( "count_danger_day5b", {
  s <- 
"0,9 -> 5,9
8,0 -> 0,8
9,4 -> 3,4
2,2 -> 2,1
7,0 -> 7,4
6,4 -> 2,0
0,9 -> 2,9
3,4 -> 1,4
0,0 -> 8,8
5,5 -> 8,2
"
  lns <- strsplit( s, "\n", fixed = TRUE )[[1]]
  dta <- parse_day5a( lns )
  ans <- count_danger_day5b_df( dta )
  expect_equal( ans, 12L )
})
