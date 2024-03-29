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

# Day 6 ----

test_that( "parse_lanternfish", {
  s <- "3,4,3,1,2"
  ans <- parse_lanternfish( s )
  expect_s3_class( ans, "data.frame" )
  expect_named( ans, c( "State", "Count" ) )
  expect_equal( ans$State, as.character( 0:8 ) )
  expect_equal( ans$Count, c( 0, 1, 1, 2, 1, 0, 0, 0, 0 ) )
})

test_that( "sim_day_lanternfish", {
  s <- "3,4,3,1,2"
  day0 <- parse_lanternfish( s )
  day1 <- sim_day_lanternfish( day0 )
  expect_equal( day1$Count, parse_lanternfish( "2,3,2,0,1" )$Count )
  day2 <- sim_day_lanternfish( day1 )
  expect_equal( day2$Count, parse_lanternfish( "1,2,1,6,0,8" )$Count )
  day3 <- sim_day_lanternfish( day2 )
  expect_equal( day3$Count, parse_lanternfish( "0,1,0,5,6,7,8" )$Count )
})

test_that( "sim_days_lanternfish", {
  s <- "3,4,3,1,2"
  day0 <- parse_lanternfish( s )
  ans <- sim_days_lanternfish( day0, 80 )
  expect_equal( ans, 5934 )
})

# Day 7 ----

test_that( "find_min_crab_fuel", {
  s <- "16,1,2,0,4,2,7,1,2,14"
  v <- scan( text = s, sep = ",", quiet = TRUE )
  ans <- find_min_crab_fuel( v )
  expect_equal( ans, 37 )
})

test_that( "find_min_crab_fuel v2", {
  s <- "16,1,2,0,4,2,7,1,2,14"
  v <- scan( text = s, sep = ",", quiet = TRUE )
  ans <- find_min_crab_fuel( v, version = 2 )
  expect_equal( ans, 168 )
  v2 <- c( v, 5 )
  ans2 <- find_min_crab_fuel( v2, version = 2 )
  expect_equal( ans2, 168 )
})

# Day 8 ----

test_dta_day8a <-
"be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe
edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc
fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg
fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb
aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea
fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb
dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe
bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef
egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb
gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce"

test_that( "parse_day8a", {
  lns <- strsplit( test_dta_day8a, "\n" )[[1]]
  ans <- parse_day8a( lns )
  expect_named( ans, c( "input", "output" ) )
})

test_that( "parse_segments", {
  ans <- parse_segments( c( "ace", "bcdg" ) )
  expect_equal( ans, list( c( 1, 3, 5 ), c( 2, 3, 4, 7 ) ) )
})

test_that( "count_unique_day8a", {
  lns <- strsplit( test_dta_day8a, "\n" )[[1]]
  inpdta <- parse_day8a( lns )
  ans <- count_unique_day8a( inpdta$output )
  expect_equal( ans, 26 )
})

test_that( "calc_input_scores_day8b", {
  lns <- strsplit( test_dta_day8a, "\n" )[[1]]
  inpdta <- parse_day8a( lns )
  ans <- calc_input_scores_day8b( inpdta )
  expect_equal( length( ans ), 10L )
  expect_equal( ans[[1]]
              , structure( c( A = 4L, B = 8L, C = 7L, D = 8L
                            , E = 9L, F = 7L, G = 6L
                            )
                         , .Dim = 7L
                         , .Dimnames = structure( list( c( "A", "B", "C"
                                                         , "D", "E", "F"
                                                         , "G"
                                                         )
                                                      )
                                                , .Names = ""
                                                )
                         , class = "table"
                         )
              )
})

test_that( "calc_output_scores", {
  lns <- "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf"
  # 
  inpdta <- parse_day8a( lns )
  ans0 <- calc_input_scores_day8b( inpdta )
  # "A" segment is "wrong" so it is not the same as the "a" segment
  # this segment is used in 8 distinct digits
  # "B" segment is used in 9 distinct digits
  #      A  B C D E F G sum
  # 0    ?  ? ? ? ? ? ?   6
  # 1    ?  ? ? ? ? ? ?   2
  # 2    ?  ? ? ? ? ? ?   5
  # 3    ?  ? ? ? ? ? ?   5
  # 4    ?  ? ? ? ? ? ?   4
  # 5    ?  ? ? ? ? ? ?   5
  # 6    ?  ? ? ? ? ? ?   6
  # 7    ?  ? ? ? ? ? ?   3
  # 8    ?  ? ? ? ? ? ?   7
  # 9    ?  ? ? ? ? ? ?   6
  # each column sum has to add up to one of
  #      a  b  c  d  e  f  g
  # sum 61 42 57 45 28 63 50 
  expect_equal( ans0[[1]]
              , structure( c( A = 8L, B = 9L, C = 7L, D = 8L
                            , E = 6L, F = 7L, G = 4L
                            )
                         , .Dim = 7L
                         , .Dimnames = structure( list( c( "A", "B", "C"
                                                         , "D", "E", "F"
                                                         , "G"
                                                         )
                                                      )
                                                , .Names = ""
                                                )
                         , class = "table"
                         )
              )
  ans <- calc_output_scores( inpdta )
  expect_equal( ans, list( c( 5, 3, 5, 3 ) ) )
})
