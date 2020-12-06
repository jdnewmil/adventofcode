# test_adventofcode_2020.R

library(testthat)
suppressPackageStartupMessages({
  library(dplyr)
  library(purrr)
})

rdir <- "../R"
#rdir <- "R"
source( file.path( rdir, "adventofcode_2020.R" ) )

test_data_day1 <- c( 1721, 979, 366, 299, 675, 1456 )
test_data_day2 <- c( "1-3 a: abcde"
                   , "1-3 b: cdefg"
                   , "2-9 c: ccccccccc"
                   )

test_that( "find_vec_pairsum", {
  result <- find_vec_pairsum( test_data_day1, 2020 )
  expect_equal( result, c( 1, 4 ) )
  result <- find_vec_pairsum( test_data_day1, 2021 )
  expect_equal( result, integer( 0 ) )
})

test_that( "find_vec_nsum", {
  result <- find_vec_nsum( test_data_day1, 2020, 3L )
  expect_equal( result, c( 2, 3, 5 ) )
  result <- find_vec_pairsum( test_data_day1, 2021 )
  expect_equal( result, integer( 0 ) )
})

test_that( "parse_pwd2a", {
  result <- parse_pwd2a( test_data_day2 )
  expect_s3_class( result, "data.frame" )
  expect_named( result, c( "ixlo", "ixhi", "ltr", "pwd" ) )
  expect_equal( result$ixlo, c( 1, 1, 2 ) )
  expect_equal( result$ixhi, c( 3, 3, 9 ) )
  expect_equal( result$ltr, c( "a", "b", "c" ) )
  expect_equal( result$pwd, c( "abcde", "cdefg", "ccccccccc" ) )
})

test_that( "chk_pwd2a", {
  result <- chk_pwd2a( ixlo = 1, ixhi = 3, ltr = "a", pwd = "abcde" )
  expect_true( result )
  result <- chk_pwd2a( ixlo = 1, ixhi = 3, ltr = "b", pwd = "cdefg" )
  expect_false( result )
})

test_that( "chk_pwd2b", {
  expect_true( chk_pwd2b( ixlo = 1, ixhi = 3, ltr = "a", pwd = "abcde" ) )
  expect_false( chk_pwd2b( ixlo = 1, ixhi = 3, ltr = "b", pwd = "cdefg" ) )
  expect_false( chk_pwd2b( ixlo = 1, ixhi = 3, ltr = "c", pwd = "ccccccccc" ) )
})

test_that( "parse_forest", {
  fd <- c( "..##......."
         , "#...#...#.."
         , ".#....#..#."
         , "..#.#...#.#"
         , ".#...##..#."
         , "..#.##....."
         , ".#.#.#....#"
         , ".#........#"
         , "#.##...#..."
         , "#...##....#"
         , ".#..#...#.#"
         )
  result <- parse_forest( fd )
  expect_equal( typeof( result ), "logical" )
  expect_equal( nrow( result ), 11L )
  expect_equal( ncol( result ), 11L )
  expect_equal( result[ , 1 ], c( FALSE, TRUE, FALSE, FALSE, FALSE, FALSE
                                , FALSE, FALSE, TRUE, TRUE, FALSE
                                )
              )
})

test_that( "generate_trajectory", {
  result <- generate_trajectory( row_delta = 1L
                               , col_delta = 3L
                               , row_max = 4L
                               , col_max = 4L
                               )
  expect_equal( result
              , structure( c( 1, 2, 3, 4
                            , 1, 4, 3, 2
                            )
                         , .Dim = c(4L, 2L)
                         )
              )
})

test_that( "count_t", {
  fd <- c( "..##......."
           , "#...#...#.."
           , ".#....#..#."
           , "..#.#...#.#"
           , ".#...##..#."
           , "..#.##....."
           , ".#.#.#....#"
           , ".#........#"
           , "#.##...#..."
           , "#...##....#"
           , ".#..#...#.#"
  )
  tree_map <- parse_forest( fd )
  result <- count_toboggan_trees( tree_map, row_delta = 1L, col_delta = 3L )
  expect_equal( result, 7L )
})

test_that( "parse_ppdata", {
  lns <- c( "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd"
          , "byr:1937 iyr:2017 cid:147 hgt:183cm"
          , ""
          , "iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884"
          , "hcl:#cfa07d byr:1929"
          , ""
          , "hcl:#ae17e1 iyr:2013"
          , "eyr:2024"
          , "ecl:brn pid:760753108 byr:1931"
          , "hgt:179cm"
          , ""
          , "hcl:#cfa07d eyr:2025 pid:166559648"
          , "iyr:2011 ecl:brn hgt:59in"
          )
  result <- parse_ppdata( lns )
  expect_s3_class( result, "data.frame" )
  expect_named( result
              , c( "ecl", "pid", "eyr", "hcl"
                 , "byr", "iyr", "cid", "hgt" )
              )
  expect_equal( nrow( result ), 4L )
})

test_that( "parse_pprec", {
  s <- c( "ecl:0001", "any:xxx" )
  result <- parse_pprec( s )
  expect_s3_class( result, "data.frame" )
  expect_named( result, c( "ecl", "any" ) )
  expect_equal( result$ecl, "0001" )
  expect_equal( result$any, "xxx" )
})

test_that( "validate_ppdata", {
  lns <- c( "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd"
          , "byr:1937 iyr:2017 cid:147 hgt:183cm"
          , ""
          , "iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884"
          , "hcl:#cfa07d byr:1929"
          , ""
          , "hcl:#ae17e1 iyr:2013"
          , "eyr:2024"
          , "ecl:brn pid:760753108 byr:1931"
          , "hgt:179cm"
          , ""
          , "hcl:#cfa07d eyr:2025 pid:166559648"
          , "iyr:2011 ecl:brn hgt:59in"
          )
  ppdata <- parse_ppdata( lns )
  result <- validate_ppdata( ppdata )
  expect_equal( result, c( TRUE, FALSE, TRUE, FALSE ) )
  lns2 <- c( "eyr:1972 cid:100"
           , "hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926"
           , ""
           , "iyr:2019"
           , "hcl:#602927 eyr:1967 hgt:170cm"
           , "ecl:grn pid:012533040 byr:1946"
           , ""
           , "hcl:dab227 iyr:2012"
           , "ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277"
           , ""
           , "hgt:59cm ecl:zzz"
           , "eyr:2038 hcl:74454a iyr:2023"
           , "pid:3556412378 byr:2007"
           )
  ppdata2 <- parse_ppdata( lns2 )
  result2 <- validate_ppdata( ppdata2, enhanced = "4b" )
  expect_equal( result2, c( FALSE, FALSE, FALSE, FALSE ) )
  lns3 <- c( "pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980"
           , "hcl:#623a2f"
           , ""
           , "eyr:2029 ecl:blu cid:129 byr:1989"
           , "iyr:2014 pid:896056539 hcl:#a97842 hgt:165cm"
           , ""
           , "hcl:#888785"
           , "hgt:164cm byr:2001 iyr:2015 cid:88"
           , "pid:545766238 ecl:hzl"
           , "eyr:2022"
           , ""
           , "iyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719"
           )
  ppdata3 <- parse_ppdata( lns3 )
  result3 <- validate_ppdata( ppdata3, enhanced = "4b" )
  expect_equal( result3, c( TRUE, TRUE, TRUE, TRUE ) )
})

test_that( "decode_bp_to_int", {
  bd_dta <- read.table( text =
"code      row  col  seat_id
BFFFBBFRRR  70    7      567
FFFBBBFRRR  14    7      119
BBFFBBFRLL 102    4      820
", header = TRUE, as.is = TRUE )
  result <- decode_bp_to_int( s = bd_dta$code )
  expect_equal( result, bd_dta$seat_id )
  expect_equal( seat_id_to_row( result ), bd_dta$row )
  expect_equal( seat_id_to_col( result ), bd_dta$col )
})

