# test_adventofcode_2020.R

library(testthat)
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
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

# Day 1 ----

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

# Day 2 ----

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

# Day 3 ----

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

test_that( "count_toboggan_trees", {
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

# Day 4 ----

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

# Day 5 ----

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

# Day 6 ----

test_that( "", {
  sample_dta <- readLines( textConnection(
"abc

a
b
c

ab
ac

a
a
a
a

b
"
  ))
  result <- parse_multiline_recs( sample_dta )
  expect_true( is.list( result ) )
  expect_equal( length( result ), 5 )
  expect_equal( result[[ 1 ]], "abc" )
  expect_equal( result[[ 3 ]], c( "ab", "ac" ) )
  expect_equal( result[[ 5 ]], "b" )
})

test_that( "normalize_rec_answers", {
  result <- normalize_rec_answers( c( "ab", "ac" ) )
  expect_equal( result, c( "a", "b", "c" ) )
  result <- normalize_rec_answers( "abc" )
  expect_equal( result, c( "a", "b", "c" ) )
  result <- normalize_rec_answers( "abc", FUN = intersect )
  expect_equal( result, c( "a", "b", "c" ) )
  result <- normalize_rec_answers( c( "a", "b", "c" ), FUN = intersect )
  expect_equal( result, character( 0 ) )
})

test_that( "count_rec_answers", {
  recList <- list( `0` = "abc", `1` = c("a", "b", "c")
                 , `2` = c("ab", "ac"), `3` = c("a", "a", "a", "a")
                 , `4` = "b"
                 )
  result <- count_rec_answers( recList )
  expect_equal( result, c( 3, 3, 3, 1, 1 ) )
  result2 <- count_rec_answers( recList, FUN = intersect )
  expect_equal( result2, c( 3, 0, 1, 1, 1 ) )
})

# Day 7 ----

test_that( "parse_bag_rules", {
  sample_rules <- readLines( textConnection(
"light red bags contain 1 bright white bag, 2 muted yellow bags.
dark orange bags contain 3 bright white bags, 4 muted yellow bags.
bright white bags contain 1 shiny gold bag.
muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
dark olive bags contain 3 faded blue bags, 4 dotted black bags.
vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
faded blue bags contain no other bags.
dotted black bags contain no other bags.
"))
  result <- parse_bag_rules( sample_rules )
  expect_equal( result
              , structure( list( containing_bag_color =
                                  c("light red", "light red", "dark orange"
                                   , "dark orange", "bright white"
                                   , "muted yellow", "muted yellow"
                                   , "shiny gold", "shiny gold", "dark olive"
                                   , "dark olive", "vibrant plum"
                                   , "vibrant plum", "faded blue"
                                   , "dotted black")
                               , cbnum = c("1", "2", "3", "4", "1", "2", "9"
                                          , "1", "2", "3", "4", "5", "6"
                                          , "no", "no"
                                          )
                               , cbcolor = c("bright white", "muted yellow"
                                            , "bright white", "muted yellow"
                                            , "shiny gold", "shiny gold"
                                            , "faded blue", "dark olive"
                                            , "vibrant plum", "faded blue"
                                            , "dotted black", "faded blue"
                                            , "dotted black", "other", "other"
                                            )
                               )
                         , row.names = c(NA, -15L)
                         , class = c("tbl_df", "tbl", "data.frame")
                         )
              )
})

test_that( "count_colors_containing", {
  sample_rules <- readLines( textConnection(
    "light red bags contain 1 bright white bag, 2 muted yellow bags.
dark orange bags contain 3 bright white bags, 4 muted yellow bags.
bright white bags contain 1 shiny gold bag.
muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
dark olive bags contain 3 faded blue bags, 4 dotted black bags.
vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
faded blue bags contain no other bags.
dotted black bags contain no other bags.
"))
  brdf <- parse_bag_rules( sample_rules )
  brmat <- make_bag_rules_matrix( brdf )
  result <- count_colors_containing( brmat, "shiny gold" )
  expect_equal( result, 4L )
})

test_that( "count_required_bags", {
  sample_rules <- readLines( textConnection(
    "shiny gold bags contain 2 dark red bags.
dark red bags contain 2 dark orange bags.
dark orange bags contain 2 dark yellow bags.
dark yellow bags contain 2 dark green bags.
dark green bags contain 2 dark blue bags.
dark blue bags contain 2 dark violet bags.
dark violet bags contain no other bags.
"))
  brdf <- parse_bag_rules( sample_rules )
  brmat <- make_bag_rules_matrix( brdf )
  result <- count_required_bags( brmat, "shiny gold" )
  expect_equal( result, 126L )
})

# Day 8 ----

test_that( "run_code_8a", {
  sample_pgm <- readLines( textConnection(
"nop +0
acc +1
jmp +4
acc +3
jmp -3
acc -99
acc +1
jmp -4
acc +6"
))
  code <- parse_pgm_8a( sample_pgm )
  expect_s3_class( code, "data.frame" )
  expect_named( code, c( "op", "arg1" ) )
  result <- run_code_8a( code )
})

test_that( "run_code_8b", {
  sample_pgm <- readLines( textConnection(
"nop +0
acc +1
jmp +4
acc +3
jmp -3
acc -99
acc +1
jmp -4
acc +6"
  ))
  code <- parse_pgm_8a( sample_pgm )
  expect_s3_class( code, "data.frame" )
  expect_named( code, c( "op", "arg1" ) )
  result <- run_code_8b( code )
  expect_named( result, c( "last_acc", "finished", "marks" ) )
  expect_false( result$finished )
  expect_equal( result$marks
              , c( 1, 2, 3, 6, 7, NA, 4, 5, NA )
              )
})

test_that( "run_code_8b", {
  sample_pgm <- readLines( textConnection(
    "nop +0
acc +1
jmp +4
acc +3
jmp -3
acc -99
acc +1
jmp -4
acc +6"
  ))
  code <- parse_pgm_8a( sample_pgm )
  expect_s3_class( code, "data.frame" )
  expect_named( code, c( "op", "arg1" ) )
  result <- alter_nopjmps( code )
  expect_named( result, c( "op", "arg1", "marks", "finished", "last_acc" ) )
  expect_equal( result$finished
              , c( FALSE, NA, FALSE, NA, FALSE, NA, NA, TRUE, NA )
  )
})

# Day 9 ----

test_that( "check_sum_validity", {
  sample_dta <- scan( text =
"35
20
15
25
47
40
62
55
65
95
102
117
150
182
127
219
299
277
309
576", quiet = TRUE )
  result <- check_sum_validity( sample_dta, 5L )
  expect_equal( result, 15L )
})

test_that( "", {
  sample_dta <- scan( text =
"35
20
15
25
47
40
62
55
65
95
102
117
150
182
127
219
299
277
309
576", quiet = TRUE )
  result <- find_encryption_weakness( sample_dta, 15L )
  expect_equal( result, c( 3, 6 ) )
})

# Day 10 ----

test_that( "get_joltdiff_table", {
  sample_dta <- scan( textConnection(
"16
10
15
5
1
11
7
19
6
12
4" ), quiet = TRUE )
  ans <- get_joltdiff_table( sample_dta )
  result <- as.vector( ans[ "1" ] * ans[ "3" ] )
  expect_equal( result, 35L )
  
  sample_dta2 <- scan( textConnection(
"28
33
18
42
31
14
46
20
48
47
24
23
49
45
19
38
39
11
1
32
25
35
8
17
7
9
4
2
34
10
3"), quiet = TRUE )
  ans <- get_joltdiff_table( sample_dta2 )
  result2 <- as.vector( ans[ "1" ] * ans[ "3" ] )
  expect_equal( result2, 220L )
})


test_that( "count_joltdiff_combos", {
  sample_dta <- scan( textConnection(
    "16
10
15
5
1
11
7
19
6
12
4" ), quiet = TRUE )
  result <- count_joltdiff_combos_v( sample_dta )
  expect_equal( prod( result ), 8L )
  
  sample_dta2 <- scan( textConnection(
    "28
33
18
42
31
14
46
20
48
47
24
23
49
45
19
38
39
11
1
32
25
35
8
17
7
9
4
2
34
10
3"), quiet = TRUE )
  result <- count_joltdiff_combos_v( sample_dta2 )
  expect_equal( prod( result ), 19208L )
})

# Day 11 ----

test_that( "parse_seat_map", {
  lns <- readLines( textConnection(
"L.LL.LL.LL
LLLLLLL.LL
L.L.L..L..
LLLL.LL.LL
L.LL.LL.LL
L.LLLLL.LL
..L.L.....
LLLLLLLLLL
L.LLLLLL.L
L.LLLLL.LL"))
  seatmap <- parse_seat_map( lns )
#  expect_named( seatmap, c( "trans", "map" ) )
  expect_true( inherits( seatmap, "matrix" ) )
  expect_equal( dim( seatmap ), c( 12L, 12L ) )
})

test_that( "reseat_seatmap", {
  lns <- readLines( textConnection(
    "L.LL.LL.LL
LLLLLLL.LL
L.L.L..L..
LLLL.LL.LL
L.LL.LL.LL
L.LLLLL.LL
..L.L.....
LLLLLLLLLL
L.LLLLLL.L
L.LLLLL.LL"))
  seatmap <- parse_seat_map( lns )
  result <- seatmap_table( reseat_seatmap( seatmap ) )
  expect_equal( result[ "#" ], c( `#` = 71 ) )
})

test_that( "seatmap_steady_state", {
  lns <- readLines( textConnection(
"L.LL.LL.LL
LLLLLLL.LL
L.L.L..L..
LLLL.LL.LL
L.LL.LL.LL
L.LLLLL.LL
..L.L.....
LLLLLLLLLL
L.LLLLLL.L
L.LLLLL.LL"))
  seatmap <- parse_seat_map( lns )
  result <- seatmap_table( seatmap_steady_state(seatmap)$seatmap )
  expect_equal( unname( result[ "#" ] ), 37 )
})

test_that( "reseat_seatmap_11b", {
  lns <- readLines( textConnection(
"L.LL.LL.LL
LLLLLLL.LL
L.L.L..L..
LLLL.LL.LL
L.LL.LL.LL
L.LLLLL.LL
..L.L.....
LLLLLLLLLL
L.LLLLLL.L
L.LLLLL.LL"))

res1 <- parse_seat_map( readLines( textConnection(
"#.##.##.##
#######.##
#.#.#..#..
####.##.##
#.##.##.##
#.#####.##
..#.#.....
##########
#.######.#
#.#####.##"
)))
res2 <- parse_seat_map( readLines( textConnection(
"#.LL.LL.L#
#LLLLLL.LL
L.L.L..L..
LLLL.LL.LL
L.LL.LL.LL
L.LLLLL.LL
..L.L.....
LLLLLLLLL#
#.LLLLLL.L
#.LLLLL.L#"
)))

  seatmap <- parse_seat_map( lns )
  adjmap <- find_seatmap_visible_seats( seatmap = seatmap )
  result <- reseat_seatmap_11b( seatmap, adjmap = adjmap )
  expect_equal( result, res1 )
  result2 <- reseat_seatmap_11b( result, adjmap = adjmap )
  expect_equal( result2, res2 )
})

test_that( "find_seatmap_first_visible", {
  lns <- readLines( textConnection(
"L.LL.LL.LL
LLLLLLL.LL
L.L.L..L..
LLLL.LL.LL
L.LL.LL.LL
L.LLLLL.LL
..L.L.....
LLLLLLLLLL
L.LLLLLL.L
L.LLLLL.LL"))
  seatmap <- parse_seat_map( lns )
  result <- find_seatmap_first_visible( dv = c( 1, 0 )
                                      , from = c( 2, 3 )
                                      , seatmap = seatmap
                                      )
  expect_equal( result, 27 )
  expect_equal( find_seatmap_first_visible( dv = c( 1, 0 )
                                          , from = c( 3, 3 )
                                          , seatmap = seatmap
                                          )
              , 29
              )
  expect_equal( find_seatmap_first_visible( dv = c( 1, 1 )
                                          , from = c( 9, 2 )
                                          , seatmap = seatmap
                                          )
              , 47
  )
})

test_that( "find_seatmap_visible_seats", {
  lns <- readLines( textConnection(
"L.LL.LL.LL
LLLLLLL.LL
L.L.L..L..
LLLL.LL.LL
L.LL.LL.LL
L.LLLLL.LL
..L.L.....
LLLLLLLLLL
L.LLLLLL.L
L.LLLLL.LL"))
  seatmap <- parse_seat_map( lns )
  result <- find_seatmap_visible_seats( seatmap = seatmap )
  expect_equal( result[ , 1 ]
              , c( NA, NA, NA, 15, 27, 38, NA, NA )
              )
  expect_equal( result[ , 11 ]
                , c( NA, 14, 15, 27, 39, 38, NA, NA )
  )
})

# Day 12----

test_that( "", {
  sample_traj <- readLines( textConnection(
"F10
N3
F7
R90
F11"))
  result <- parse_traj( sample_traj )
  expect_s3_class( result, "data.frame" )
  expect_equal( result$dir, c( "F", "N", "F", "R", "F" ) )
  expect_equal( result$mag, c( 10L, 3L, 7L, 90L, 11L ) )
  expect_equal( result$is_abs, c( FALSE, TRUE, FALSE, FALSE, FALSE ) )
  expect_equal( result$ang, c( 0L, 1L, 0L, 3L, 0L ) )
})

test_that( "traverse_12a", {
  sample_traj <- readLines( textConnection(
"F10
N3
F7
R90
F11" ) )
  traj <- parse_traj( sample_traj )
  result <- traverse_12a( traj )
  expect_s3_class( result, "data.frame" )
  expect_equal( result$pos_x
              , c( 10, 10, 17, 17, 17 )
              )
  expect_equal( result$pos_y
              , c( 0, 3, 3, 3, -8 )
              )
  expect_equal( result$pos_ang
              , c( 0L, 0L, 0L, 3L, 3L )
              )
  sample_traj2 <- readLines( textConnection(
"F10
L180
R270" ) )
  traj2 <- parse_traj( sample_traj2 )
  result2 <- traverse_12a( traj2 )
  expect_equal( result2$pos_ang
              , c( 0, 2, 3 )
              )
})

test_that( "get_rot90_matrix", {
  expect_equal( get_rot90_matrix( "L", 0 )
              , matrix( c( 1, 0, 0, 1 ), nrow = 2L )
              )
  expect_equal( get_rot90_matrix( "R", 0 )
              , matrix( c( 1, 0, 0, 1 ), nrow = 2L )
              )
  expect_equal( get_rot90_matrix( "L", 90 )
              , matrix( c( 0, 1, -1, 0 ), nrow = 2L )
              )
  expect_equal( get_rot90_matrix( "R", 90 )
              , matrix( c( 0, -1, 1, 0 ), nrow = 2L )
              )
  expect_equal( get_rot90_matrix( "L", 180 )
              , matrix( c( -1, 0, 0, -1 ), nrow = 2L )
              )
  expect_equal( get_rot90_matrix( "R", 180 )
              , matrix( c( -1, 0, 0, -1 ), nrow = 2L )
              )
  expect_equal( get_rot90_matrix( "L", 270 )
              , matrix( c( 0, -1, 1, 0 ), nrow = 2L )
              )
  expect_equal( get_rot90_matrix( "R", 270 )
              , matrix( c( 0, 1, -1, 0 ), nrow = 2L )
              )
})

test_that( "traverse_12b", {
  sample_traj <- readLines( textConnection(
"F10
N3
F7
R90
F11" ) )
  traj <- parse_traj( sample_traj )
  result <- traverse_12b( traj )
  expect_s3_class( result, "data.frame" )
  expect_equal( result$pos_x
              , c( 100, 100, 170, 170, 214 )
              )
  expect_equal( result$pos_y
              , c( 10, 10, 38, 38, -72 )
              )
  expect_equal( result$wp_x
              , c( 10L, 10L, 10L, 4L, 4L )
              )
  expect_equal( result$wp_y
              , c( 1L, 4L, 4L, -10L, -10L )
              )
  sample_traj2 <- readLines( textConnection(
"F10
L180
R270" ) )
  traj2 <- parse_traj( sample_traj2 )
  result2 <- traverse_12b( traj2 )
  expect_equal( result2$wp_x
              , c( 10, -10, 1 )
              )
  expect_equal( result2$wp_y
              , c( 1, -1, -10 )
              )
})
