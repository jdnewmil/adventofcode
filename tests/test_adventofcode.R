# test_adventofcode.R

library(testthat)

rdir <- "../R"
source( file.path( rdir, "adventofcode.R" ) )

dta_day3 <- list( A = c( "R75,D30,R83,U83,L12,D49,R71,U7,L72"
                       , "U62,R66,U55,R34,D71,R55,D58,R83"
                       )
                , B = c( "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51"
                       , "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7" )
                , C = c( "R8,U5,L5,D3"
                       , "U7,R6,D4,L4"
                       )
                )

test_that( "calc_fuel", {
  m <- c( 12, 14, 1969, 100756 )
  expect_equal( calc_fuel( m ), c( 2, 2, 654, 33583 ) )
})

test_that( "calc_fuel2", {
  m <- c( 12, 14, 1969, 100756 )
  expect_equal( calc_fuel2( m ), c( 2, 2, 966, 50346 ) )
})

test_that( "intcode1", {
  v1 <- c( 1, 0, 0, 0, 99 )
  result1 <- intcode1( v1 )
  expect_equal( result1$pgm, c( 2, 0, 0, 0, 99 ))
  v2 <- c( 2, 3, 0, 3, 99 )
  result2 <- intcode1( v2 )
  expect_equal( result2$pgm, c( 2, 3, 0, 6, 99 ))
  v3 <- c( 2, 4, 4, 5, 99, 0 )
  result3 <- intcode1( v3 )
  expect_equal( result3$pgm, c( 2, 4, 4, 5, 99, 9801 ))
  v4 <- c( 1, 1, 1, 4, 99, 5, 6, 0, 99 )
  result4 <- intcode1( v4 )
  expect_equal( result4$pgm, c( 30, 1, 1, 4, 2, 5, 6, 0, 99 ) )
})

test_that( "f_day2_part2", {
  pgm_day2 <- c( 1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,6,1,19,1,5,19,23,2,9,23,27,1,6,27,31,1,31,9,35,2,35,10,39,1,5,39,43,2,43,9,47,1,5,47,51,1,51,5,55,1,55,9,59,2,59,13,63,1,63,9,67,1,9,67,71,2,71,10,75,1,75,6,79,2,10,79,83,1,5,83,87,2,87,10,91,1,91,5,95,1,6,95,99,2,99,13,103,1,103,6,107,1,107,5,111,2,6,111,115,1,115,13,119,1,119,2,123,1,5,123,0,99,2,0,14,0 )
  expect_equal( f_day2_part2( 12, 2, pgm_day2 ), 3101844 )
})

test_that( "parse_day3", {
  input1 <- "R75,D30,R83,U83,L12,D49,R71,U7,L72,U62,R66,U55,R34,D71,R55,D58,R83"
  expect_equal( parse_day3( input1 )
              , structure( c( 75L, 0L, 83L, 0L, -12L, 0L, 71L, 0L, -72L, 0L, 66L
                            , 0L, 34L, 0L, 55L, 0L, 83L, 0L, -30L, 0L, 83L, 0L
                            , -49L, 0L, 7L, 0L, 62L, 0L, 55L, 0L, -71L, 0L
                            , -58L, 0L
                            )
                         , .Dim = c( 17L, 2L )
                         , .Dimnames = list( NULL, c( "V1", "V2" ) ) )
              )
})

test_that( "xprod", {
  A1 <- c( 0, 10 )
  B1 <- c( 5, 0 )
  expect_equal( xprod( A1, B1 ), -50 )
})

test_that( "calc_intersection", {
  A1 <- c( 0, 10 )
  C1 <- c( 5, 0 )
  AB1 <- c( 20, 0 )
  CD1 <- c( 0, 10 )
  expect_equal( calc_intersection( A1, C1, AB1, CD1 ), c( 5, 10 ) )
  AB2 <- c( 2, 0 )
  expect_equal( calc_intersection( A1, C1, AB2, CD1 ), c( NaN, Inf ) )
  CD2 <- c( 0, 2 )
  expect_equal( calc_intersection( A1, C1, AB1, CD2 ), c( NaN, Inf ) )
})

test_that( "f_day3_part1", {
  expect_equal( f_day3_part1( dta_day3$A[1], dta_day3$A[2] ), 159 )
  expect_equal( f_day3_part1( dta_day3$B[1], dta_day3$B[2] ), 135 )
})

test_that( "f_day3_part1_impl", {
  result <- f_day3_part1_impl( dta_day3$A[1], dta_day3$A[2] )
  expect_equal( result$inp1
              , structure( c( 75L, 0L, 83L, 0L, -12L, 0L, 71L, 0L, -72L, 0L
                            , -30L, 0L, 83L, 0L, -49L, 0L, 7L, 0L
                            )
                         , .Dim = c(9L, 2L)
                         , .Dimnames = list( NULL, c("V1", "V2") )
                         )
              )
  expect_equal( result$inp2
              , structure( c( 0L, 66L, 0L, 34L, 0L, 55L, 0L, 83L, 62L, 0L
                            , 55L, 0L, -71L, 0L, -58L, 0L
                            )
                         , .Dim = c(8L, 2L)
                         , .Dimnames = list( NULL, c("V1", "V2") )
                         )
              )
  expect_equal( result$len1
              , c( 0, 75, 105, 188, 271, 283, 332, 403, 410, 482 )
              )
  expect_equal( result$len2
              , c( 0, 62, 128, 183, 217, 288, 343, 401, 484 )
              )
  expect_equal( result$pos1
              , structure( c( 0, 75, 75, 158, 158, 146, 146, 217, 217
                            , 145, 0, 0, -30, -30, 53, 53, 4, 4, 11, 11
                            )
                         , .Dim = c( 10L, 2L )
                         , .Dimnames = list( NULL, c( "X", "Y" ) )
                         )
              )
  expect_equal( result$pos2
              , structure( c( 0, 0, 66, 66, 100, 100, 155, 155, 238, 0
                            , 62, 62, 117, 117, 46, 46, -12, -12
                            )
                         , .Dim = c(9L, 2L)
                         , .Dimnames = list( NULL, c("X", "Y") )
                         )
              )
  expect_equal( result$min_md, 159 )
  expect_equal( result$pts
              , structure( list( i = c( 6L, 7L, 9L, 4L)
                               , j = c( 6L, 7L, 7L, 8L )
                               , x = c( 146, 155, 155, 158 )
                               , y = c( 46, 4, 11, -12 )
                               , md = c( 192, 159, 166, 146 )
                               )
                         , row.names = c( 51L, 61L, 63L, 67L )
                         , class = "data.frame"
                         )
              )
})

test_that( "f_day3_part2", {
  expect_equal( f_day3_part2( dta_day3$A[1], dta_day3$A[2] ), 610 )
  expect_equal( f_day3_part2( dta_day3$B[1], dta_day3$B[2] ), 410 )
})

# v <- dta_day3$A
# f_day3_part2( v[ 1 ], v[ 2 ] )
# impl <- f_day3_part1_impl( v[ 1 ], v[ 2 ] )
# plot_day3_part1( impl )
# impl$pos1
# impl$len1

test_that( "test_day4_part1", {
  test_day4p1_values <- c( 111111L, 223450L, 123789L )
  expect_true( test_day4_part1( test_day4p1_values[ 1 ] ) )
  expect_false( test_day4_part1( test_day4p1_values[ 2 ] ) )
  expect_false( test_day4_part1( test_day4p1_values[ 3 ] ) )
})

test_that( "test_day4_part2", {
  test_day4p1_values <- c( 112233L, 123444L, 111122L )
  expect_true( test_day4_part2( test_day4p1_values[ 1 ] ) )
  expect_false( test_day4_part2( test_day4p1_values[ 2 ] ) )
  expect_true( test_day4_part2( test_day4p1_values[ 3 ] ) )
})

test_that( "opcode 3 and 4", {
  test_day5_pgm <- c( 3, 0, 4, 0, 99 )
  result <- intcode1( test_day5_pgm, 7, integer( 0L ) )
  expect_equal( result$input, numeric( 0 ) )
  expect_equal( result$output, 7 )
})

test_that( "parameter mode immediate", {
  test_day5_pgm_imm <- c( 3, 0, 102, 3, 0, 1, 4, 1, 99 )
  result <- intcode1( test_day5_pgm_imm, 7, integer( 0L ) )
  expect_equal( result$input, numeric( 0 ) )
  expect_equal( result$output, 21 )
})

test_that( "opcode_equals", {
  t1 <- c( 3, 9, 8, 9, 10, 9, 4, 9, 99, -1, 8 )
  expect_equal( intcode1_pipe( t1, 1L ), 0L )
  expect_equal( intcode1_pipe( t1, 8L ), 1L )
  t2 <- c( 3,3,1108,-1,8,3,4,3,99 )
  expect_equal( intcode1_pipe( t1, 1L ), 0L )
  expect_equal( intcode1_pipe( t1, 8L ), 1L )
})

test_that( "opcode_less_than", {
  t1 <- c( 3,9,7,9,10,9,4,9,99,-1,8 )
  expect_equal( intcode1_pipe( t1, 1L ), 1L )
  expect_equal( intcode1_pipe( t1, 8L ), 0L )
  t2 <- c( 3,3,1107,-1,8,3,4,3,99 )
  expect_equal( intcode1_pipe( t1, 1L ), 1L )
  expect_equal( intcode1_pipe( t1, 8L ), 0L )
})

test_that( "opcode_jump", {
  t1 <- c( 3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9 )
  expect_equal( intcode1_pipe( t1, 0L ), 0L )
  expect_equal( intcode1_pipe( t1, 8L ), 1L )
  t2 <- c( 3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31
         , 1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104
         , 999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99
         )
  expect_equal( intcode1_pipe( t2, -2L ), 999L )
  expect_equal( intcode1_pipe( t2, 8L ), 1000L )
  expect_equal( intcode1_pipe( t2, 12L ), 1001L )
})

