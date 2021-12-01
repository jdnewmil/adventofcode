# adventofcode_2021.R

# Day 1 ----

count_increases <- function( x ) {
  sum( 1 == ( 0 < diff( x ) ) )
}
