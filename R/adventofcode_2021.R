# adventofcode_2021.R

# Day 1 ----

count_increases <- function( x ) {
  sum( 1 == ( 0 < diff( x ) ) )
}

# Day 2 ----

parse_subfile <- function( text ) {
  read.table( text = text ) |> setNames( c( "dir", "dist" ) )
}

augment_subfile_day2 <- function( sf_df ) {
  sf_df <- merge( sf_df, data.frame( dir = c( "forward", "down", "up" )
                                   , dir_x = c( 1, 0, 0 )
                                   , dir_y = c( 0, 1, -1 )
                                   ))
  sf_df$dir_x <- with( sf_df, dir_x * dist )
  sf_df$dir_y <- with( sf_df, dir_y * dist )
  sf_df
}

calc_pos_2a <- function( text ) {
  (  text
  |> parse_subfile()
  |> augment_subfile_day2()
  |> subset( select = dir_x:dir_y )
  |> lapply( sum )
  )
}

calc_pos_2b <- function( text ) {
  coords <- (  text
            |> parse_subfile()
            )
  x <- y <- aim <- 0
  for ( i in seq.int( nrow( coords ) ) ) {
    tmp <- coords[[ "dist" ]][ i ]
    if ( "forward" == ( tmpdir <- coords[[ "dir" ]][ i ] ) ) {
      x <- tmp + x
      y <- y + tmp * aim
    } else if ( "down" == tmpdir ) {
      aim <- aim + tmp
    } else if ( "up" == tmpdir ) {
      aim <- aim - tmp
    } else {
      stop( "invalid direction" )
    }
  }
  data.frame( x = x, y = y )
}
