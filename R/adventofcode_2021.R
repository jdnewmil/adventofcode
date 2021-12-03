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

# Day 3 ----

cvt_bin_vec_2_int <- function( v ) {
  Reduce( \(n, dig) { n <- 2 * n + ( "0" != dig )}, v, init = 0L )
}

cvt_lgl_vec_2_int <- function( v ) {
  Reduce( \(n, dig) { n <- 2 * n + dig }, v, init = 0L )
}

parse_bin <- function( binv ) {
  (  binv
  |> strsplit( "", fixed = TRUE )
  |> sapply( cvt_bin_vec_2_int )
  )
}


parse_lgl_columns_m <- function( binv ) {
  (  binv
  |> strsplit( "", fixed = TRUE )
  |> (\(x) do.call( rbind, x ))()
  )
}

parse_lgl_columns <- function( binv ) {
  (  binv
  |> parse_lgl_columns_m()
  |> (\(x) lapply( seq.int( ncol( x ) )
                 , \(y) (nrow(x)/2) <= unname(table(x[ , y ])["1"])
                 ))()
  |> unlist()
  )
}

calc_gamma_rate <- function( lv ) {
  cvt_lgl_vec_2_int( lv )
}

calc_epsilon_rate <- function( lv ) {
  cvt_lgl_vec_2_int( !lv )
}

test_oxy_gen_criteria <- function( v ) {
  most_common <- attr( which.max( rev( table( v ) ) )[ 1 ], "names" ) == "TRUE"
  v == most_common
}

test_CO2_scrubber_criteria <- function( v ) {
  !test_oxy_gen_criteria( v )
}

calc_CO2_scrubber_rating <- function( m ) {
  calc_day3_rating( m, f = test_CO2_scrubber_criteria )
}

calc_oxy_gen_rating <- function( m ) {
  calc_day3_rating( m, f = test_oxy_gen_criteria )
}

calc_day3_rating <- function( m, f ) {
  m <- matrix( "1" == m, ncol = ncol( m ) )
  n <- ncol( m )
  i <- 0
  sel <- rep( TRUE, nrow( m ) )
  while( i < n && 1L < sum( sel ) ) {
    i <- i + 1L
    sel[ sel ] <- f( m[ sel, i ] )
  }
  stopifnot( 1L == sum( sel ) )
  cvt_lgl_vec_2_int( m[ sel, ] )
}
