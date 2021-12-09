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

# Day 4 ----

read_day4a <- function( lns ) {
  draws <- scan( text = lns[1], sep = ",", quiet = TRUE )
  lns <- lns[ -(1:2) ]
  sects <- cumsum( "" == lns )
  boards <- (  lns
            |> split( sects )
            |> lapply( \(lns) as.matrix( read.table( text = lns ) ) )
            )
  list( draws = draws, boards = boards )
}

mark_a_card <- function( card, board, draw ) {
  pos <- which( board == draw, arr.ind = TRUE )
  card[ pos ] <- TRUE
  card
}

is_card_winning <- function( card ) {
  i <- 0L
  won <- FALSE
  # cycle through rows
  while( !won && i < 5L ) {
    i <- i + 1L
    won <- all( card[ i, ] )
  }
  if ( !won ) {
    i <- 0L
    # cycle through columns
    while( !won && i < 5L ) {
      i <- i + 1L
      won <- all( card[ , i ] )
    }
  }
  won
}

play_draws <- function( boards, draws ) {
  cards <- lapply( boards
                 , function( bd ) {
                     matrix( FALSE, ncol = ncol( bd ), nrow = nrow( bd ) )
                   }
                 )
  draw_idx <- 0L
  won <- FALSE
  while( !won ) {
    draw_idx <- draw_idx + 1L
    stopifnot( draw_idx <= length( draws ) )
    draw <- draws[ draw_idx ]
    # update cards
    cards <- lapply( seq_along( boards )
                   , function( i ) {
                        mark_a_card( cards[[ i ]], boards[[ i ]], draw )
                     }
                   )
    results <- sapply( cards, is_card_winning )
    won <- any( results )
  }
  win_card_idx <- which( results )
  win_card <- cards[[ win_card_idx ]]
  score <- sum( boards[[ win_card_idx ]][ !c( win_card ) ] )
  draw * score
}

play_draws_last <- function( boards, draws ) {
  cards <- lapply( boards
                   , function( bd ) {
                     matrix( FALSE, ncol = ncol( bd ), nrow = nrow( bd ) )
                   }
  )
  draw_idx <- 0L
  last <- FALSE
  while( !last ) {
    draw_idx <- draw_idx + 1L
    stopifnot( draw_idx <= length( draws ) )
    draw <- draws[ draw_idx ]
    # update cards
    cards <- lapply( seq_along( boards )
                     , function( i ) {
                       mark_a_card( cards[[ i ]], boards[[ i ]], draw )
                     }
    )
    results <- sapply( cards, is_card_winning )
    last <- 1L == sum( !results )
  }
  last_card_idx <- which( !results )
  won <- FALSE
  while( !won ) {
    draw_idx <- draw_idx + 1L
    stopifnot( draw_idx <= length( draws ) )
    draw <- draws[ draw_idx ]
    # update cards
    cards <- lapply( seq_along( boards )
                     , function( i ) {
                       mark_a_card( cards[[ i ]], boards[[ i ]], draw )
                     }
    )
    results <- sapply( cards, is_card_winning )
    won <- all( results )
  }
  last_card <- cards[[ last_card_idx ]]
  score <- sum( boards[[ last_card_idx ]][ !c( last_card ) ] )
  draw * score
}

# Day 5 ----

parse_day5a <- function( lns ) {
  (  lns
  |> (\(x) gsub( "[^0-9]+", " ", x ) )()
  |> paste( collapse = "\n" )
  |> (\(x) read.table( text = x ))()
  |> setNames( c( "x1", "y1", "x2", "y2" ) )
  )
}

build_map_day5a <- function( dta ) {
  rngx <- range( c( dta$x1, dta$x2 ) )
  rngy <- range( c( dta$y1, dta$y2 ) )
  map <- matrix( 0
               , nrow = diff( rngy ) + 1L
               , ncol = diff( rngx ) + 1L
               )
  rownames( map ) <- seq( rngy[ 1 ], rngy[ 2 ] )
  colnames( map ) <- seq( rngx[ 1 ], rngx[ 2 ] )
  for (i in seq_along( dta$x1 ) ) {
    rw <- dta[ i, ]
    if ( rw$y1 == rw$y2 ) {
      ix <- seq( rw$x1 - rngx[ 1L ] + 1L, rw$x2 - rngx[ 1L ] + 1L ) 
      map[ rw$y1 -rngy[ 1L ] + 1L, ix ] <- map[ rw$y1 - rngy[ 1L ] + 1L, ix ] + 1L
    } else if ( rw$x1 == rw$x2 ) {
      iy <- seq( rw$y1 - rngy[ 1L ] + 1L, rw$y2 - rngy[ 1L ] + 1L ) 
      map[ iy, rw$x1 - rngx[ 1L ] + 1L ] <- map[ iy, rw$x1 - rngx[ 1L ] + 1L ] + 1L
    }
  }
  map
}

build_map_day5b <- function( dta ) {
  rngx <- range( c( dta$x1, dta$x2 ) )
  rngy <- range( c( dta$y1, dta$y2 ) )
  map <- matrix( 0
                 , nrow = diff( rngy ) + 1L
                 , ncol = diff( rngx ) + 1L
  )
  rownames( map ) <- seq( rngy[ 1 ], rngy[ 2 ] )
  colnames( map ) <- seq( rngx[ 1 ], rngx[ 2 ] )
  for (i in seq_along( dta$x1 ) ) {
    rw <- dta[ i, ]
    if ( rw$y1 == rw$y2 ) {
      # horizontal lines
      ix <- seq( rw$x1 - rngx[ 1L ] + 1L, rw$x2 - rngx[ 1L ] + 1L ) 
      map[ rw$y1 -rngy[ 1L ] + 1L, ix ] <- map[ rw$y1 - rngy[ 1L ] + 1L, ix ] + 1L
    } else if ( rw$x1 == rw$x2 ) {
      # vertical lines
      iy <- seq( rw$y1 - rngy[ 1L ] + 1L, rw$y2 - rngy[ 1L ] + 1L ) 
      map[ iy, rw$x1 - rngx[ 1L ] + 1L ] <- map[ iy, rw$x1 - rngx[ 1L ] + 1L ] + 1L
    } else if ( abs( rw$x1 - rw$x2 ) == abs( rw$y1 - rw$y2 ) ) {
      # diagonal 45deg lines
      ix <- seq( rw$x1 - rngx[ 1L ] + 1L, rw$x2 - rngx[ 1L ] + 1L ) 
      iy <- seq( rw$y1 - rngy[ 1L ] + 1L, rw$y2 - rngy[ 1L ] + 1L ) 
      ixy <- matrix( c( iy, ix ), ncol = 2L )
      map[ ixy ] <- map[ ixy ] + 1L
    }
  }
  map
}

count_danger_day5a <- function( map ) {
  sum( 2 <= map )
}

build_ixy_df <- function( x1, y1, x2, y2 ) {
  if ( y1 == y2 ) {
    ix <- seq( x1, x2 ) 
    iy <- rep( y1, length( ix ) )
  } else if ( x1 == x2 ) {
    iy <- seq( y1, y2 ) 
    ix <- rep( x1, length( iy ) )
  } else if ( abs( x1 - x2 ) == abs( y1 - y2 ) ) {
    ix <- seq( x1, x2 )
    iy <- seq( y1, y2 )
  } else {
    ix <- integer( 0 )
    iy <- integer( 0 )
  }
  data.frame( iy = iy, ix = ix )
}

count_danger_day5b_df <- function( dta ) {
  DF <- (   dta
        %>% rowwise()
        %>% mutate( ixy = list( build_ixy_df( x1, y1, x2, y2 ) ) )
        %>% ungroup()
        %>% select( ixy )
        %>% unnest( cols = "ixy" )
        %>% group_by( ix, iy )
        %>% count()
        %>% filter( 2 <= n )
        )
  nrow( DF )
}

# Day 6 ----

parse_lanternfish <- function( s ) {
  dta <- (  s
         |> strsplit( "," )
         |> unlist()
         |> table()
         |> as.data.frame()
         |> within({ Var1 = as.character( Var1 ) })
         |> merge( data.frame( Var1 = as.character( 0:8 ) )
                 , all.y = TRUE
                 , by = "Var1"
                 )
         |> within({ Freq[ is.na( Freq ) ] <- 0
                  })
         |> setNames( c( "State", "Count" ) )
         )
  dta[ order( dta$State ), ]
}

sim_day_lanternfish <- function( state ) {
  birthing <- state$Count[ 1 ]
  state$Count <- c( state$Count[ -1 ], birthing )
  state$Count[ 7 ] <- state$Count[ 7 ] + birthing
  state
}

sim_days_lanternfish <- function( state, days ) {
  for ( day in seq.int( days ) ) {
    state <- sim_day_lanternfish( state )
  }
  sum( state$Count )
}

# Day 7 ----

find_crab_fuel <- function( x, v, version = 1 ) {
  if ( 1 == version ) {
    cost <- \(x) x
  } else {
    cost <- (\(n) n*(n+1)/2 )
  }
  f1 <- \(x1) ( ( v - x1 ) |> abs() |> cost() |> round() |> sum() )
  # handle vector-valued x to perform fuel calcs repeatedly
  sapply( x, f1 )
}

find_min_crab_fuel <- function( v, version = 1 ) {
  # initial guess function
  if ( 1 == version ) {
    center <- median
  } else {
    center <- mean
  }
  guess1 <- (  v
            |> center()
            |> round()
            )
  # initial guess at minimum fuel
  ans1 <- find_crab_fuel( guess1, v, version = version )
  # rounding errors mean true minimum could be off by a few
  delta <- 1
  # guess 1 higher
  ans2 <- find_crab_fuel( guess2 <- guess1 + delta, v, version = version )
  if ( ans1 < ans2 ) {
    # did not decrease so start guessing lower
    delta <- -1
  } else {
    # progress further in this direction
    ans1 <- ans2
    guess1 <- guess2
  }
  # new second estimate
  ans2 <- find_crab_fuel( guess2 <- guess1 + delta, v, version = version )
  # keep going until first increase
  while ( ans2 < ans1 ) {
    guess1 <- guess2
    ans1 <- ans2
    ans2 <- find_crab_fuel( guess2 <- guess1 + delta, v, version = version )
  }
  ans1
}

# Day 8 ----

parse_segments <- function( s ) {
  (  s
  |> strsplit( "" )
  |> lapply( (\(x) as.integer( factor( x, levels = letters[ 1:8 ] ) ) ) )
  )
}

parse_day8a <- function( lns ) {
  lns2 <- (  lns
          |> strsplit( split = "|", fixed = TRUE )
          )
  inp <- (  lns2
         |> sapply( \(x) `[`( x, 1 ) )
         |> lapply( \(x) scan( text = x, what = character(), quiet = TRUE ) )
         )
  outp <- (  lns2
          |> sapply( \(x) `[`( x, 2 ) )
          |> lapply( \(x) scan( text = x, what = character(), quiet = TRUE ) )
          )
  list( input = inp, output = outp )
}

count_unique_day8a <- function( lst_s ) {
  (  lst_s
  |> unlist()
  |> nchar()
  |> tabulate( nbins = 8L )
  |> (\(x) x[ c( 2, 3, 4, 7 ) ] )()
  |> sum()
  )
}

# M = 7 * 7
# x = 7 * N
# M * x = X
# X = 7 * N
# d = 10 * 7 known

#  a
# b c
#  d
# e f
#  g

d <- ( scan( text = 
#a b c d e f g
"1 1 1 0 1 1 1 # 0
 0 0 1 0 0 1 0 # 1
 1 0 1 1 1 0 1 # 2
 1 0 1 1 0 1 1 # 3
 0 1 1 1 0 1 0 # 4
 1 1 0 1 0 1 1 # 5
 1 1 0 1 1 1 1 # 6
 1 0 1 0 0 1 0 # 7
 1 1 1 1 1 1 1 # 8
 1 1 1 1 0 1 1 # 9
"
           , quiet = TRUE
           , comment.char = "#"
           )
     |> matrix( byrow = TRUE, ncol = 7L )
     )
dlgl <- matrix( as.logical(d), ncol = ncol( d ) )

# diversion for logical vector approach... dead end
form_digit_string <- function(x) {
  forms <- (
" aaaa    ....    aaaa    aaaa    ....   
b    c  .    c  .    c  .    c  b    c  
b    c  .    c  .    c  .    c  b    c  
 ....    ....    dddd    dddd    dddd   
e    f  .    f  e    .  .    f  .    f  
e    f  .    f  e    .  .    f  .    f  
 gggg    ....    gggg    gggg    ....   
 aaaa    aaaa    aaaa    aaaa    aaaa   
b    .  b    .  .    c  b    c  b    c  
b    .  b    .  .    c  b    c  b    c  
 dddd    dddd    ....    dddd    dddd   
.    f  e    f  .    f  e    f  .    f  
.    f  e    f  .    f  e    f  .    f  
 gggg    gggg    ....    gggg    gggg   
"
           |> (\(.) strsplit( ., "\n" )[[1]])()
           |> strsplit( "" )
           |> (\(.) do.call( rbind, . ) )()
           )
  forms <- cbind( forms[ 1:7, ], forms[ 8:14, ] )
  forms <- array( c( forms ), dim = c( 7, 8, 10 ) )
  out <- do.call( cbind, lapply( x, \(.) forms[ , , . + 1 ] )) 
  (  out
  |> apply( 1, \(.) paste0( ., collapse = "" ) )
  |> (\(.) paste( ., collapse = "\n" ) )()
  )
}

# dead end... diversion
lookup_digit <- function( segs ) {
  segs <- as.logical( segs )
  i <- 1L
  while( i < 10L && any( xor( segs, dlgl[ i, ] ) ) ) {
    i <- i + 1L
  }
  if ( 10L <= i ) NULL
  else i - 1L
}

#lookup_digit( segs = c( 0, 0, 1, 0, 0, 1, 0 ) )
#lookup_digit( segs = c( 0, 1, 1, 1, 0, 1, 0 ) )

# number of segment in digits 0 to 9
segs_len <- c( 6, 2, 5, 4, 5, 6, 7, 6 )
# segment score is number of digits using that segment ... 7 distinct scores
segs_scores <- apply( t(d) * apply(d, 2, sum), 2, sum )

# among 10 unique digits, can sum up total number of segments needed
# either of two ways:
#  by digit: each digit has some number of segments, add those for each digit
#    (use this for output digits)
#  by segment: each segment corresponds to some number of digits, add those for each segment
#    (use this for input segments)
# both approaches involve the same total number of segments

# inputs constitute each of the 10 digits in some order
# input scores are counts of segment occurrences in input
calc_input_scores_day8b <- function( inpdta ) {
  inp <- inpdta$input
  (  inp
  |> lapply( (\(.) paste0( ., collapse = "" ) ) )
  |> unlist()
  |> strsplit( "", fixed = TRUE )
  |> lapply( toupper ) # keyed by "wrong" segment labeling
  |> lapply( table )
  )
}

# output scores use input segment scores to compute corresponding
# output segment scores
# for a given output digit, only one of the input scores will match
score_output <- function( outv, intbl ) {
  (  outv
  |> toupper() # keyed by "wrong" segment labeling
  |> strsplit( "" )
  |> sapply( \(ltrs) sum( intbl[ ltrs ] ) )
  |> match( segs_scores ) # get table indexes
  |> (\(.) . - 1 )() # convert to digit
  )
}
#score_output( inpdta$output[[1]], ans[[ 1 ]] )

calc_output_scores <- function( inpdta ) {
  inp_scores <- calc_input_scores_day8b( inpdta )
  lapply( seq_along( inp_scores )
        , \(i) score_output( inpdta$output[[i]], inp_scores[[i]] )
        )
}
