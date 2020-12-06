# adventofcode.R

calc_fuel <- function( m ) {
  floor( m / 3 ) - 2
}

calc_fuel2 <- function( m ) {
  f <- pmax( 0, calc_fuel( m ) )
  acc <- f
  while ( any( f > 0 ) ) {
    f <- pmax( 0, calc_fuel( f ) )
    acc <- f + acc
  }
  acc
}

parametermode_readvalue <- function( cpu, off ) {
  pm <- cpu$parametermodes[ off ]
  if ( 0L == pm ) {
    ptr <- cpu$pgm[ cpu$offset + off ] + 1L
    cpu$pgm[ ptr ]
  } else if ( 1L == pm ) {
    cpu$pgm[ cpu$offset + off ]
  } else {
    stop( sprintf( "Invalid read parameter mode %d at program location %d.", pm, cpu$offset ) )
  }
}

parametermode_writevalue <- function( cpu, off, value ) {
  pm <- cpu$parametermodes[ off ]
  if ( 0L == pm ) {
    ptr <- cpu$pgm[ cpu$offset + off ] + 1L
    cpu$pgm[ ptr ] <- value
  } else if ( 1L == pm ) {
    stop( sprintf( "Immediate mode not valid for write at program location %d.", cpu$offset ) )
  } else {
    stop( sprintf( "Invalid write parameter mode %d at program location %d.", pm, cpu$offset ) )
  }
}

opcode_add <- function( cpu ) {
  v1 <- parametermode_readvalue( cpu, 1L )
  v2 <- parametermode_readvalue( cpu, 2L )
  parametermode_writevalue( cpu, 3L, v1 + v2 )
  cpu$offset <- cpu$offset + 4L
  invisible( NULL )
}

opcode_multiply <- function( cpu ) {
  v1 <- parametermode_readvalue( cpu, 1L )
  v2 <- parametermode_readvalue( cpu, 2L )
  parametermode_writevalue( cpu, 3L, v1 * v2 )
  cpu$offset <- cpu$offset + 4L
  invisible( NULL )
}

opcode_input <- function( cpu ) {
  stopifnot( 0 < length( cpu$input ) )
  parametermode_writevalue( cpu, 1L, cpu$input[ 1L ] )
  cpu$input <- cpu$input[ -1L ]
  cpu$offset <- cpu$offset + 2L
  invisible( NULL )
}

opcode_output <- function( cpu ) {
  v1 <- parametermode_readvalue( cpu, 1L )
  cpu$output[ length( cpu$output ) + 1L ] <- v1
  cpu$offset <- cpu$offset + 2L
  invisible( NULL )
}

opcode_jump_if_true <- function( cpu ) {
  v1 <- parametermode_readvalue( cpu, 1L )
  if ( 0 != v1 ) {
    cpu$offset <- parametermode_readvalue( cpu, 2L ) + 1L
  } else {
    cpu$offset <- cpu$offset + 3L
  }
  invisible( NULL )
}

opcode_jump_if_false <- function( cpu ) {
  v1 <- parametermode_readvalue( cpu, 1L )
  if ( 0 == v1 ) {
    cpu$offset <- parametermode_readvalue( cpu, 2L ) + 1L
  } else {
    cpu$offset <- cpu$offset + 3L
  }
  invisible( NULL )
}

opcode_less_than <- function( cpu ) {
  v1 <- parametermode_readvalue( cpu, 1L )
  v2 <- parametermode_readvalue( cpu, 2L )
  result <- as.integer( v1 < v2 )
  parametermode_writevalue( cpu, 3L, result )
  cpu$offset <- cpu$offset + 4L
  invisible( NULL )
}

opcode_equals <- function( cpu ) {
  v1 <- parametermode_readvalue( cpu, 1L )
  v2 <- parametermode_readvalue( cpu, 2L )
  result <- as.integer( v1 == v2 )
  parametermode_writevalue( cpu, 3L, result )
  cpu$offset <- cpu$offset + 4L
  invisible( NULL )
}


opcodes <- list( 99L )
opcodes[[ 1L ]] <- opcode_add
opcodes[[ 2L ]] <- opcode_multiply
opcodes[[ 3L ]] <- opcode_input
opcodes[[ 4L ]] <- opcode_output
opcodes[[ 5L ]] <- opcode_jump_if_true
opcodes[[ 6L ]] <- opcode_jump_if_false
opcodes[[ 7L ]] <- opcode_less_than
opcodes[[ 8L ]] <- opcode_equals

parametermode_extractopcode <- function( cpu ) {
  ocode <- cpu$pgm[ cpu$offset ]
  opcode <- ocode %% 100L
  ocode <- ocode %/% 100L
  parametermodes <- integer( 3L )
  parametermodes[ 1L ] <- ocode %% 10L
  ocode <- ocode %/% 10L
  parametermodes[ 2L ] <- ocode %% 10L
  ocode <- ocode %/% 10L
  parametermodes[ 3L ] <- ocode %% 10L
  cpu$parametermodes <- parametermodes
  opcode
}

intcode1 <- function( pgm, input = NULL, output = NULL ){
  cpu <- new.env( parent = emptyenv() )
  cpu$pgm <- pgm
  cpu$offset <- 1L
  cpu$input <- input
  cpu$output <- output
  cpu$parametermodes <- integer( 3L )
  while ( 99L != ( opcode <- parametermode_extractopcode( cpu ) ) ) {
    opcodes[[ opcode ]]( cpu ) # execute opcode
  }
  cpu
}

intcode1_pipe <- function( pgm, input ) {
  result <- intcode1( pgm, input = input )
  result$output
}

f_day2_part2 <- function( noun, verb, pgm ) {
  pgm[ 2 ] <- noun
  pgm[ 3 ] <- verb
  result <- intcode1( pgm )
  result$pgm[ 1 ]
}

xing_dirs <- c( "R", "U", "L", "D" )
xing_map <- as.matrix( read.table( text =
"1  0
 0  1
-1  0
 0 -1
", header=FALSE ) )

parse_day3 <- function( s ) {
  ss <- strsplit( s, "," )[[ 1 ]]
  d <- as.integer( factor( substr( ss, 1, 1 ), xing_dirs ) )
  mag <- as.integer( sub( "^.(.*)$", "\\1", ss ) )
  xing_map[ d, ] * mag
}

xprod <- function( AB, CD ) {
  AB[ 1 ] * CD[ 2 ] - AB[ 2 ] * CD[ 1 ]
}

calc_intersection <- function( A, C, AB, CD ) {
  den <- xprod( CD, AB )
  ss <- xprod( A - C, AB ) / den
  tt <- xprod( A - C, CD ) / den
  if ( ss < 0 || 1 < ss || tt < 0 || 1 < tt ) {
    ss <- Inf
  }
  p <- C + ss * CD
}

calc_md <- function( delta ) {
  sum( abs( delta ) )
}

f_day3_part1_impl <- function( input1, input2 ) {
  inp1 <- parse_day3( input1 )
  inp2 <- parse_day3( input2 )
  len1 <- c( 0, cumsum( abs( inp1[ , 1 ] ) + abs( inp1[ , 2 ] ) ) )
  len2 <- c( 0, cumsum( abs( inp2[ , 1 ] ) + abs( inp2[ , 2 ] ) ) )
  pos1 <- rbind( c( 0, 0 ), apply( inp1, 2, cumsum ) )
  pos2 <- rbind( c( 0, 0 ), apply( inp2, 2, cumsum ) )
  colnames( pos1 ) <- colnames( pos2 ) <- c( "X", "Y" )
  max_intersects <- nrow( pos1 ) * nrow( pos2 )
  i_max <- nrow( pos1 ) - 1L
  j_max <- nrow( pos2 ) - 1L
  i_seq <- seq.int( i_max )
  j_seq <- seq.int( j_max )
  idx_intersects <- expand.grid( i = seq.int( i_max )
                               , j = seq.int( j_max )
                               )
  pos_x_intersects <- matrix( NA_real_, nrow = i_max, ncol = j_max )
  pos_y_intersects <- matrix( NA_real_, nrow = i_max, ncol = j_max )
  min_md <- Inf
  for ( i in i_seq ) {
    for ( j in j_seq ) {
      if ( 1 != i || 1 != j ) {
        intersection <- calc_intersection( pos1[ i, ], pos2[ j, ], inp1[ i, ], inp2[ j, ] )
        md <- calc_md( intersection )
        if ( is.finite( md ) ) {
          min_md <- min( min_md, md )
          pos_x_intersects[ i, j ] <- intersection[ 1 ]
          pos_y_intersects[ i, j ] <- intersection[ 2 ]
        }
      }
    }
  }
  pts <- data.frame( idx_intersects
                   , x = c( pos_x_intersects )
                   , y = c( pos_y_intersects )
                   , md = c( pos_x_intersects ) + c( pos_y_intersects )
                   )
  list( inp1 = inp1
      , inp2 = inp2
      , len1 = len1
      , len2 = len2
      , pos1 = pos1
      , pos2 = pos2
      , min_md = min_md
      , pts = pts[ !is.na( pts$md ), ]
      )
}

f_day3_part1 <- function( input1, input2 ) {
  f_day3_part1_impl( input1, input2 )$min_md
}

f_day3_part2 <- function( input1, input2 ) {
  result <- f_day3_part1_impl( input1, input2 )
  i_dist <- apply( as.matrix( result$pts[ , c( "x", "y" ) ] )
                 - result$pos1[ result$pts$i, ]
                 , 1
                 , calc_md 
                 )
  j_dist <- apply( as.matrix( result$pts[ , c( "x", "y" ) ] )
                 - result$pos2[ result$pts$j, ]
                 , 1
                 , calc_md
                 )
  L <- with( result
           ,   len1[ pts$i ]
             + len2[ pts$j ] 
             + i_dist 
             + j_dist
           )
  idx <- which.min( L )
  unname( L[ idx ] )
}

plot_day3_part1 <- function( d3p1 ) {
  DF1 <- as.data.frame( d3p1$pos1 )
  DF2 <- as.data.frame( d3p1$pos2 )
  DF <- rbind( data.frame( Path = "1", DF1, stringsAsFactors = FALSE )
             , data.frame( Path = "2", DF2, stringsAsFactors = FALSE )
             )
  ggplot( DF, aes( x = X, y = Y, colour = Path ) ) +
    geom_path( size=1 ) +
    geom_point( data = d3p1$pts, mapping = aes( x = x, y = y ), inherit.aes = FALSE )
}

test_day4_part1 <- function( v ) {
  has_double <- FALSE
  last_d <- 10L
  for ( i in 1:6 ) {
    d <- v %% 10L
    if ( last_d < d ) return( FALSE )
    has_double <- has_double || ( last_d == d )
    v <- v %/% 10L
    last_d <- d
  }
  has_double
}

f_day4_part1 <- function( v2 ) {
  acc <- 0L
  for ( v in seq( v2[ 1 ], v2[ 2 ] ) ) {
    acc <- acc + test_day4_part1( v )
  }
  acc
}

test_day4_part2 <- function( v ) {
  has_double <- integer( 10 )
  last_d <- 10L
  for ( i in 1:6 ) {
    d <- v %% 10L
    if ( last_d < d ) return( FALSE )
    if ( last_d == d ) {
      has_double[ d ] <- has_double[ d ] + 1L
    }
    v <- v %/% 10L
    last_d <- d
  }
  any( 1L == has_double )
}

f_day4_part2 <- function( v2, tf = test_day4_part2 ) {
  acc <- 0L
  for ( v in seq( v2[ 1 ], v2[ 2 ] ) ) {
    acc <- acc + tf( v )
  }
  acc
}

f_day5_part1 <- function( pgm, input ) {
  result <- intcode1( pgm, input = input )
  result$output
}
