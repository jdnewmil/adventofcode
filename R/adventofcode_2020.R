# adventofcode_2020.R

# Day 1 ----

#' Return indexes of two values that sum to pair_sum
#' @param v Integer vector
#' @param pair_sum Target value to find
find_vec_pairsum <- function( v, pair_sum ) {
  m <- outer( v, v, FUN = "+" )
  c( which( m == pair_sum & upper.tri( m ), arr.ind = TRUE ) )
}

#' Return indexes of N values that sum to n_sum
#' 
#' @param v Integer vector, set of numbers to search for desired sums
#' @param nsum Integer scalar, target value to search for
#' @param n Integer scalar, count of numbers from v that should sum to nsum
#' @return Integer vector of indexes of numbers in v that sum to nsum
find_vec_nsum <- function( v, nsum, n ) {
  if ( 2 == n ) {
    find_vec_pairsum( v, pair_sum = nsum )
  } else {
    i <- length( v ) + 1L
    ix <- integer( 0 )
    while ( 0 == length( ix ) && n < i ) {
      i <- i - 1L
      last <- v[ i ]
      v <- v[ -i ]
      ns <- nsum - last
      ix <- find_vec_nsum( v, ns, n - 1L )
    }
    c( ix, i )
  }
}

# Day 2 ----

#' Parse lines of password database
#' 
#' @param s Character vector, lines read from password database
#' @return Data frame with columns:
#'   \describe{
#'     \item{ixlo}{Integer, first number from password db}
#'     \item{ixhi}{Integer, second number from password db}
#'     \item{ltr}{Integer, Character, specified letter from password db}
#'     \item{pwd}{Character, password to verify}
#'   }
parse_pwd2a <- function( s ) {
  lst <- strsplit( s, "[- :]+" )
  if ( any( 4 != sapply( lst, length ) ) ) {
    stop( "invalid format")
  }
  result <- setNames( as.data.frame( matrix( unlist( lst )
                                           , ncol = 4
                                           , byrow = TRUE
                                           )
                                   )
                    , c( "ixlo", "ixhi", "ltr", "pwd" )
                    )
  result$ixlo <- as.integer( result$ixlo )
  result$ixhi <- as.integer( result$ixhi )
  result
}

#' Day 2 first problem check password algorithm
#' 
#' @param ixlo Integer, first number from password db
#' @param ixhi Integer, second number from password db
#' @param ltr Character, specified letter from password db
#' @param pwd Character, password to verify
#' @return Logical vector, TRUE if password is verified
chk_pwd2a <- function( ixlo, ixhi, ltr, pwd ) {
  pwd_ltrs <- strsplit( pwd, "" )[[ 1 ]]
  ftbl <- table( pwd_ltrs )
  ct <- ftbl[ ltr ]
  if ( is.na( ct ) ) ct <- 0
  ixlo <= ct && ct <= ixhi
}

#' Day 2 second problem check password algorithm
#' 
#' @param ixlo Integer, first number from password db
#' @param ixhi Integer, second number from password db
#' @param ltr Character, specified letter from password db
#' @param pwd Character, password to verify
#' @return Logical vector, TRUE if password is verified
chk_pwd2b <- function( ixlo, ixhi, ltr, pwd ) {
  pwd_ltrs <- strsplit( pwd, "" )[[ 1 ]]
  1 == sum( pwd_ltrs[ ixlo ] == ltr, pwd_ltrs[ ixhi ] == ltr )
}

# Day 3 ----

#' @param lns Character, lines of text consisting of '.' for empty space and '#'
#'   for trees. Each line is a "row" of tree location information.
#' @return Logical matrix, TRUE if a tree is in that row/column
parse_forest <- function( lns ) {
  (   lns   # vector of lines of periods and hashes
  %>% strsplit( "" ) # list of vectors of characters
  %>% unlist() # vector of characters
  %>% matrix( nrow = length( lns )
            , byrow = TRUE
            )
  ) == "#"
}

#' @param row_delta Integer, increment of row positions along trajectory
#' @param col_delta Integer, increment of column positions along trajectory
#' @param row_max Integer, number of available rows
#' @param col_max Integer, number of available columns
#' @return Integer matrix, N by 2, designating positions in tree map
#'   that this trajectory starting at 1,1 will intersect.
generate_trajectory <- function( row_delta, col_delta, row_max, col_max ) {
  if ( 0 == row_delta ) {
    if ( 0 == col_delta )
      stop( "cannot generate 0,0 trajectory" )
    C <- seq( col_delta, col_max - 1L, by = col_delta )
    R <- rep( 0, length( C ) )
  } else {
    R <- seq( 0, row_max - 1L, by = row_delta ) # sequence of row values
    if ( 0 == col_delta ) C <- rep( 0, length( R ) )
    else C <- col_delta * ( seq.int( length( R ) ) - 1L )
  }
  # coordinate matrix for the trajectory
  matrix( c( 1L + R %% row_max
           , 1L + C %% col_max
           )
        , ncol = 2L
        )
}

#' @param tree_map Logical matrix, TRUE if a tree is at that location
#' @param row_delta Integer, interval at which rows are incremented
#' @param col_delta Integer, interval at which columns are incremented
#' @return Integer, number of trees (TRUE values in tree_map) encountered
#'   in progressing from position 1,1 along the trajectory of deltas
count_toboggan_trees <- function( tree_map, row_delta, col_delta ) {
  traj <- generate_trajectory( row_delta = row_delta
                             , col_delta = col_delta
                             , row_max = nrow( tree_map )
                             , col_max = ncol( tree_map )
                             )
  sum( tree_map[ traj ] )
}

# Day 4 ----

#' Parse a (on-line) passport record
#' 
#' @param s Character vector, one record per element
#' @return Data frame (one row) with column names as defoined in the record
parse_pprec <- function( s ) {
  # list of vectors
  parts <- strsplit( s, ":" )
  # extract first element of each vector (names) as a character vector
  nms <- map_chr( parts, `[`, i = 1L )
  # extract second element of each vector (values) as a list
  result <- map( parts, `[`, i = 2L )
  names( result ) <- nms
  # create a one-row data frame out of the named list
  data.frame( result, stringsAsFactors = FALSE )
}


#' passport data parsing
#' 
#' @param lns Character, lines from file of blank-line-delimited records,
#'   each record is a key and value as a semicolon-separated pair
#' @return Data frame, one column per key. Sequence of keys is dependent on
#'   the input data.
parse_ppdata <- function( lns ) {
  # mark end of record lines
  eor <- "" == lns
  (   lns
  %>% split( cumsum( eor ) ) # make a list of records (vectors of fields)
  %>% map( paste, collapse = " " ) # list of single-record strings
  %>% map( function( r ) # list of character vectors of fields
              scan( text = r
                  , what = character()
                  , quiet = TRUE
                  )
         )
  %>% map( parse_pprec ) # list of data frames
  %>% bind_rows()
  )
}

ppdata_field_info <- read.table( text =
"field  rqd    pat
byr     TRUE   '^\\d{4}$'
iyr     TRUE   '^\\d{4}$'
eyr     TRUE   '^\\d{4}$'
hgt     TRUE   '^(\\d+)(cm|in)$'
hcl     TRUE   '^#[0-9a-f]{6}$'
ecl     TRUE   '^(amb|blu|brn|gry|grn|hzl|oth)$'
pid     TRUE   '^[0-9]{9}$'
cid     FALSE  '.*'
", header = TRUE, as.is = TRUE )

#' @param v Character, integer values to range check
#' @param lo Integer, minimum allowable value
#' @param hi Integer, maximum allowable value
#' @return Logical, TRUE if passed range check
ppdta_chk_int_range <- function( v, lo, hi ) {
  v <- as.integer( v )
  ans <- lo <= v & v <= hi
  ans[ is.na( ans ) ] <- FALSE
  ans
}

#' passport dataframe validation
#' 
#' @param ppdata Data frame, must contain at least the columns
#'   defined in \code{ppdata_field_info}
#' @param enhanced Character scalar, one of "4a" or "4b"
#' @return Logical vector, one element per row of ppdata
validate_ppdata <- function( ppdata, enhanced = "4a" ) {
  ppdata_field_check <- ppdata_field_info$field %in% names( ppdata )
  if ( !all( ppdata_field_check ) ) {
    stop( sprintf( "ppdata missing fields: %s"
                 , paste( names( ppdata )[ ppdata_field_check ]
                        , collapse = ", "
                        )
                 )
        )
  }
  ppdata_field_check <-
    if ( "4a" == enhanced ) {
      function( field, rqd, pat ) {
        ( !is.na( ppdata[[ field ]] )
        | !rqd
        )
      }
    } else if ( "4b" == enhanced ) {
      function( field, rqd, pat ) {
        ( ( !is.na( ppdata[[ field ]] )
          & grepl( pat, ppdata[[ field ]] )
          )
        | !rqd
        )
      }
    } else {
      stop( sprintf( "invalid 'enhanced' argument : %s"
                   , enhanced
                   )
          )
    }
  fldok <- pmap( ppdata_field_info, ppdata_field_check )
  if ( "4b" == enhanced ) {
    height_val <- as.integer( sub( "^(\\d+).*", "\\1", ppdata$hgt ) )
    height_unit <- sub( "^\\d+(cm|in)$", "\\1", ppdata$hgt )
    height_cm <- "cm" == height_unit
    height_in <- "in" == height_unit
    height_lo <- ifelse( height_cm
                       , 150
                       , ifelse( height_in
                               , 59
                               , Inf
                               )
                       )
    height_hi <- ifelse( height_cm
                       , 193
                       , ifelse( height_in
                               , 76
                               , -Inf
                         )
                       )
    heightok <- height_lo <= height_val & height_val <= height_hi
    heightok[ is.na( heightok ) ] <- FALSE
    byr_ok <- ppdta_chk_int_range( ppdata$byr, 1920, 2002 )
    iyr_ok <- ppdta_chk_int_range( ppdata$iyr, 2010, 2020 )
    eyr_ok <- ppdta_chk_int_range( ppdata$eyr, 2020, 2030 )
    names( fldok ) <- ppdata_field_info$field
    (   fldok
    %>% reduce( pmin )
    %>% as.logical()
    & heightok
    & byr_ok
    & iyr_ok
    & eyr_ok
    )
  } else { # 4a
    (   fldok
    %>% reduce( pmin )
    %>% as.logical()
    )
  }
}

# Day 5 ----

decode_bp_to_int <- function( s ) {
  (   s
  %>% gsub( "[FL]", "0", . )
  %>% gsub( "[BR]", "1", . )
  %>% strtoi( base = 2L )
  )
}

seat_id_to_row <- function( si ) {
  si %/% 8L
}

seat_id_to_col <- function( si ) {
  si %% 8L
}

# Day 6 ----

#' multiline space-delimited record parsing
#' 
#' @param lns Character, lines from file of blank-line-delimited records,
#'   each record is space delimited field information
#' @return List of character vectors (fields)
parse_multiline_recs <- function( lns ) {
  # mark end of record lines
  eor <- "" == lns
  (   lns
  %>% split( cumsum( eor ) ) # make a list of records (vectors of fields)
  %>% map( paste, collapse = " " ) # list of single-record strings
  %>% map( function( r ) # list of character vectors of fields
              scan( text = r
                  , what = character()
                  , quiet = TRUE
                  )
         )
  %>% compact()
  )
}

#' @param rec Character vector, each element consists of answers for one person
#' @param FUN Function, one of `union` (part 6a) or `intersect` (part 6b)
#' @return Character vector, individual answers meeting criteria
normalize_rec_answers <- function( rec, FUN = union ) {
  (   rec
  %>% strsplit( split = "" )
  %>% reduce( FUN )
  )
}

#' @param List of character vectors, each of which contains answers from various
#'   party members
#' @param FUN Function, one of `union` (part 6a) or `intersect` (part 6b)
#' @return List of character vectors, one for each record, identifying answers
#'   meeting criteria for that party
count_rec_answers <- function( recList, FUN = union ) {
  (   recList
  %>% map( normalize_rec_answers, FUN = FUN )
  %>% map_int( length )
  %>% as.vector()
  )
}

# Day 7 ----

#' Parse a (on-line) passport record
#' 
#' @param s Character vector, one record per element
#' @return Data frame (one row) with column names as defoined in the record
parse_keyval <- function( s, pat, keyname, valname ) {
  # extract part before first separator
  nms <- sub( pat, "\\1", s )
  # extract second element of each vector (values) as a list
  vals <- sub( pat, "\\2", s )
  # create a data frame out of the named list
  result <- data.frame( key = nms
                      , vals = vals
                      , stringsAsFactors = FALSE
                      )
  names( result ) <- c( keyname, valname )
  result
}

parse_bag_rules <- function( s ) {
  ruleno <- seq_along( s )
  some_bags_ix <- grepl( "\\D+ bags contain .*$", s )
  DF <- (   s[ some_bags_ix ]
        %>% parse_keyval( pat = "^(.*?) bags contain (.*)$"
                        , "containing_bag_color"
                        , "all_vals" )
        %>% rowwise()
        %>% mutate( data = (   all_vals
                           %>% strsplit( ", " )
                           %>% map( function( x )
                                sub( " bags?.?$", "", x ) )
                           %>% map( parse_keyval
                                  , pat = "^(.*?) (.*)$"
                                  , keyname = "cbnum"
                                  , valname = "cbcolor"
                                  )
                           )
                  )
        %>% ungroup()
        %>% select( -all_vals )
        %>% unnest( cols = "data" )
        )
  DF
}

make_bag_rules_matrix <- function( bag_rules_df ) {
  bag_rules_df <- subset( bag_rules_df, "no" != cbnum )
  bag_rules_df$cbnum <- as.integer( bag_rules_df$cbnum )
  colrs <- union( bag_rules_df$containing_bag_color
                , bag_rules_df$cbcolor
                )
  result <- matrix( 0
                  , ncol = length( colrs )
                  , nrow = length( colrs )
                  )
  rownames( result ) <- colnames( result ) <- colrs
  result[ matrix( with( bag_rules_df
                      , match( c( containing_bag_color
                                , cbcolor
                                )
                             , colrs
                             )
                      )
                , ncol = 2L
                )
        ] <- bag_rules_df$cbnum
  result
}

count_colors_containing <- function( bag_rules_mat, contained_color ) {
  brl <- 0 != bag_rules_mat
  v <- brl[ , match( contained_color, colnames( brl ) ) ]
  svlast <- 0
  while ( svlast < ( sv <- sum( v ) ) ) {
    svlast <- sv
    v <- as.logical( brl %*% v ) | v
  }
  sum( v )
}

count_required_bags <- function( bag_rules_mat, containing_color ) {
  v <- bag_rules_mat[ containing_color, ]
  accum <- 0
  while ( 0 < ( sv <- sum( v ) ) ) {
    accum <- accum + sv 
    v <- v %*% bag_rules_mat
  }
  accum
}

# Day 8 ----

#' define a virtual cpu
cpu8a <- function() {
  ptr <- 1L
  acc <- 0L
  
  list( ops = list( nop = function( arg1 ) ptr <<- ptr + 1L
                  , jmp = function( arg1 ) ptr <<- ptr + arg1
                  , acc = function( arg1 ) {
                      acc <<- acc + arg1
                      ptr <<- ptr + 1L
                    }
                  )
      , ptr = function() ptr
      , acc = function() acc
      )
}


parse_pgm_8a <- function( src ) {
  (   data.frame( src = src
                , stringsAsFactors = FALSE
                )
  %>% separate( src
              , into = c( "op", "arg1" )
              , sep = " " )
  %>% mutate( arg1 = as.integer( arg1 ) )
  )
}

#' compile the code (create an "executable" object)
compile_code_8a <- function( code ) {
  cpu <- cpu8a()
  lcode <- (   code
           %>% rowwise()
           %>% mutate( fun = list( cpu$ops[[ op ]] )
                     , marks = FALSE
                     )
           %>% ungroup()
           )
  list( exec1 = function() {
          p <- cpu$ptr() # instruction ptr
          lcode$marks[ cpu$ptr() ] <<- TRUE
          lcode$fun[[ p ]]( lcode$arg1[ p ] ) # exec opcode
          lcode$marks[ cpu$ptr() ]
        }
      , acc = function() cpu$acc()
      )
}

#' run the specified code
run_code_8a <- function( code ) {
  lcode <- compile_code_8a( code )
  last_acc <- lcode$acc()
  while ( !lcode$exec1() ) {
    last_acc <- lcode$acc()
  }
  last_acc
}

## Day 8b ----

#' convert the code to compiled form (assign functions)
#' 
#' This function instruments the code to track behavior
compile_code_8b <- function( code ) {
  last_run <- 0
  cpu <- cpu8a()
  lcode <- (   code
           %>% rowwise()
           %>% mutate( fun = list( cpu$ops[[ op ]] )
                     , marks = NA_integer_
                     )
           %>% ungroup()
           )
  list( exec1 = function() {
          p <- cpu$ptr() # instruction ptr
          last_run <<- last_run + 1L
          lcode$marks[ cpu$ptr() ] <<- last_run
          lcode$fun[[ p ]]( lcode$arg1[ p ] ) # exec opcode
          lcode$marks[ cpu$ptr() ]
        }
      , acc = function() cpu$acc()
      , ptr = function() cpu$ptr()
      , marks = function() lcode$marks
      )
}

#' run the specified code with instrumented output
run_code_8b <- function( code ) {
  lcode <- compile_code_8b( code )
  last_acc <- lcode$acc()
  finished <- FALSE
  while (  is.na( lcode$exec1() )
        && !( finished <- nrow( code ) < lcode$ptr() )
        ) {
    last_acc <- lcode$acc()
  }
  list( last_acc = last_acc
      , finished = finished
      , marks = lcode$marks()
      )
}

#' run repeatedly to find which single instruction change stops the loop
alter_nopjmps <- function( code ) {
  dryrun <- run_code_8b( code )
  code$marks <- dryrun$marks
  ptrs_to_change <- which( code$op %in% c( "jmp", "nop" )
                         & !is.na( code$marks )
                         )
  code$finished <- NA
  code$last_acc <- NA
  for ( p in ptrs_to_change ) {
    test_code <- code[ , c( "op", "arg1" ) ]
    if ( "jmp" == test_code$op[ p ] ) test_code$op[ p ] <- "nop"
    else test_code$op[ p ] <- "jmp"
    ans <- run_code_8b( test_code )
    code$finished[ p ] <- ans$finished
    code$last_acc[ p ] <- ans$last_acc
  }
  code
}

# Day 9 ----

check_sum_validity <- function( v, n ) {
  ix <- n+1L
  while (  ix <= length( v )
        && 0 < length( find_vec_pairsum( v[ seq( ix-n-1L, ix-1L ) ]
                                       , v[ ix ]
                                       ) ) ) {
    ix <- ix + 1L
  }
  if ( length( v ) < ix ) NA
  else ix
}

#' find a contiguous sequence of numbers in v that add to the ix-th element of v
find_encryption_weakness <- function( v, ix ) {
  goal <- v[ ix ]
  ix1 <- ix0 <- 1L
  accum <- v[ ix0 ]
  while (  ix0 <= length( v )
        && accum != goal
        ) {
    if ( accum < goal ) {
      ix1 <- ix1 + 1L
      if ( length( v ) < ix1 ) return( NULL )
      accum <- accum + v[ ix1 ]
    } else {
      if ( ix0 == ix1 ) {
        ix1 <- ix0 <- ix0 + 1L
        accum <- v[ ix0 ]
      } else {
        accum <- accum - v[ ix0 ]
        ix0 <- ix0 + 1L
      }
    }
  }
  c( ix0, ix1 )
}

# Day 10 ----

get_joltdiff_table <- function( v ) {
  table( diff( c( 0, sort( v ), max( v ) + 3L ) ) )
}

trinarynum1 <- function( n ) {
  if ( n < 11L ) c( 1, 2, 4, 7, 13, 24, 44, 81, 149, 274 )[ n ]
  else {
    ( trinarynum1( n - 1L ) 
    + trinarynum1( n - 2L ) 
    + trinarynum1( n - 3L )
    )
  }
}

trinarynum <- Vectorize( trinarynum1, "n" )

count_joltdiff_combos_v <- function( v ) {
  r <- rle( diff( c( 0, sort( v ), max( v ) + 3L ) ) )
  n <- r$lengths[ 1L == r$values ]
  nn <- n[ 1L < n ]
  trinarynum( nn )
}

# Day 11 ----

parse_seat_map <- function( lns ) {
  trans <- setNames( 1:3, c( ".", "L", "#" ) )
  map <- (   lapply( lns, strsplit, split = "" )
         %>% lapply( `[[`, i = 1L )
         %>% do.call( rbind, . )
         %>% `[`( trans, . )
         %>% matrix( nrow = length( lns ) )
         )
  map1 <- matrix( 1
                , nrow = length( lns ) + 2L
                , ncol = ncol( map ) + 2L
                )
  map1[ seq( 2L, 1L + nrow( map ) )
      , seq( 2L, 1L + ncol( map ) )
      ] <- map
  map1
}

reseat_seatmap <- function( seatmap ) {
  adjacency <- matrix( c( -1, -1 # NW
                        ,  0, -1 # W
                        ,  1, -1 # SW
                        ,  1,  0 # S
                        ,  1,  1 # SE
                        ,  0,  1 # E
                        , -1,  1 # NE
                        , -1,  0 # N
                        )
                     , ncol = 2L
                     , byrow = TRUE
                     )
  sm <- seatmap
  fmap <- function( x, y ) {
    if ( 1L == x | 1L == y | nrow( sm ) == x | ncol( sm ) == y ) return( 1L )
    loc <- matrix( c( x, y ), ncol = 2, nrow = 8, byrow = TRUE )
    adj <- sm[ loc + adjacency ]
    empties <- sum( 2L == adj, na.rm = TRUE )
    fulls <- sum( 3L == adj, na.rm = TRUE )
    here <- sm[ x, y ]
    if ( 2L == here ) { # this seat is empty
      if ( 0L == fulls ) 3L # no full seats? full
      else here
    } else if ( 3L == here ) { # this location is full
      if ( 4L <= fulls ) 2L # 4 or more full? empty
      else here
    } else here
  }
  fmapv <- Vectorize( fmap, c( "x", "y" ) )
  outer( seq.int( nrow( sm ) )
       , seq.int( ncol( sm ) )
       , fmapv
       )
}

is_inside_border <- function( point, seatmap ) {
  (  1L < point[ 1 ]
  && 1L < point[ 2 ]
  && point[ 1 ] < nrow( seatmap )
  && point[ 2 ] < ncol( seatmap )
  )
}

seatmap_border <- function( seatmap ) {
  dta <- expand.grid( x = seq.int( nrow( seatmap ) )
                    , y = seq.int( ncol( seatmap ) )
                    )
  dta$border <- FALSE
  dta$border[ with( dta
                  ,   1L == x
                    | 1L == y
                    | nrow( seatmap ) == x
                    | ncol( seatmap ) == y
                  ) ] <- TRUE
  matrix( dta$border, ncol = ncol( seatmap ) )
}

seatmap_table <- function( seatmap ) {
  bord <- 2 * ( nrow( seatmap ) + ncol( seatmap ) ) - 4L
  chrs <- c( ".", "L", "#" )
  tt <- table( factor( chrs[ seatmap ], levels = chrs ) )
  tt[ 1 ] <- tt[ 1 ] - bord
  tt
}

print_seatmap <- function( seatmap ) {
  map <- matrix( seatmap[ !seatmap_border( seatmap ) ]
               , nrow = nrow( seatmap ) - 2L
               )
  s <- (   map
       %>% nrow()
       %>% seq.int()
       %>% sapply( function(i) {
                     v <- map[i,]
                     paste( c( ".", "L", "#" )[ v ], collapse = "" )
                   }
                 )
       %>% paste( collapse = "\n" )
       )
  cat( s )
}

seatmap_steady_state <- function( seatmap, reseat = reseat_seatmap, ... ) {
  nxt <- reseat( seatmap, ... )
  ctr <- 1L
  while ( !all( seatmap == nxt ) ) {
    seatmap <- nxt
    nxt <- reseat( seatmap, ... )
    ctr <- ctr + 1L
  }
  list( seatmap = seatmap
      , ctr = ctr
      )
}

find_seatmap_first_visible <- function( dv, from, seatmap ) {
  from <- matrix( from, ncol = 2 ) + dv
  while ( is_inside_border( from, seatmap ) ) {
    if ( 1L < seatmap[ from ] )
      return( from[ 1L ]
            + ( from[ 2L ] - 1L ) * nrow( seatmap )
            )
    from <- from + dv
  }
  NA
}

find_seatmap_visible_seats <- function( seatmap ) {
  adjacency <- matrix( c( -1, -1 # NW
                        ,  0, -1 # W
                        ,  1, -1 # SW
                        ,  1,  0 # S
                        ,  1,  1 # SE
                        ,  0,  1 # E
                        , -1,  1 # NE
                        , -1,  0 # N
                        )
                     , ncol = 2L
                     , byrow = TRUE
                     )
  (   expand.grid( N = seq( 2L, nrow( seatmap ) - 1L )
                 , M = seq( 2L, ncol( seatmap ) - 1L )
                 )
  %>% as.matrix()
  %>% apply( 1L
           , function( v ) { # for all positions
               apply( adjacency # for all directions
                    , 1L
                    , find_seatmap_first_visible
                    , from = v
                    , seatmap = seatmap
                    )
             }
           )
  )
}

reseat_seatmap_11b <- function( seatmap, adjmap ) {
  fmap <- function( x, y ) {
    loc <- x + ( y - 1L ) * ( nrow( seatmap ) - 2L )
    adj <- seatmap[ na.omit( adjmap[ , loc ] ) ]
    empties <- sum( 2L == adj )
    fulls <- sum( 3L == adj )
    here <- seatmap[ x+1L, y+1L ]
    if ( 2L == here ) { # this seat is empty
      if ( 0L == fulls ) 3L # no full seats? full
      else here
    } else if ( 3L == here ) { # this location is full
      if ( 5L <= fulls ) 2L # 5 or more full? empty
      else here
    } else here
  }
  fmapv <- Vectorize( fmap, c( "x", "y" ) )
  rseq <- seq.int( nrow( seatmap ) - 2L )
  cseq <- seq.int( ncol( seatmap ) - 2L )
  seatmap[ rseq + 1L, cseq + 1L ] <- outer( rseq, cseq, fmapv )
  seatmap
}

# Day 12 ----

parse_traj <- function( lns ) {
  trav_dta <- read.table( text =
"dir is_abs  ang
N      TRUE   1
E      TRUE   0
W      TRUE   2
S      TRUE   3
L      FALSE  1
R      FALSE  3
F      FALSE  0
", header = TRUE, as.is=TRUE )
  result <- data.frame( lns = lns
                      , dir = substr( lns, 1, 1 )
                      , mag = as.integer( substr( lns, 2, nchar( lns ) ) )
                      , stringsAsFactors = FALSE
                      )
  inner_join( result, trav_dta, by = "dir" )
}

traverse_12a <- function( traj, initial_ang = 0L, initial_pos = c( 0L, 0L ) ) {
  rel_pos <- matrix( c(  1L,  0L
                      ,  0L,  1L
                      , -1L,  0L
                      ,  0L, -1L
                      )
                   , ncol = 2L
                   , byrow = TRUE
                   )
  positions <- matrix( NA_integer_, nrow = nrow( traj ), ncol = 2L )
  angs <- integer( nrow( traj ) )
  for ( i in seq_along( traj$ang ) ) {
    if ( traj$is_abs[ i ] ) {
      d_pos <- traj$mag[ i ] * rel_pos[ traj$ang[ i ] + 1L, ]
    } else if ( "F" == traj$dir[ i ] ) {
      d_pos <- traj$mag[ i ] * rel_pos[ initial_ang + 1L, ]
    } else {
      initial_ang <- ( initial_ang
                     + ifelse( "L"==traj$dir[ i ], 1L, -1L )
                       * traj$mag[ i ] %/% 90L
                     ) %% 4L
      d_pos <- c( 0, 0 )
      stopifnot( 0 == ( traj$mag[ i ] %% 90L ) )
    }
    initial_pos <- initial_pos + d_pos
    positions[ i, ] <- initial_pos
    angs[ i ] <- initial_ang
  }
  traj$pos_x <- positions[ , 1L ]
  traj$pos_y <- positions[ , 2L ]
  traj$pos_ang <- angs
  traj
}

get_final_manhattan <- function( trav ) {
  with( trav[ nrow( trav ), ], abs( pos_x ) + abs( pos_y ) )
}

ref_dta_12b <- list(
  rel_left = array( c(  1,  0 # 0
                     ,  0,  1
                     ,  0,  1 # 90
                     , -1,  0
                     , -1,  0 # 180
                     ,  0, -1
                     ,  0, -1 # 270
                     ,  1,  0
                     )
                  , dim = c( 2L, 2, 4L )
                  )
)

get_rot90_matrix <- function( dir, mag ) {
  mul <- ifelse( "L" == dir, 1L, -1L )
  idx <- ( 1L
         + ( ( mul * mag ) %/% 90L ) %% 4L
         )
  ref_dta_12b$rel_left[ , , idx ]
}

traverse_12b <- function( traj
                        , initial_pos = c( 0L, 0L )
                        , initial_wp = c( 10L, 1L )
                        ) {
  rel_pos <- matrix( c(  1L,  0L
                      ,  0L,  1L
                      , -1L,  0L
                      ,  0L, -1L
                      )
                   , ncol = 2L
                   , byrow = TRUE
                   )
  positions <- matrix( NA_integer_, nrow = 2L, ncol = nrow( traj ) )
  wp_positions <- positions
  for ( i in seq_along( traj$ang ) ) {
    if ( traj$is_abs[ i ] ) {
      initial_wp <- initial_wp + traj$mag[ i ] * rel_pos[ traj$ang[ i ] + 1L, ]
    } else if ( "F" == traj$dir[ i ] ) {
      initial_pos <- initial_pos + traj$mag[ i ] * initial_wp
    } else {
      initial_wp <- as.vector( get_rot90_matrix( dir = traj$dir[ i ]
                                               , mag = traj$mag[ i ]
                                               )
                             %*% initial_wp
                             )
      stopifnot( 0 == ( traj$mag[ i ] %% 90L ) )
    }
    positions[ , i ] <- initial_pos
    wp_positions[ , i ] <- initial_wp
  }
  traj$pos_x <- positions[ 1L, ]
  traj$pos_y <- positions[ 2L, ]
  traj$wp_x <- wp_positions[ 1L, ]
  traj$wp_y <- wp_positions[ 2L, ]
  traj
}

# Day 13 ----

parse_sched_13 <- function( lns ) {
  earliest <- as.integer( lns[ 1 ] )
  ids <- scan( text = lns[ 2 ], sep = ",", what = character(0), quiet = TRUE )
  ids <- suppressWarnings( as.integer( ids ) )
  idxs <- which( !is.na( ids ) )
  list( earliest = earliest
      , ids = ids[ idxs ]
      , idxs = idxs
      )
}

find_next_bus_13a <- function( sched ) {
  delay <- sched$ids - ( sched$earliest %% sched$ids )
  idx <- which.min( delay )
  id <- sched$ids[ idx ]
  list( id = id
      , delay = delay[ idx ]
      )
}

#' Extended Euclidean gcd
#' 
#' @param a Integer
#' @param b Integer
#' @return List of result integers:
#'   \describe{
#'     \item{gcd}{Integer, greatest common denominator}
#'     \item{bezout_a}{Integer, Bezout coefficient for a}
#'     \item{bezout_b}{Integer, Bezout coefficient for b}
#'   }
egcd <- function( a, b ) {
  is_a_less_b <- a < b
  if ( is_a_less_b ) {
    r0 <- b
    r1 <- a
  } else {
    r0 <- a
    r1 <- b
  }
  #r0 <- max( a, b ); r1 <- min( a, b )
  s0 <- 1L; s1 <- 0L
  t0 <- 0L; t1 <- 1L
  while( 0L != ( r <- r0 %% r1 ) ) {
    q <- r0 %/% r1
    s <- s0 - q * s1
    t <- t0 - q * t1
    s0 <- s1
    s1 <- s
    t0 <- t1
    t1 <- t
    r0 <- r1
    r1 <- r
  }
  list( gcd = r1
      , bezout_a = if ( is_a_less_b ) t else s
      , bezout_b = if ( is_a_less_b ) s else t
      )
}

find_earliest_13b_pair <- function( a, b, ra, rb ) {
  g <- egcd( a, b )
  ans0 <- rb * g$bezout_a * a + ra * g$bezout_b * b
  stopifnot( 1 == g$gcd )
  ab <- a * b
  mab <- max( a, b )
  ans1 <- ans0 %% ab
  if ( ans1 < mab ) 
    ans1 + ab
  else
    ans1
}

find_earliest_13b <- function( sched ) {
  n <- sched$ids
  last_n <- as.bigz( n[ 1L ] )
  r <- ( 1L - sched$idxs ) %% sched$ids
  last_r <- as.bigz( r[ 1L ] )
  for ( i in seq( 2L, length( n ) ) ) {
    N <- as.bigz( n[ i ] )
    R <- as.bigz( r[ i ] )
    ans <- find_earliest_13b_pair( last_n
                                 , N
                                 , last_r
                                 , R
                                 )
    last_n <- last_n * N
    last_r <- ans %% last_n
  }
  ans
}

# Day 14 ----

bigz2numericv <- function( z, size ) {
  normalize_numericv( as.numeric( charToRaw( as.character( z, b = 2 ) ) ) - 48, size )
}

numericv2bigz <- function( v ) {
  as.bigz( paste0( "0b", rawToChar( as.raw( 1 * v + 48 ) ) ) )
}

normalize_numericv <- function( v, size ) {
  c( rep( 0, size - length( v ) ), v )
}

numericv_AndNA <- function( v1, v2na ) {
  v2na[ is.na( v2na ) ] <- 1
  v1 & v2na
}

numericv_OrNA <- function( v1, v2na ) {
  v2na[ is.na( v2na ) ] <- 0
  v1 | v2na
}

numericv_Not <- function( v1 ) {
  ifelse( v1, 0, 1 )
}

idx2numericv <- function( idx, size ) {
  v <- rep( 0, size )
  v[ rev( idx[ 0 != idx ] ) ] <- 1
  v
}

bigz_bitV <- function( x, y, size ) {
  #https://stackoverflow.com/questions/50077077/bitwise-operations-with-bigz-in-gmp
  #express as numeric vectors of 0s and 1s
  x1 <- bigz2numericv( x, size )
  y1 <- bigz2numericv( y, size )
  
  list( x = x1
      , y = y1
      )
}

bigz_bitAnd <- function( x, y, size ) {
  vlist <- bigz_bitV( x, y, size )
  numericv2bigz( vlist$x & vlist$y )
}

bigz_bitOr <- function( x, y, size ) {
  vlist <- bigz_bitV( x, y, size )
  numericv2bigz( vlist$x | vlist$y )
}

mem_p14a <- function( size ) {
  mem_hash <- hash::hash()
  mask <- rep( NA, size )
  list( write = function( addr, z ) {
                  z <- (   bigz2numericv( z, size )
                       %>% numericv_OrNA( mask ) # set 1 bits
                       %>% numericv_AndNA( mask ) # set 0 bits
                       %>% numericv2bigz()
                       )
                  mem_hash[[ as.character( addr ) ]] <<- z
                }
      , set_mask = function( v ) {
                     mask <<- v
                   }
      , get_ram = function() {
                    as.list( mem_hash )
                  }
      )
}

parse_mask_14a <- function( s ) {
  suppressWarnings( as.numeric( strsplit( s, "" )[[ 1 ]] ) )
}

parse_14a <- function( lns ) {
  (   data.frame( stmt = lns, stringsAsFactors = FALSE )
  %>% filter( "" != stmt )
  %>% mutate( lhs = sub( "^(.*) = .*$", "\\1", stmt )
            , rhs = sub( "^.* = (.*)$", "\\1", stmt )
            , op = sub( "^([^[]+).*$", "\\1", lhs )
            , addr = ifelse( "mem"==op
                           , sub( "^.*\\[(.*)\\]", "\\1", lhs )
                           , NA
                           )
            )
  )
}

#' define a virtual cpu
cpu_14a <- function( size, memory = mem_p14a( size ) ) {
  list( ops = list( mask = function( addr, arg1 ) memory$set_mask( parse_mask_14a( arg1 ) )
                  , mem = function( addr, arg1 ) memory$write( as.bigz( addr )
                                                           , as.bigz( arg1 )
                                                           )
                  )
      , get_ram = function() {
          memory$get_ram()
        }
  )
}

interpret_p14 <- function( code, cpu = cpu_14a( 36 ) ) {
  (   code
  %>% select( op, addr, rhs )
  %>% pwalk( function( op, addr, rhs ) {
               cpu$ops[[ op ]]( addr, rhs )
             }
           )
  )
  Reduce( `+`, cpu$get_ram() )
}

mem_p14b <- function( size ) {
  mem_hash <- hash::hash()
  mask <- list()
  mask_ix <- rep( 0, size )
  list( write = function( addr, z ) {
                  if ( 0 < length( mask ) ) {
                    # set addr bits to zero where NAs are found in mask
                    addrz <- (   addr
                             %>% bigz2numericv( size )
                             %>% numericv_AndNA( numericv_Not( mask_ix ) )
                             )
                    # Or each possible mask with addrz and write it to memory
                    walk( mask
                        , function( . ) {
                            k <- (   numericv_OrNA( ., v2na = addrz ) # set 1 bits
                                 %>% numericv2bigz()
                                 %>% as.character()
                                 )
                            mem_hash[[ k ]] <- z
                          }
                        )
                    invisible(NULL)
                  } else {
                    mem_hash[[ addr ]] <- z
                  }
                }
      , set_mask = function( v ) {
                     mask_ix <<- is.na( v )
                     v0 <- v
                     v0[ mask_ix ] <- 0
                     mask <<- (   v
                              %>% is.na()
                              %>% which()
                              %>% c( ., rep( 0, length( . ) ) )
                              %>% matrix( nrow = 2, byrow = TRUE )
                              %>% as.data.frame()
                              %>% expand.grid()
                              %>% t()
                              %>% as.data.frame()
                              %>% map( idx2numericv, size = size )
                              %>% map( function( z ) z | v0 )
                              )
                   }
      , get_ram = function() {
          as.list( mem_hash )
        }
      )
}
