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
  R <- seq( 1, row_max, by = row_delta ) # sequence of row values
  # sequence of corresponding column values
  C <- 1L + col_delta * ( seq.int( length( R ) ) - 1L )
  # coordinate matrix for the trajectory
  matrix( c( 1L + ( R - 1L ) %% row_max
           , 1L + ( C - 1L ) %% col_max
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
