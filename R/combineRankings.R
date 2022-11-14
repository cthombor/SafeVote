check.ranking <- function(r) {
    rc <- rank( r, ties.method="min" )
    if( ! all( r == rc ) ) {
        warning( "ranking (", paste(r,collapse=" "), ") corrected to (",
                paste(rc,collapse=" "), ")" )
    }
    rc
}

combineRankings <- function(r1,r2) {
    if( length(r1) != length(r2) ){
        stop( "Error: can't combine rankings of different length" )
    }
    r1 <- check.ranking(r1)
    r2 <- check.ranking(r2)
    rank( pmin(r1,r2), ties.method="min" )
}
