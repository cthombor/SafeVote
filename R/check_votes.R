#' parameter-checking method for nseats (not exported)
#'
#' @param nseats initially-specified number of seats to be filled in an election
#' @param ncandidates the number of candidates standing for election
#' @param default the return value of this function when nseats=NULL
#' @param mcan a deprecated name for nseats
#' @param complete.ranking when TRUE, the return value is in 1..ncandidates
#'   When FALSE, the return value is in 1..ncandidates-1 (for backwards
#'   compatibility)
#'
#' @return a valid non-NULL value for the number of seats to be filled
#'
#' @examples
#'    ## appropriately default nseats, when testing stv() with five candidates  
#'    nc<-5
#'    nseats<-NULL
#'    nseats<-SafeVote:::check.nseats(nseats=nseats, 
#'       ncandidates=nc, default=floor(nc/2))
#' # https://www.r-bloggers.com/2021/06/documentation-for-internal-functions/ 
#
#' @examples
#'    ## appropriately default nseats, when using stv() to rank candidates 
#'    nc<-5
#'    nseats<-NULL
#'    nseats<-SafeVote:::check.nseats(nseats=nseats, ncandidates=nc, 
#'      complete.ranking=TRUE, default=nc)
#' 
check.nseats <- function(nseats = NULL, ncandidates, default=1, mcan = NULL,
                         complete.ranking = FALSE) {
  if(!is.null(mcan)) {
    if(is.null(nseats)) nseats <- mcan
    warning("Argument mcan is deprecated and will be removed in the future.", 
            "Use nseats instead. Using nseats = ", nseats)
  }
  if(is.null(nseats)) return(default)
  if( (nseats < 1 || nseats > ncandidates) ) {
    warning("Adjusting nseats (originally ", nseats, 
            ") to be in 1..", ncandidates
            )
    nseats <- max(1, min(nseats, ncandidates))
  }
  return(nseats)
}

#' undocumented internal method
#' @param record,equal.ranking,... undocumented
#' @return undocumented
check.votes.stv <- function(record, equal.ranking = FALSE, ...) {
  if(any(! (record %in% 0:length(record))) || ! 1 %in% record) {
    return(FALSE)
  }
  if(!equal.ranking){
    z <- sort(diff(c(0, diff(sort(c(0, record))), 1)))
    return(z[length(record)] == 0 && z[length(record) + 1] == 1)
  }
  # check for equal ranking
  return(all(record[record > 0] == rank(record[record > 0], 
                                        ties.method = "min")))
}

#' undocumented internal method
#' @param record,...  undocumented
#' @return undocumented
check.votes.condorcet <- function(record, ...) {
  if(any(! (record %in% 0:length(record))) || ! 1 %in% record) return(FALSE)
  should.be <- rank(record[record > 0], ties.method = "min")
  return(all(record[record > 0] == should.be))
}

#' undocumented internal method
#' @param record,... undocumented
#' @return undocumented
check.votes.approval <- function(record, ...) {
  return(all(record %in% c(1,0)))
}

#' undocumented internal method
#' @param record,... undocumented
#' @return undocumented
check.votes.plurality <- function(record, ...) {
  return(all(record %in% c(1,0)) && sum(record) == 1)
}

#' undocumented internal method
#' @param record,max.score,... undocumented
#' @return undocumented
check.votes.score <- function(record, max.score, ...) {
  return(all(record %in% 0:max.score))
}

#' undocumented internal method
#' @param record,... undocumented
#' @return undocumented
check.votes.tworound.runoff <- function(record, ...) {
  return(check.votes.stv(record, ...))
}

#' undocumented internal method
#' @param x,method,... undocumented
#' @return undocumented
is.valid.vote <- function(x, method, ...) {
  return(apply(x, 1, paste0("check.votes.", method), ...))
}

#' undocumented internal method
#' @param x,quiet,... undocumented
#' @return undocumented
check.votes <- function(x, ..., quiet = FALSE) {
  ok <- is.valid.vote(x, ...)
  if(any(!ok) && !quiet)
    cat("Detected ", sum(!ok), "invalid votes. Number of valid votes is", sum(ok), ".\nUse invalid.votes(...) function to view discarded records.\n")
  return(x[ok, ])
}

#' undocumented internal method
#' @param x,max.score,... undocumented
#' @return undocumented
assemble.args.for.check.score <- function(x, max.score = NULL, ...) {
  if(is.null(max.score)  || max.score < 1) max.score <- max(x)
  return(list(max.score=max.score))
}

#' undocumented internal method
#' @param x,equal.ranking,... undocumented
#' @return undocumented
assemble.args.for.check.stv <- function(x, equal.ranking = FALSE, ...) {
  return(list(equal.ranking=equal.ranking))
}

#' Coerce input 'data' into a matrix
#' @param data possibly a .csv file, possibly an R object
#' @param fsep separation character for .csv e.g. tab or comma
#' @return a matrix with one row per ballot, one column per candidate, with
#'   named rows and columns
#' @importFrom utils read.table
prepare.votes <- function(data, fsep="\n") {
  if (is.character(data)) {
    data <- utils::read.table(file = data, header = TRUE, 
                              sep=fsep, row.names = NULL)
  }
  x <- as.matrix(data)
  ##TODO: see if data.table offers any performance improvement over matrix, e.g.
  ## through multithreading
  x[is.na(x)] <- 0
  if (is.null(colnames(x))) {
    warning("Candidate names not supplied, dummy names used instead.")
    colnames(x) <- LETTERS[1:ncol(x)]
  }
  if (is.null(rownames(x))) {
    rownames(x) <- 1:nrow(x)
  }
  return(x)
}

#' undocumented internal method
#' @param votes,partial,quiet undocumented
#' @return undocumented
correct.ranking <- function(votes, partial = FALSE, quiet = FALSE){
  do.rank <- function(x){
    res <- rep(0, length(x))
    res[x > 0] <- rank(x[x > 0], ties.method = "min")
    res
  }
  do.partial <- function(x) {
    d <- diff(sort(c(0, x))) # handle gaps in ranking
    if(any(d > 1)) {
      r <- x[names(d[d > 1][1])]
      x[x >= r] <- 0
    }
    d <- which(duplicated(x) & x > 0)
    if(length(d) > 0) x[x >= min(x[d])] <- 0
    return(x)
  }
  if(partial) {
    fct <- do.partial
    wrn <- "partially corrected (some may be still invalid)"
  } else {
    fct <- do.rank
    wrn <- "corrected to comply with the required format"
  }
  if(partial) do.partial else do.rank
  v <- t(apply(votes, 1, fct))
  dif <- rowSums(v != votes)
  if(any(dif > 0) && !quiet) warning("Votes ", paste(which(dif>0), collapse = ", "), " were ", wrn, ".\n")
  colnames(v) <- colnames(votes)
  rownames(v) <- rownames(votes)
  return(v)
}

#' undocumented internal method
#' @param votes,can,quiet undocumented
#' @return undocumented
remove.candidate <- function(votes, can, quiet = TRUE){
  if(!all(can %in% colnames(votes)) || (is.numeric(can) && !all(can %in% 1:nrow(votes))))
    stop("Value(s) of can not found in the set of candidates.")
  if(is.numeric(can))
    votes <- votes[,-can, drop = FALSE]
  else
    votes <- votes[,!colnames(votes) %in% can]
  return(correct.ranking(votes, quiet = quiet))
}
