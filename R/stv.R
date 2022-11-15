#' Count preferential ballots using an STV method
#'
#' @param votes an array with one column per candidate and one row per ballot
#' @param nseats the number of seats to be filled in this election
#' @param eps fuzz-factor when comparing fractional votes
#' @param equal.ranking TRUE if ballots are allowed to rank candidates equally
#' @param fsep column-separator for output
#' @param ties vector of tie-breaking methods: 'f' for forward, 'b' for backward
#' @param constant.quota TRUE if quota is held constant.  Over-rides quota.hare.
#' @param quota.hare TRUE if Hare quota, FALSE if Droop quota
#' @param group.nseats number of seats reserved to members of a group, as in the
#'   Church of England's STV methodology.
#' @param group.members vector of members of a group with reserved seats
#' @param complete.ranking TRUE if the balloting is intended to produce a
#'   ranking of all candidates.  This affects the value assigned to nseats when
#'   stv() is called with nseats=NULL
#' @param invalid.partial TRUE if ballots are invalid (aka "informal") if they
#'   do not specify a complete ranking of candidates
#' @param verbose TRUE for diagnostic output
#' @param seed integer seed for the (default) RNG in this instance of R
#' @param quiet TRUE to suppress console output
#' @param digits number of significant digits in the output table
#' @param safety number of standard deviations on vote-counts, when producing a
#'   safeRank by clustering near-ties in a complete ranking
#' @param ... undocumented
#'
#' @return object of class vote.stv
#' @export
#'
#' @examples data(food_election)
#' @examples stv(food_election, complete.ranking=TRUE, safety=0.5)
#' @examples stv(food_election, nseats = NULL, eps = 0.001,
#'   equal.ranking = FALSE, fsep = "\t", ties = c("f", "b"), 
#'   constant.quota = FALSE, quota.hare = FALSE, 
#'   group.nseats = NULL, group.members = NULL,
#'   complete.ranking = FALSE, invalid.partial = FALSE, 
#'   verbose = FALSE, seed = 1234, quiet = FALSE, digits = 3)
#'   
stv <- function(votes, nseats = NULL, eps = 0.001, equal.ranking = FALSE,
                fsep = "\t", ties = c("f", "b"), constant.quota = FALSE,
                quota.hare = FALSE, group.nseats = NULL, group.members = NULL,
                complete.ranking = FALSE, invalid.partial = FALSE,
                verbose = FALSE, seed = 1234,
                quiet = FALSE, digits = 3, safety = 1.0, ...) {

    if(verbose && !quiet) {
        cat("\nSingle transferable vote count")
        if(equal.ranking) cat(" with equal preferences")
        cat("\n===================================")
        if(equal.ranking) cat("==================")
        cat("\n")
    }

    ## Prepare by finding names of candidates, setting up vector w of
    ## vote weights and list of elected candidates,
    ## and declaring a vector of candidate ranks (recorded in order of
    ## selection, with deletions occupying the lowest ranks)
    votes <- prepare.votes(votes, fsep=fsep)
    nc <- ncol(votes)
    cnames <- colnames(votes)
    if( complete.ranking ){
        ## hack on check.nseats() args to allow a complete ranking by
        ## STV without any eliminations
        nseats <- check.nseats(nseats, ncandidates=nc, default=nc,
                               complete.ranking = complete.ranking)
    } else {
        nseats <- check.nseats(nseats, ncandidates=nc, default=floor(nc/2))
    }

    ## check groups (if used)
    use.marking <- FALSE
    if(!is.null(group.nseats)) {
        ## number of candidates to be elected from a group
        if(is.null(group.members)) {
            stop("If group.nseats is given, argument group.members must be ",
                 "used to mark members of the group.")
        }
        if(group.nseats > nseats) {
            warning("group.nseats must be <= nseats. ",
                    "Adjusting group.nseats to ", nseats, ".")
            group.nseats <- nseats
	}
        if(length(group.members) < group.nseats) {
            warning("There are fewer group members than group.nseats. ",
                    "Adjusting group.nseats to ",
                    length(group.members), ".")
            group.nseats <- length(group.members)
	}
        if(!is.numeric(group.members)) {
            ## convert names to indices
            gind <- match(group.members, cnames)
            if(any(is.na(gind))) {
                warning("Group member(s) ",
                        paste(group.members[is.na(gind)], collapse = ", "),
                        " not found in the set of candidates, ",
                        "therefore removed from the group.")
                gind <- gind[!is.na(gind)]
            }
            group.members <- gind
        }
	## now group memebers are given as indices
	group.members <-
            unique(group.members[group.members <= nc & group.members > 0])
        use.marking <- TRUE
    } else {
        group.nseats <- 0
        group.members <- c()
    }

    elected <- NULL
    eliminated <- NULL

    ##
    ## The next step is to remove invalid votes. A vote is invalid if
    ## the preferences are not numbered in consecutively increasing order.
    ## A warning is printed out for each invalid vote, but the votes are
    ## not counted. If necessary, it is possible to correct errors in the
    ## original x matrix.
    ##
    ## If x is generated from an excel spreadsheet, then the jth vote will
    ## be in row (j-1) of the spreadsheet.
    ##

    if(verbose && !quiet) cat("Number of votes cast is", nrow(votes), "\n")
    corvotes <- votes
    corrected.votes <- NULL

    if (equal.ranking) {
      corvotes <- correct.ranking(votes, partial = FALSE, quiet = quiet)
    } else {
      if (invalid.partial) {
        corvotes <- correct.ranking(votes, partial = TRUE, quiet = quiet)
      }    
      
      x <- check.votes(corvotes, "stv", equal.ranking = equal.ranking,
                     quiet = quiet)
      corrected <-
        which(rowSums(corvotes != votes) > 0 &
                rownames(votes) %in% rownames(x))
      
      if( length(corrected) > 0 ) {
        corrected.votes <-
          list(original = votes[corrected, ],
               new = corvotes[corrected,],
               index = as.numeric(corrected))
      }
      
    }

    nvotes <- nrow(x)
    if( is.null(nvotes) || nvotes == 0 ) {
        stop("There must be more than one valid ballot to run STV.")
    }
    w <- rep(1, nvotes)

    ## Create elimination ranking
    tie.method <- match.arg(ties)
    tie.method.name <- c(f = "forwards", b = "backwards")
    otb <- ordered.tiebreak(x, seed)

    if(use.marking) {
        if(verbose && !quiet) {
           cat("Number of reserved seats is", group.nseats, "\n")
            cat("Eligible for reserved seats:",
                paste(cnames[group.members], collapse = ", "), "\n")
        }
        group.nseats.orig <- group.nseats
    }

    ## initialize results
    result.pref <-
        result.elect <-
            matrix(NA, ncol=nc, nrow=0, dimnames=list(NULL, cnames))
    result.quota <-
        result.ties <-
            c()
    result.ranks <- rep(NA,nc)
    names(result.ranks) <- cnames
    result.margins <- rep(NA,nc)
    names(result.margins) <- cnames
    orig.x <- x

    ##
    ## the main loop
    ##
    if(verbose && !quiet) {
        cat("\nList of 1st preferences in STV counts: \n")
    }

    count <- 0
    inPlay <- rep(TRUE,nc)
    while(nseats > 0) {
      ## calculate quota and total first preference votes
      count <- count + 1
      A <- (x == 1)/rowSums(x == 1) ## splits 1st votes if there are
      ## more than one first ranking
      ## per vote
      A[is.na(A)] <- 0
      uij <- w * A
      vcast <- apply(uij, 2, sum)
      names(vcast) <- cnames
      if(!constant.quota || count == 1) {
        ## Quota calculation via either Hare (quota.hare is TRUE)
        ## or Droop (FALSE) method
        quota <- if(quota.hare)
          sum(vcast)/nseats + eps
        else
          sum(vcast)/(nseats + 1) + eps
      }
      result.quota <- c(result.quota, quota)
      result.pref <- rbind(result.pref, vcast)
      result.elect <- rbind(result.elect, rep(0,nc))
      tie <- 0
      if(verbose && !quiet) {
        cat("\nCount:" , count, "\n")
        df <- data.frame(
          QUOTA=round(quota, 3), t(round(vcast[vcast != 0], 3)))
        rownames(df) <- count
        print(df)
      }
      
      ## if leading candidate exceeds quota, declare elected and
      ## adjust weights
      ##
      ## mark candidate for elimination in subsequent counting
      ##
      ## if the number of remaining candidates is less than or equal
      ## to the number of seats, then select the one with the
      ## largest vcast, no matter if quota is exceeded
      ##
      vmax <- max(vcast)
      ic <- (1:nc)[vcast == vmax]
      D <- colSums(abs(result.elect)) == 0 ## set of hopeful candidates
      if(use.marking){
        Dm <- D
        Dm[-group.members] <- FALSE ## set of hopeful marked candidates
      }

      if ((vmax >= quota &&
           
           !(!any(ic %in% group.members) &&
             nseats == group.nseats) ||
           ## with constant.quota, elected candidates may not need
           ## to reach quota
           
           (constant.quota && sum(D) <= nseats)) ||
          
          (use.marking &&
           any(ic %in% group.members) &&
           (sum(Dm) <= group.nseats || sum(D) - sum(Dm) == 0))) {
        if( use.marking && length(ic) > 1 && sum(Dm) <= group.nseats ) {
          ## if a tiebreak, choose marked candidates if needed
          ic <- ic[ic %in% group.members]
        }
        
        if( length(ic) > 1 ) {
          ## tie
          ic <- solveTiebreak(tie.method, result.pref, ic, otb,
                              elim = FALSE)
          tie <- 1
          tie <- tie + (attr(ic, "ordered") == TRUE)
          tie <- tie + (attr(ic, "sampled") == TRUE)
        }
        surplus <- if (vmax > quota)
          (vmax - quota) / vmax
        else
          0
        index <- (x[, ic] == 1) ## ballots where ic has the first preference
        w[index] <- uij[index, ic] * surplus ## update weights
        if (equal.ranking) {
          w[index] <-
            w[index] +
            rowSums(uij[index, , drop = FALSE]) -
            uij[index, ic]
        }
        
        ## reduce number of seats available
        nseats <- nseats - 1
        if (use.marking && ic %in% group.members) {
          group.nseats <- group.nseats - 1
        }
        elected <- c(elected, cnames[ic])
        result.elect[count, ic] <- 1
        
        result.ranks[ic] <- length(elected)
        wm <- winnerMargin(vcast[inPlay])
        stopifnot(
          ## regression test on tie-breaking method: ic
          ## must have maximal votes
          vcast[ic] == vcast[names(wm[1])]
          )
        result.margins[ic] <- wm[2] ## note: margin may be NA
        inPlay[ic] <- FALSE
        if (verbose && !quiet) {
          cat("Candidate", cnames[ic],
              "elected with margin", result.margins[ic])
          if (tie > 0) {
            cat("using", tie.method.name[tie.method])
            if (tie == 2)
              cat(" & ordered")
            cat(" tie-breaking method ")
            if (tie > 2)
              cat("(sampled)")
            
          }
          cat("\n")
        }
      } else {
        ## if no candidate reaches quota, mark lowest candidate
        ## for elimination
        elim.select <- D
        if (use.marking &&
            (nseats == group.nseats ||
             sum(Dm) <= group.nseats)) {
          elim.select <- elim.select & !Dm
        }
        vmin <- min(vcast[elim.select])
        ic <- (1:nc)[vcast == vmin & elim.select]
        if (length(ic) > 1) {
          ## tie
          ic <-
            solveTiebreak(tie.method, result.pref, ic, otb,
                          elim = TRUE)
          tie <- 1
          tie <- tie + (attr(ic, "ordered") == TRUE)
          tie <- tie + (attr(ic, "sampled") == TRUE)
        }
        result.elect[count, ic] <- -1
        eliminated <- c(eliminated, cnames[ic])
        nEliminated <- length(eliminated)
        result.ranks[ic] <- nc - nEliminated
        
        lm <- loserMargin(vcast[inPlay])
        stopifnot(## regression test on tie-breaking method: ic
          ## must have minimal votes
          vcast[ic] == vcast[names(lm[1])])
        result.margins[ic] <- lm[2] ## note: margin may be NA
        inPlay[ic] <- FALSE
        
        if (verbose && !quiet) {
          cat("Candidate",
              cnames[ic],
              "eliminated with margin",
              result.margins[ic])
          if (tie > 0) {
            cat("using", tie.method.name[tie.method])
            if (tie == 2)
              cat(" & ordered")
            cat(" tie-breaking method ")
            if (tie > 2)
              cat("(sampled)")
          }
          cat("\n")
        }
      }
      result.ties <- c(result.ties, tie)
      ## shift votes for voters who voted for ic
      jp <- x[, ic]
      for (i in which(jp > 0)) { ## TODO maybe vectorise this loop?
        index <- x[i,] > jp[i]
        x[i, index] <- x[i, index] - 1
      }
      
      x[, ic] <- 0
      
    }
    
    rownames(result.pref) <- 1:count
    partialResult <- structure(
        list(elected = elected, preferences = result.pref,
             quotas = result.quota, elect.elim = result.elect,
             ranking = result.ranks,
             margins = result.margins,
             fuzz = safety*sqrt(nrow(orig.x)), ## for safeRank
             equal.pref.allowed = equal.ranking,
             ties = translate.ties(result.ties, tie.method),
             data = orig.x,
             invalid.votes =
                 votes[setdiff(rownames(votes), rownames(x)),,drop = FALSE],
             corrected.votes = corrected.votes,
             reserved.seats = if(use.marking) group.nseats.orig else NULL,
             group.members = if(use.marking) group.members else NULL
             )
    )

    crt <- completeRankingTable(partialResult)
    sr <- crt$SafeRank
    names(sr) <- crt$Candidate
    result <- structure(
        append(
            partialResult,
            list( rankingTable=crt, safeRank=sr )
        ),
        class = "SafeVote.stv"
    )
    ##TODO: define and use an explicit constructor for SafeVote objects

    if(!quiet) {
        print(summary(result, digits = digits))
    }
    invisible(result)
}

#' Find a winner and their margin of victory 
#'
#' @param votes cleaned ballots
#'
#' @return length-2 vector: the index of a winning candidate, and their margin
#'         of victory (which is 0 in the case of a tie)
#'         
winnerMargin <- function(votes) {
  if( length(votes) == 0 ) {
    warning("winnerMargin() was called on an empty set of votes")
    return( c(NA, NA) )
  }
  winner <- which( votes == max(votes) )
  if( length(votes) > length(winner) ) {
    margin <- max(votes) - max( votes[-winner] )
  } else {
    margin <- NA
  }
  return( c(winner, margin) )
}

#' Find a loser and their margin of victory 
#'
#' @param votes cleaned ballots
#'
#' @return length-2 vector: the index of a losing candidate, and their margin
#'         of loss (which is 0 in the case of a tie)
#'   
loserMargin <- function(votes) {
  if( length(votes) == 0 ) {
    warning("loserMargin() was called on an empty set of votes")
    return( c(NA, NA) )
  }
  loser <- which( votes == min(votes) )
  if( length(votes) > length(loser) ) {
    margin <- min( votes[-loser] ) - min(votes)
  } else {
    margin <- NA
  }
  return( c(loser, margin) )
}

#' Undocumented internal method from original code
#'
#' @param ties undocumented
#' @param method 'f' for forward, 'b' for backward 
#'
#' @return undocumented
#'
translate.ties <- function(ties, method){
    ties.char <- ifelse(ties == 0, "", method)
    ties.char <- ifelse(ties > 1, paste0(ties.char, "o"), ties.char)
    ties.char <- ifelse(ties > 2, paste0(ties.char, "s"), ties.char)
    names(ties.char) <- 1:length(ties)
    return(ties.char)
}

#' Undocumented internal method, renamed from 'solve.tiebreak' to avoid 
#' confusion with generic solve()
#'
#' @param method undocumented
#' @param prefs undocumented
#' @param icans undocumented
#' @param ordered.ranking undocumented
#' @param elim undocumented
#'
#' @return undocumented
#'
solveTiebreak <- function(method, prefs, icans,
                           ordered.ranking = NULL, elim = TRUE){
    if(method == "f") { ## forwards
        ic <- forwards.tiebreak(prefs, icans, elim = elim)
    }
    else { ## backwards
        ic <- backwards.tiebreak(prefs, icans, elim = elim)
    }
    # solve remaining ties by ordered ranking
    sampled <- FALSE
    ordered <- FALSE
    if(length(ic) > 1) {
        ic <- ic[ if(elim)
                      which.min(ordered.ranking[ic])
                  else which.max(ordered.ranking[ic])]
        sampled <- attr(ordered.ranking, "sampled")[ic]
        ordered <- TRUE
    }
    attr(ic, "sampled") <- sampled
    attr(ic, "ordered") <- ordered
    return(ic)
}

#' Undocumented internal method
#'
#' @param vmat undocumented
ordered.preferences <- function(vmat) {
    sapply(1:ncol(vmat),
           function(pref) apply(vmat, 2, function(f) sum(f == pref)))
}

#' Undocumented internal method
#'
#' @param vmat undocumented
#' @param seed undocumented
ordered.tiebreak <- function(vmat, seed = NULL) {
    ## Create elimination ranking using ordered tie-breaking element
    ## ij in matrix nij is the number of j-th preferences for
    ## candidate i
    nij <- ordered.preferences(vmat)
    ## ranking for each preference
    nij.ranking <- apply(nij, 2, rank, ties.method="min")
    rnk <- nij.ranking[,1]
    dpl <- duplicated(rnk) | duplicated(rnk, fromLast = TRUE)
    sampled <- rep(FALSE, length(rnk))
    ## resolve ranking duplicates by moving to the next column
    if(any(dpl)) {
        if(!is.null(seed)) set.seed(seed)
        for(pref in 1:ncol(vmat)) {
            if(! pref %in% rnk[dpl]) next
            j <- 2
            rnk[rnk == pref] <- NA
            while(any(is.na(rnk))) {
                ## which candidates to resolve
                in.game <- is.na(rnk)
                ## if we moved across all columns and there are still
                ## duplicates, determine the rank randomly
                if(j > ncol(nij)) {
                    rnk[in.game] <- sample(sum(in.game)) + pref - 1
                    sampled <- sampled | in.game
                    break
                }
                rnk[in.game] <-
                    rank(nij.ranking[in.game, j], ties.method="min") +
                    pref - 1
                dplj <-
                    rnk == pref &
                    (duplicated(rnk) |
                     duplicated(rnk, fromLast = TRUE))
                rnk[dplj] <- NA
                j <- j + 1
            }
        }
    }
    attr(rnk, "sampled") <- sampled
    return(rnk)
}

#' Undocumented internal method
#'
#' @param prefs undocumented
#' @param icans undocumented
#' @param elim undocumented
forwards.tiebreak <- function(prefs, icans, elim = TRUE) {
    if(!elim) prefs <- -prefs
    if(is.null(dim(prefs))) dim(prefs) <- c(1, length(prefs))
    rnk <- t(apply(prefs, 1, rank, ties.method="min"))
    if(is.null(dim(rnk))) dim(rnk) <- c(1, length(rnk))
    i <- 0
    icv <- rep(FALSE, ncol(prefs))
    icv[icans] <- TRUE
    while(i < nrow(rnk) && length(icans) > 1){
        i <- i + 1
        ic.rnk <- rnk[i, icans]
        icans <- which(icv & (rnk[i, ] == min(ic.rnk)))
    }
    return(icans)
}

#' Undocumented internal method
#'
#' @param prefs undocumented
#' @param icans undocumented
#' @param elim undocumented
backwards.tiebreak <- function(prefs, icans, elim = TRUE) {
    if(!elim) prefs <- -prefs
    if(is.null(dim(prefs))) dim(prefs) <- c(1, length(prefs))
    rnk <- t(apply(prefs, 1, rank, ties.method="min"))
    if(is.null(dim(rnk))) dim(rnk) <- c(1, length(rnk))
    i <- nrow(rnk)
    icv <- rep(FALSE, ncol(prefs))
    icv[icans] <- TRUE
    while(i > 1 && length(icans) > 1){
        i <- i - 1
        ic.rnk <- rnk[i, icans]
        icans <- which(icv & (rnk[i, ] == min(ic.rnk)))
    }
    return(icans)
}

#' summary() method for a SafeVote result
#'
#' @param object undocumented
#' @param ... undocumented
#' @param digits undocumented
#' 
#' @export
#' 
summary.SafeVote.stv <- function(object, ..., digits = 3) {
    decimalplaces <- function(x) {
        ifelse(abs(x - round(x)) > .Machine$double.eps^0.5,
               nchar(sub('^\\d+\\.', '', sub('0+$', '', as.character(x)))),
               0)
    }
    ncounts <- nrow(object$preferences)
    df <- data.frame(
                matrix(NA, nrow=ncol(object$preferences)+4, ncol=2*ncounts-1),
        stringsAsFactors = FALSE)
    rownames(df) <- c("Quota", colnames(object$preferences),
                      "Tie-breaks", "Elected", "Eliminated")
    colnames(df)[1] <- 1
    idxcols <- 1
    if(ncounts > 1) {
        colnames(df)[2:ncol(df)] <-
            paste0(rep(2:ncounts, each=2), c("-trans", ""))
        idxcols <- c(idxcols, seq(3,ncol(df), by=2))
    }
    df["Quota", idxcols] <- object$quotas
    df[2:(nrow(df)-3), idxcols] <- t(object$preferences)
    ## calculate transfers
    pref <- object$preferences
    ## remove quotas for winners and compute difference
    where.winner <- which(rowSums(object$elect.elim==1)==1)
    pref[where.winner,] <-
    pref[where.winner,] -
    object$elect.elim[where.winner,]*object$quotas[where.winner]
    if(ncounts > 1) {
        tmp <- t(object$preferences[2:nrow(object$preferences),] -
                 pref[1:(nrow(pref)-1),])
        if(nrow(tmp) == 1) {
            tmp <- as.numeric(tmp) ## because of R weirdness with
                                   ## vectors and matrices (when there
                                   ## are just two counts)
        }
        df[2:(nrow(df)-3), seq(2,ncol(df), by=2)] <- tmp
    }
    ## format the right number of digits
    df[1:(nrow(df)-3),] <-
        apply(df[1:(nrow(df)-3), , drop = FALSE], 2,
            function(d) {
                ifelse(!is.na(d),
                       format(round(d, digits),
                              nsmall =
                                  min(digits,
                                      max(
                                          decimalplaces(round(d[!is.na(d)], digits))

                                         )
                                     )
                             ),
                       ""
                       )
            }
        )
    where.elim <- which(rowSums(object$elect.elim==-1)==1)
    cnames <- colnames(object$elect.elim)
    for(i in 1:ncounts) {
        if (i %in% where.winner) {
            elected <- cnames[which(object$elect.elim[i,]==1)]
            df["Elected", idxcols[i]] <- paste(elected, collapse=", ")
            for(can in elected) {
                if (idxcols[i]+2 <= ncol(df)) {
                    df[can, (idxcols[i]+2):ncol(df)] <- NA

                }
            }

        }
        if (i %in%  where.elim) {
            eliminated <-cnames[which(object$elect.elim[i,]==-1)]
            df["Eliminated",idxcols[i]] <- paste(eliminated, collapse=", ")
            for(can in eliminated) {
                if(idxcols[i]+2 <=  ncol(df)) {
                    df[can, (idxcols[i]+2):ncol(df)] <- NA
                }
            }
        }
    }

    if(any(object$ties != "")) {
        df["Tie-breaks", seq(1, ncol(df), by = 2)] <- object$ties
    }
    else {
        df <- df[-which(rownames(df) == "Tie-breaks"),, drop = FALSE]
    }
    if(!is.null(object$reserved.seats)) {
        rownames(df)[object$group.members + 1] <-
            paste0(rownames(df)[object$group.members + 1], "*")
    }
    df[is.na(df)] <- ""
    class(df) <- c('summary.SafeVote.stv', class(df))
    attr(df, "number.of.votes") <- nrow(object$data)
    attr(df, "number.of.invalid.votes") <- nrow(object$invalid.votes)
    attr(df, "number.of.candidates") <- ncol(object$preferences)
    attr(df, "number.of.seats") <- length(object$elected)
    if(!is.null(object$reserved.seats)) {
        attr(df, "reserved.seats") <- object$reserved.seats
        attr(df, "reservation.eligible") <- object$group.members
    }
    attr(df, "equal.pref.allowed") <- object$equal.pref.allowed
    attr(df, "rankingTable") <- object$rankingTable
    attr(df, "fuzz") <- object$fuzz

    return(df)
    }

#' print() method for a summary() of a SafeVote result
#'
#' @param x undocumented
#' @param ... undocumented
#' 
#' @import knitr
#' @export
print.summary.SafeVote.stv <- function(x, ...) {
    cat("\nResults of Single transferable vote")
    if(attr(x, "equal.pref.allowed")) cat(" with equal preferences")
    cat("\n===================================")
    if(attr(x, "equal.pref.allowed")) cat("=======================")
    election.info(x)
    if( !is.null(attr(x, "reserved.seats")) ) {
        cat("Number of reserved seats:\t", attr(x, "reserved.seats"), "\n")
        cat("Eligible for reserved seats:\t",
            length(attr(x, "reservation.eligible")), "\n")
    }
    if( !is.null(attr(x, "rankingTable")) ) {
      cat("\nFuzz on safeRank:\t", attr(x, "fuzz"))
      cat("\nComplete ranking:")
      print(knitr::kable(attr(x, "rankingTable"), 
                         align = c("r", "l", "c", "r", "l"), ...))
    }
    
    cat("\nVote transfers:")
    print(knitr::kable(x, align='r', ...))

    cat("\nElected:",
        paste(x['Elected', x['Elected',] != ""], collapse=", "),
        "\n\n")
}

#' view() method for the result of an stv() ballot-count
#' @param object undocumented
#' @param ... undocumented
#'
#' @return formatted object
#' @import formattable
#' @export
#'
view.SafeVote.stv <- function(object, ...) {
    s <- summary(object)
    formatter <-
        list(area(row=2:(nrow(s)-2),
                  col=seq(1,ncol(s), by=2)) ~ color_text("red", "red"),
             area(row=1, col=seq(1,ncol(s), by=2)) ~ color_text("blue", "blue")
             ##Quota=color_text("blue", "blue")
             )
    formattable(s, formatter, ...)
}

#' image() method for the result of an stv() ballot-count
#'
#' @param x,xpref,ypref,all.pref,proportion,... undocumented
#'
#' @return image object
#' @import grDevices graphics data.table
#' @export
#'
image.SafeVote.stv <- function(x, xpref = 2, ypref = 1,
                           all.pref = FALSE, proportion = TRUE, ...) {

    voter <- rank <- NULL # to avoid warnings of the CRAN check
    xd <- x$data
    nc <- ncol(xd)
    if(all.pref) {
        nij <- ordered.preferences(xd)[nc:1,]
        fields::image.plot(x = 1:nc, y = 1:nc, t(nij),
                   axes = FALSE, xlab = "", ylab = "",
                   col = grDevices::hcl.colors(12, "YlOrRd", rev = TRUE), ...)
        graphics::axis(3, at = 1:nc, labels = 1:nc)
        graphics::axis(2, at = 1:nc, labels = rev(colnames(xd)), tick = FALSE, las = 2)
        mtext("Ranking", side = 1, line = 0.5)
    } else {
        xdt <- data.table(xd)
        xdt[, voter := 1:nrow(xd)]
        xdtl <- data.table::melt(xdt, id.vars = "voter",
                     variable.name = "candidate", value.name = "rank")
        xdtl <- xdtl[rank %in% c(xpref, ypref)]
        if(sum(duplicated(xdtl[, c("voter", "rank"), with = FALSE])) > 0) {
            stop("Sorry, the image function is not available for ballots",
                 "with equal preferences.")
        }
        xdtw <- dcast(xdtl, voter ~ rank, value.var = "candidate")
        setnames(xdtw, as.character(xpref), "xpref")
        setnames(xdtw, as.character(ypref), "ypref")
        ctbl <- table(xdtw[, ypref], xdtw[, xpref])
        if(proportion) {
            ctbl <- ctbl/rowSums(ctbl)
            ctbl[is.na(ctbl)] <- 0
        }
        fields::image.plot(x = 1:nc, y = 1:nc, t(ctbl[nc:1,]),
                   axes = FALSE, xlab = "", ylab = "",
                   col = hcl.colors(12, "YlOrRd", rev = TRUE), ...)
        graphics::axis(2, at = nc:1, labels = rownames(ctbl), 
                       tick = FALSE, las = 1)
        text(1:nc, y = par("usr")[4], labels = colnames(ctbl),
             xpd = NA, srt = 45, adj = 0)
        mtext(paste("Preference", ypref), side = 4, line = 0.1)
        mtext(paste("Preference", xpref), side = 1, line = 0.5)
    }
}

#' plot() method for the result of an stv() ballot-count
#' @param x,xlab,ylab,point.size,... undocumented 
#' @export
plot.SafeVote.stv <- function(x, xlab = "Count", ylab = "Preferences",
                          point.size = 2, ...) {
    stopifnot(requireNamespace("ggplot2", quietly = TRUE))
    Count <- value <- selection <-
        i.value <- count.select <- Candidate <-
        i.Count <- NULL  ## to avoid warnings of the CRAN check
    ## Plot evolution of the preferences
    ## prepare data in the long format
    df <- data.table(x$preferences)
    df[, Count := 1:nrow(df)]
    dfl <- data.table::melt(df, id.vars = "Count", variable.name = "Candidate")
    dfl <- rbind(dfl, dfl[Count == 1][, Count := 0]) ## add Count 0
                                                     ## with initial
                                                     ## values
    ## dataset for plotting the quota
    dfq <- data.table(Count = 1:length(x$quotas),
                      value = x$quotas, Candidate = "Quota")

    ## dataset for plotting points of elected and eliminated candidates
    dfe <- data.table::melt(data.table(Count = 1:nrow(x$elect.elim), x$elect.elim),
                id.vars = "Count",
                variable.name = "Candidate")
    dfe <- dfe[value != 0]
    dfe[, selection := ifelse(value > 0, "elected", "eliminated")]
    dfe <- dfe[dfl, value := i.value, on = c("Count", "Candidate")]

    ## remove data after candidates are selected
    dfl[dfe, count.select := i.Count, on = "Candidate"]
    dfl <- dfl[is.na(count.select) | Count <= count.select]

    ## create plots
    g <- ggplot2::ggplot(dfl,
                         ggplot2::aes(x = as.factor(Count),
                                      y = value, color = Candidate,
                                      group = Candidate)
                         ) +
        ggplot2::geom_line()
    g <- g + ggplot2::geom_line(data = dfq,
                                ggplot2::aes(x = as.factor(Count)),
                                color = "black"
                                ) +
        ggplot2::xlab(xlab) + ggplot2::ylab(ylab)
    g <- g + ggplot2::geom_point(data = dfe,
                                 ggplot2::aes(shape = selection),
                                 size = point.size
                                 ) +
        ggplot2::ylim(range(0, max(dfl$value, dfq$value)))
    g <- g + ggplot2::annotate(geom="text",
                               x=as.factor(1),
                               y=dfq[Count == 1, value],
                               label="Quota", hjust = "right")
    g
}

#' internal method to analyse the partial results of an stv() ballot count,
#' to discover a complete ranking of all candidates.  The ranking may depend
#' on the value of nseats, because this affects how votes are transferred.
#'
#' @param object partial results 
#' @param ... undocumented, currently unused
#'
#' @return data.frame with columns Rank, Margin, Candidate, Elected, SafeRank
#'
completeRankingTable <- function(object, ...){
    cand.in.play <- colSums(abs(object$elect.elim)) == 0
    ## list the eliminated candidates in reverse order of elimination
    eliminated <- rev(
        names(unlist(apply(object$elect.elim,1,function(r) which(r<0))))
        )
    loseMargins <- object$margins[eliminated]
    winMarg = as.numeric(object$margins[object$elected])
    if( is.na(winMarg[length(object$elected)]) ){
        ## The winning margin of a candidate elected unopposed in the
        ## last round is the losing margin of the first-eliminated
        ## candidate (if any).
        if( length(loseMargins) > 0 ){
            winMarg[length(winMarg)] <- loseMargins[1]
        }
    }
    result <- data.frame(Rank = 1:length(object$elected),
                         Margin = winMarg,
                         Candidate = object$elected,
                         Elected = "x"
                         )
    if( any(cand.in.play) ) {
        ## Candidates which were neither elected nor eliminated are
        ## ranked by their position in the last round
        lastRound <- object$preferences[nrow(object$preferences), cand.in.play]
        names(lastRound) <- colnames(object$preferences)[cand.in.play]
        rnk <- rank(-lastRound, ties.method = "random")
        lastRound <- lastRound[order(rnk)]
        winMarg <- lastRound - c(lastRound[-1],NA)
        if( length(loseMargins) > 0 ){
            ## The winning margin of the last in-play candidate is the
            ## losing margin of the first-eliminated candidate (if any).
            winMarg[length(winMarg)] <- loseMargins[1]
        }
        result <-
            rbind(
                result,
                data.frame(
                    Rank = seq(max(result$Rank) + 1, length = length(rnk)),
                    Margin = as.numeric(winMarg),
                    Candidate = names(lastRound),
                    Elected = ""
                )
            )
    }

    if( length(loseMargins) > 0 ){
        ## shift losing margins onto the (pairwise) winners
        marg <- c(loseMargins[-1], NA)
        rnk <- seq(max(result$Rank) + 1, length = length(eliminated))
        result <-
            rbind(result, data.frame(row.names = rnk,
                                     Rank = rnk,
                                     Margin = marg,
                                     Candidate = eliminated,
                                     Elected = ""
                                     )
                  )
    }

    safeRank <- result$Rank
    for( i in 2:nrow(result) ){ ## iterative 1-d clustering
        if( result$Margin[i-1] < object$fuzz ) {
            safeRank[i] <- safeRank[i-1]
        }
    }

    result <- cbind(result,SafeRank=safeRank)

    return(result)
}

