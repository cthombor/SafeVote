# The functions below can be used to test the sensitivity of a partial
# ranking (e.g. SafeRank) or a complete ranking to small perturbations
# in the votes.

#' Test the sensitivity of a ranking to random deletions from a given
#' set of ballots.  Ballots are deleted until the ranking changes, or
#' until a specified number of ballots ('dlimit') have been deleted.
#'
#' @param votes A set of ballots
#' @param countMethod The name of a function which will count the
#'     ballots
#' @param countArgs List of args to be passed to countMethod (in
#'     addition to votes)
#' @param rankMethod Name of a ranking attribute in the output of
#'     countMethod
#' @param dlimit Maximum number of ballots to delete
#' @param dinc Number of ballots to be deleted in each step
#' @return A vector of the indices of ballots whose deletions affected
#'     the ranking. The vector is of zero length if the 'dlimit'
#'     termination condition had been reached without affecting the
#'     ranking.
#' @examples
#' test.deletions(food_election, countMethod="condorcet", dlimit=4)
#' test.deletions(ims_election, countMethod="stv", countArgs=c("nseats=10"))

test.deletions <- function( votes=food_election, countMethod = "condorcet",
                           countArgs = NULL, dlimit = NULL,
                           dinc = NULL, rankMethod = "SafeRank" ) {

    if( is.null(dlimit) ) {
        dlimit = 1
    }
    dlimit = min( dlimit, nrow(votes)-1 )

    if( is.null(dinc) ) {
        dinc <- round( sqrt(dlimit) )
    }

    countArgs <- append( countArgs, list(quiet=TRUE) )
    ## TODO: implement a 'quiet' parameter in this function

    deletedBallots <- c()

    cr <- do.call(countMethod,append(countArgs,list(votes=votes)))
    if(! rankMethod %in% attributes(cr)$names ) {
        stop(paste("countMethod", countMethod, "does not produce a",
                   rankMethod))
    }

    initRank <- cr[[rankMethod]]

    if( nrow(votes) != nrow(cr$data) ){
        warning(
            paste(
                nrow(votes)-nrow(cr$data),
                "informal ballots were deleted prior to testing."
            )
        )
    }
    votes <- cr$data # cleaned ballots, with rownames

    cat("Initial ranking:\n")
    print(initRank)
    cat( "\nDeleting up to", dlimit, "ballots at random:\n " )
    violFound <- FALSE

    cat("Testing progress: ")

    while( (!violFound) && (ndel <= dlimit-dinc) ) {

        ndel <- ndel + dinc

        rvn <- sample(nrow(votes),dinc)
        deletedBallots <- append(deletedBallots, rvn)
        stopifnot( ndel == length(deletedBallots) ) # regression test

        ## progress report
        if( dinc == 1 ){
            cat( paste0(" ", rownames(votes)[rvn], ",") )
        } else {
            cat( "*" )
        }

        votes <- votes[-rvn,]

        newCR <- do.call(countMethod,
                         append(countArgs,list(votes=votes))
                         )
        newRank <- newCR[[rankMethod]]

        if( identical(newRank, initRank) ) {
            if( ((ndel %/% dinc) %% 20) == 0 ) {
                cat(" ", nrow(votes), "ballots remaining\n ")
            }
        } else {
            violFound <- TRUE
            if( ((ndel %/% dinc) %% 20) != 0 ) cat("\n")
            cat("Ranking changed after", ndel, "ballots were deleted.\n")
            cat("Final ranking:\n")
            print(newRank)
        }
    }

    if( ! violFound ) {
        cat("\nNo safety violations after",
            length(deletedBallots),
            "ballot deletions.\n")
    } else {
        cat("\nSafety violation after", length(deletedBallots),
            "ballot deletions.\n")
        if( length(deletedBallots)<100 ) {
            cat("\nDeleted ballots:", deletedBallots, "\n" )
        }
    }

    result = deletedBallots
    attributes(result) <- list(finalResults=newCR)
    return(invisible(result))
}


#' Test the sensitivity of a ranking to the addition of "plumping"
#' or "tactical voting" ballots.  Ballots are added until the
#' ranking changes, or until a specified number of ballots ('alimit')
#' have been deleted.
#'
#' @param votes A set of ballots
#' @param countMethod The name of a function which will count the
#'     ballots
#' @param countArgs List of args to be passed to countMethod (in
#'     addition to votes)
#' @param rankMethod Name of a ranking attribute in the output of
#'     countMethod
#' @param favoured Name of the candidate being "plumped".  If NULL, a
#'     random candidate is selected from among the candidates not
#'     initially top-ranked.  All other candidates are ranked #2.  An
#'     integer value for 'favoured' is interpreted as an index into
#'     the candidate names.
#' @param tacticalBallot A ballot paper i.e. a vector of length
#'     ncol(ballots).  If this argument is non-null, it takes
#'     precedence over 'favoured' when the ballot box is being
#'     stuffed.
#' @param alimit Maximum number of plumping ballots to add
#' @param ainc Number of ballots to be added in each step
#' @return Number of plumping ballots required for the tactical ballot
#'     (if any) to affect the ranking, or for the favoured candidate
#'     to move up in the ranking; or zero, if the termination
#'     condition has been reached without the tactical voting (if any)
#'     having affected the ranking or the ranking of the favoured
#'     candidate (if any) having improved.  The results of the final
#'     ballot count are attached as an attribute "finalResults".
#' @examples
#' test.additions(food_election, countMethod="condorcet", alimit=10)
#' test.additions(ims_election, favoured=1, countArgs=c("nseats=10"))
#' test.additions(food_election, tacticalBallot=c(1,2,3,4,5), alimit=5)
test.additions <- function(votes, alimit = NULL, ainc = NULL,
                           favoured = NULL, tacticalBallot = NULL,
                           rankMethod = "SafeRank",
                           countMethod = "stv", countArgs = NULL ) {

    if( is.null(alimit) ) {
        alimit = 1
    }
    if( is.null(ainc) ) {
        ainc <- round( sqrt(alimit) )
    }

    countArgs <- append( countArgs, list(quiet=TRUE) )
    ##TODO: add a 'quiet' arg to test.additions()

    cr <- do.call(countMethod,append(countArgs,list(votes=votes)))
    if(! rankMethod %in% attributes(cr)$names ) {
        stop(paste("countMethod", countMethod, "does not produce a",
                   rankMethod))
    }

    cat("Initial ranking:\n")
    initRank <- cr[[rankMethod]]
    print( initRank )

    votes <- cr$data ## corrected ballots
    nc <- ncol(votes)

    if( is.null(tacticalBallot)) {

        if( ! is.null(favoured) ) {
            fc <- ifelse( is.string(favoured),
                         which( names(initRank) == favoured ),
                         favoured ## favoured is an integer
                         )
            stopifnot( (fc >= 1) || (fc <= nc) )
            favoured <- colnames(cr$data)[fc]
        } else {
            cl <-names(initRank[-1]) ## choose a random non-winner to favour
            favoured <- cl[sample(length(cl), 1)]
            fc <- which( colnames(cr$data) == favoured )
        }
        ## henceforth, favoured is a string and fc is an integer
        cat( "\nAdding up to", alimit, "ballots to favour", favoured, "\n" )
        fb <- rep(2,nc)
        fb[fc] <- 1 # fb is a ballot preferring fc to all other candidates
        if( initRank[fc] == 1 ) {
            warning("\n", favoured, "is already top-ranked.")
            ## possibly a desirable test of the stability of lower rankings
            ## when a top-ranked candidate is plumped
        }
    } else {
        fb <- tacticalBallot
        stopifnot( (length(fb) == nc) )
        cat( "\nAdding up to", alimit, "ballots with preferences", tacticalBallot, "\n" )
    }
    cat("Testing progress: ")

    improvementFound <- FALSE
    nadd <- 0
    while( (!improvementFound) && (nadd <= alimit-ainc) ) {
        nadd <- nadd + ainc
        cat( paste0(" ", nadd, ",") )
        if( ((nadd %/% ainc) %% 20) == 0 ) cat("\n ")
        votes <- rbind(votes,
                       matrix(fb,nrow=ainc,ncol=nc,byrow=TRUE)
                       ) ## stuffing the ballot box!
        newCR <- do.call(countMethod,
                         append(countArgs,list(votes=votes))
                         )
        newRank <- newCR[[rankMethod]]
        if( is.null(tacticalBallot) && (newRank[favoured] < initRank[favoured]) ) {
            improvementFound <- TRUE
            if( ((nadd %/% ainc) %% 20) != 0 ) cat("\n")
            cat( favoured, "was upranked after", nadd,
                "ballots were added.\n" )
            cat("Final ranking:\n")
            print( newRank )
        } else if(! identical(newRank,initRank) ) {
            improvementFound <- TRUE
            if( ((nadd %/% ainc) %% 20) != 0 ) cat("\n")
            cat( "Ranking changed after", nadd, "tactical ballots were stuffed.\n" )
            cat("Final ranking:\n")
            print( newRank )
        }

    }
    if( !improvementFound ) {
        cat( "\nNo improvement in ranking after", nadd,
            "ballots were stuffed.\n")
    }

    result = nadd
    attributes(result) <- list(finalResults=newCR)
    return(invisible(result))
}

#' Test the fraction of ballots required to determine a result.
#' Starting from some number ('astart') of randomly-selected ballots,
#' additional randomly-selected ballots are added until the same
#' result is obtained as when all of the ballots are counted.
#'
#' @param votes A set of ballots
#' @param countMethod The name of a function which will count the
#'     ballots
#' @param countArgs List of args to be passed to countMethod (in
#'     addition to votes)
#' @param rankMethod Name of a ranking attribute in the output of
#'     countMethod
#' @param astart Starting number of ballots
#' @param ainc Number of ballots to be added in each step
#' @param arep Limit on the number of repetitions of the test.
#'     Required to be non-null if ainc==0.
#' @return Number of ballots required to reach the same result as when
#'     all the ballots are counted.  The final ballot count is
#'     attached as an attribute "finalResults".
#' @examples
#' test.fraction(food_election, countMethod="condorcet", astart=3)
#' test.fraction(ims_election, countArgs=c("nseats=10"), ainc=4)
#' test.fraction(dublin_west, ainc=0, arep=1000)
test.fraction <- function(votes, astart = NULL, ainc = NULL,
                          arep = NULL, rankMethod = "SafeRank",
                          countMethod = "stv", countArgs = NULL ) {

    nv <- nrow(votes)
    nc <- ncol(votes)

    if( is.null(astart) ) {
        astart = 1
    }
    if( is.null(ainc) ) {
        ainc <- round( sqrt(nv) )
    } else {
        if( ainc == 0 ) {
            stopifnot( !is.null(arep) )
        }
    }

    countArgs <- append( countArgs, list(quiet=TRUE) )
    ##TODO: add a 'quiet' arg

    cr <- do.call(countMethod,append(countArgs,list(votes=votes)))
    if(! rankMethod %in% attributes(cr)$names ) {
        stop(paste("countMethod", countMethod, "does not produce a",
                   rankMethod))
    }

    cat("Initial ranking (counting all ballots):\n")
    initRank <- cr[[rankMethod]]
    print( initRank )

    selectedVotes <- sample(nv, astart)

    cat( "\nSelecting an increasingly-large fraction of", nv,
        "ballots until the initial ranking is found.\n" )
    cat("Progress: ")

    rankingFound <- FALSE
    nadd <- astart
    nreps <- 1

    while( !rankingFound ) {

        cat( paste0(" ", format(round(nadd/nv*100,1), digits=4), "%,") )
        if( ((nreps %% 20) == 0 ) cat("\n ")

        selVotes <- sample(nv, nadd)
        newCR <- do.call(countMethod,
                         append(countArgs,list(votes=votes[selVotes])))
        newRank <- newCR[[rankMethod]]

        if( identical(newRank,initRank) ) {
            rankingFound <- TRUE
            cat("\nRanking found after", nadd,
                "randomly-selected ballots were counted." )
            cat("\nFinal ranking:\n")
            print( newRank )
        }

        nadd <- min( nadd + ainc, nv )
        nreps <- nreps + 1
        if( (nadd > nv) || (!is.null(arep) && nreps >= arep ) ) {
            break
        }

    }

    if( !rankingFound ) {
        cat( "\nAll ballots must be counted to reach this result\n" )
    }

    result = nadd
    attributes(result) <- list(finalResults=newCR)
    return(invisible(result))
}

