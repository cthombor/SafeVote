#' Test the sensitivity of a ranking to random deletions
#'
#' A series of simulated elections are held, with ballots deleted at random
#' between each pair of elections.  Elections continue until either a
#' specified number of ballots have been deleted, or a specified
#' number of simulations has occurred.
#'
#' @param votes A set of ballots
#' @param countMethod The name of a function which will count the
#'     ballots
#' @param countArgs List of args to be passed to countMethod (in
#'     addition to votes)
#' @param rankMethod Name of a ranking attribute in the output of
#'     countMethod
#' @param dlimit Maximum number of ballots to delete
#' @param dstart  Number of ballots to be deleted after the initial count
#' @param dinc Number of additional ballots to be deleted in subsequent steps
#' @param drep Maximum number of elections (required if dinc=0)
#' @param quiet TRUE to suppress all output
#' @param verbose TRUE to produce diagnostic output
#' @return A matrix of experimental results, of dimension n by m+1, where
#'     n is the number of elections and m is the number of candidates.  The
#'     first column is named "nBallots".  Other columns indicate the rankings
#'     of the eponymous candidates.
#' @export
#' @examples {
#' data(food_election)
#' testDeletions(food_election, countMethod="condorcet", dlimit=4)
#' testDeletions(food_election, countMethod="stv", countArgs=list(nseats=10))
#' }

testDeletions <-
  function(votes = "food_election",
           countMethod = "condorcet",
           countArgs = NULL,
           dlimit = NULL,
           dstart = NULL,
           dinc = NULL,
           drep = NULL,
           rankMethod = "safeRank",
           quiet = FALSE,
           verbose = FALSE) {
    nv <- nrow(votes)
    
    ## suppress all output from counting unless verbose=TRUE
    cArgs <-
      append(countArgs, list(quiet = !verbose, verbose = verbose))
    
    ## an initial count of all ballots
    cr <- do.call(countMethod, append(cArgs, list(votes = votes)))
    if (!rankMethod %in% attributes(cr)$names) {
      stop(paste("countMethod", countMethod, "does not produce a",
                 rankMethod))
    }
    if (nv != nrow(cr$data)) {
      warning(paste(
        nrow(votes) - nrow(cr$data),
        "informal ballots were deleted."
      ))
    }
    ballots <- cr$data ## ballots are numbered and corrected
    nb <- nrow(ballots)
    
    ## include the initial ballot count in the experimental record
    crRank <- cr[[rankMethod]][colnames(votes)]
    if (rankMethod == "elected") {
      crRank <- electedAsRank(crRank, colnames(votes))
    }
    result <- rbind(append(list(nBallots = nb), crRank))
    
    if (!quiet) {
      cat("Number of ballots counted by", countMethod, ":\n", nb)
    }
    
    if (is.null(dlimit)) {
      dlimit = nb-2
    }
    dlimit = min(dlimit, nb - 2) ## an election must have at least 2 ballots
    
    if (is.null(dstart)) {
      if (is.null(dinc) && is.null(drep)) {
        dstart <- 1
      } else {
        dstart <- 0
      }
    }
    
    if (is.null(dinc)) {
      dinc <- dlimit %/% 10  ## deciles (roughly)
    }
    stopifnot(dinc >= 0)
    
    if (is.null(drep)) {
      stopifnot(dinc != 0)
      drep <- trunc(dlimit / dinc) + 1
    }
    
    if (dinc == 0) {
      nbv <- rep(nb - dstart, drep)
    } else {
      nbv <- nb - dstart - dinc * (1:drep)
      nbv <- nbv[nbv > 1]
    }
    
    nrep <- 0
    for (nBallots in nbv) {

      nrep <- nrep + 1
      if (!quiet) {
        cat(ifelse((nrep %% 10) == 0, ",\n", ","), nBallots)
      }
      
      rvn <- sample(nrow(ballots), nBallots)
      ballots <- ballots[rvn,]
      cr <-
        do.call(countMethod, append(cArgs, list(votes = ballots)))
      crRank <- cr[[rankMethod]][colnames(votes)]
      if (rankMethod == "elected") {
        crRank <- electedAsRank(crRank, colnames(votes))
      }

      result <- rbind(result, append(c(nBallots = nBallots), crRank))

    }
    
    if (!quiet) {
      cat("\nExperimental results:\n")
      print(result)
    }
    return(invisible(result))
  }

#' Test the sensitivity of a result to tactical voting.
#'
#' Ballots are added until a specified number of simulated elections (`arep`)
#' have been held   A tactic of "plumping" is used when stuffing the ballot
#' box, if a `favoured` candidate is specified.  Alternatively, a
#' `tacticalBallot` may be specified.
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
#' @param ainc Number of ballots to be added in each step
#' @param arep Maximum number of ballot-stuffed elections to run
#' @param quiet TRUE to suppress all output
#' @param verbose TRUE to produce diagnostic output
#' @return A matrix of experimental results, of dimension n by m+1, where
#'     n is the number of elections and m is the number of candidates.  The
#'     first column is named "nBallots".  Other columns indicate the rankings
#'     of the eponymous candidates.
#' @export
#' @examples
#' data(food_election)
#' testAdditions(food_election)
#' testAdditions(food_election, tacticalBallot=c(1,2,3,4,5), areps=2)
#' 
testAdditions <- function(votes,
                          ainc = NULL,
                          arep = 2,
                          favoured = NULL,
                          tacticalBallot = NULL,
                          rankMethod = "safeRank",
                          countMethod = "stv",
                          countArgs = NULL,
                          quiet = FALSE,
                          verbose = FALSE) {
  if (is.null(arep)) {
    arep = 1
  }
  stopifnot(arep > 0)
  if (is.null(ainc)) {
    ainc <- round(sqrt(nrow(votes)))
  }
  stopifnot(ainc >= 0)
  
  ## Suppress all output from counting unless verbose=TRUE
  cArgs <-
    append(countArgs, list(quiet = !verbose, verbose = verbose))
  
  cr <- do.call(countMethod, append(cArgs, list(votes = votes)))
  if (!rankMethod %in% attributes(cr)$names) {
    stop(paste("countMethod", countMethod, "does not produce a",
               rankMethod))
  }
  
  initRank <- cr[[rankMethod]][colnames(votes)]
  result <- matrix(append(c(nBallots = nrow(votes)), initRank),
                   ncol = ncol(votes) + 1)
  
  if (!quiet && verbose) {
    cat("Initial ranking by", countMethod, ":\n")
    print(initRank)
  }
  
  nc <- ncol(votes)
  nv <- nrow(votes)
  svotes <- votes ## simulated ballot box
  
  if (is.null(tacticalBallot)) {
    if (!is.null(favoured)) {
      fc <- ifelse(is.character(favoured),
                   which(names(initRank) == favoured),
                   favoured)
      stopifnot((fc >= 1) || (fc <= nc))
      favoured <- colnames(cr$data)[fc]
    } else {
      cl <- colnames(votes) ## choose a random non-winner to favour
      cl <- cl[-which(cl==names(initRank[which(initRank==1)]))]
      favoured <- cl[sample(length(cl), size=1)]
      fc <- which(colnames(votes) == favoured)
    }
    ## henceforth, favoured is a string and fc is an integer
    fb <- sample(1:nc, nc) # random ballot
    fb[fc] <- 0
    fb <- rank(fb) # favoured is most-preferred, other prefs are random
  } else {
    fb <- tacticalBallot
    stopifnot((length(fb) == nc))
  }
  names(fb) <- colnames(votes)
  if (!quiet) {
    cat("\nAdding up to",
        arep * ainc,
        countMethod,
        "ballots = (",
        fb,
        ")\n")
  }
  
  if (!quiet)
    cat("Testing progress: ")
  
  nadd <- 0
  for (repct in 1:arep) {
    svotes <- rbind(svotes,
                   matrix(
                     fb,
                     nrow = ainc,
                     ncol = nc,
                     byrow = TRUE,
                     dimnames = list(c((nv+nadd+1):(nv+nadd+ainc)), names(fb))
                   )) ## stuffing the ballot box!
    nadd <- nadd + ainc
    if (!quiet) {
      cat(paste0(" ", nadd))
      cat(ifelse(repct < arep, ifelse((repct %% 10) == 0, ",\n", ","), ""))
    }
    newCR <- do.call(countMethod,
                     append(cArgs, list(votes = svotes)))
    newRank <- newCR[[rankMethod]][colnames(votes)]
    result <-
      rbind(result, append(c(nBallots = nrow(svotes)), newRank))
  }
  
  if (!quiet) {
    cat("\nExperimental results:\n")
    print(result)
  }
  return(invisible(result))
}


#' Experiment with partial counts of ballots.
#'
#' Starting from some number ('astart') of randomly-selected ballots,
#' additional randomly-selected ballots are added.  The rankings produced
#' by each election are returned as a matrix with one row per election.
#'
#' @param votes A set of ballots
#' @param countMethod The name of a function which will count the
#'     ballots, e.g. "stv", "condorcet".
#' @param countArgs List of args to be passed to countMethod (in
#'     addition to votes)
#' @param rankMethod Name of a ranking attribute in the output of
#'     countMethod, e.g. "elected", "ranking", "safeRank".
#' @param astart Starting number of ballots (min 2)
#' @param ainc Number of ballots to be added in each step
#' @param arep Limit on the number of repetitions of the test.
#'     Required to be non-null if ainc==0.
#' @param quiet TRUE to suppress all output
#' @param verbose TRUE to produce diagnostic output
#' @return a matrix of experimental results, of dimension n by m+1, where n is
#'     the number of elections and m is the number of candidates.  The first
#'     column is named "nBallots".  Other columns indicate the ranking of each
#'     candidate in each election.
#' @export
#' @examples
#' data(food_election)
#' testFraction(food_election, countMethod="condorcet",
#'              countArgs=list(safety=0.5))
#' testFraction(dublin_west, astart=20, ainc=20, arep=19, countMethod="stv",
#'              rankMethod="elected", quiet=FALSE)
testFraction <- function(votes,
                         astart = NULL,
                         ainc = NULL,
                         arep = NULL,
                         rankMethod = "safeRank",
                         countMethod = "stv",
                         countArgs = NULL,
                         quiet = FALSE,
                         verbose = FALSE) {
  nv <- nrow(votes)
  nc <- ncol(votes)
  
  if (is.null(astart) || (astart < 2)) {
    astart = 2
  }
  if (is.null(ainc)) {
    ainc <- nv %/% 10  ## deciles (roughly)
  } else {
    if (ainc == 0) {
      stopifnot(!is.null(arep))
    }
  }
  if (is.null(arep)) {
    arep <- trunc((nv - astart) / ainc)
  }
  
  ## Suppress all output from counting unless verbose=TRUE
  cArgs <-
    append(countArgs, list(quiet = !verbose, verbose = verbose))
  
  nb <- astart + ainc * (1:arep)
  if ((ainc != 0) && (arep > trunc((nv - astart) / ainc))) {
    nb <- nb[-which(nb >= nv)]
    nb <- c(nb, nv) ## use all ballots in the last experimental run
  }
  
  nreps <- 0
  result <- NULL
  
  if (verbose && !quiet) {
    cat(
      "\nSelecting an increasingly-large fraction of",
      nv,
      "ballots until the ultimate ranking is found.\n"
    )
  }
  if (!quiet) {
    cat("Fraction of", countMethod, "ballots:\n")
  }
  
  for (nBallots in nb) {
    nreps <- nreps + 1
    if (!quiet) {
      cat(paste0(" ", format(round(
        nBallots / nv * 100, 1
      ), digits = 4), "%"))
      cat(ifelse(nreps < length(nb), ifelse((nreps %% 10) == 0, ",\n", ","), ""))
    }
    
    selBallots <- sample(nv, nBallots)
    newCR <- do.call(countMethod,
                     append(cArgs, list(votes = votes[selBallots, ])))
    
    if (!rankMethod %in% attributes(newCR)$names) {
      stop(paste(
        "countMethod",
        countMethod,
        "does not produce a",
        rankMethod
      ))
    }
    
    newRank <- newCR[[rankMethod]][colnames(votes)]
    if (rankMethod == "elected") {
      newRank <- electedAsRank(newRank, colnames(votes))
    }
    
    result <-
      rbind(result, append(c(nBallots = nBallots), newRank))
  }
  
  if (!quiet) {
    cat("\nExperimental results:\n")
    print(result)
  }
  
  return(invisible(result))
}

#' convert the names of elected candidates into a 0-1 ranking
#'
#' @param elected the names of the elected candidates
#' @param candidates the names of all candidates
#'
#' @return a named 0-1 vector of length(candidates)
#'
electedAsRank <- function(elected, candidates) {
  rv <- as.integer(candidates %in% elected)
  names(rv) <- candidates
  return(rv)
}
