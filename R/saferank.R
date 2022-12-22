#' Assess the safety of a preliminary result for an election
#'
#' Ballots are deleted at random from the ballot-box, with election results
#' computed once per `dinc` ballot-deletions.  The experiment terminates after a
#' specified number of ballots have been deleted, or a specified number of
#' ballot-counts have occurred.  Note: these ballot-counts are correlated.  Use
#' [testFraction()] to experiment with independently-drawn samples from the
#' ballot-box.
#'
#' @param votes A set of ballots, as in 
#'   [vote_2.3.2](https://cran.r-project.org/web/packages/vote/index.html)
#' @param countMethod The name of a function which will count the ballots
#' @param countArgs List of args to be passed to `countMethod` (in addition to
#'   `votes`)
#' @param rankMethod Name of a ranking attribute in the output of countMethod
#' @param dstart Number of ballots in the first ballot-count (selected at random
#'   from `votes`, without replacement)
#' @param dinc Number of ballots to be deleted in subsequent steps
#' @param dlimit Maximum number of ballots to delete (in addition to `dstart`)
#' @param drep Maximum number of elections (required if `dinc=0`)
#' @param exptName stem-name of experimental units *e.g.* "E".  If `NULL`, then
#'   a 3-character string of capital letters is chosen at random.
#' @param quiet TRUE to suppress all output
#' @param verbose TRUE to produce diagnostic output
#' @return [SafeRankExpt](new_SafeRankExpt.html) object, describing this experiment
#'   and its results
#' @export
#' @import data.table
#' @examples
#' data(food_election)
#' testDeletions(food_election)
#' testDeletions(food_election, countMethod="stv",
#'   countArgs=list(complete.ranking=TRUE))
#' 

testDeletions <- function(votes,
                          countMethod = "stv",
                          countArgs = list(),
                          dstart = NULL,
                          dinc = NULL,
                          dlimit = NULL,
                          drep = NULL,
                          rankMethod = "safeRank",
                          exptName = NULL,
                          quiet = FALSE,
                          verbose = FALSE) {
  ## stv() throws an error if there are fewer than two ballots
  stopifnot(nrow(votes) > 1)
  
  marginNames <-
    sapply(colnames(votes), function(x)
      paste0("m.", x))
  
  if (is.null(dstart)) {
    dstart <- nrow(votes)
  }
  stopifnot((dstart <= nrow(votes)) && (dstart > 1)) 

  stopifnot(is.list(countArgs))
  ## suppress all output from counting unless verbose=TRUE
  cArgs <-
    append(countArgs, list(quiet = !verbose, verbose = verbose))
  
  ## construct an initial ballot box by sampling from input 'votes'
  sv <- sample(nrow(votes), dstart)
  ballots <- votes[sv,]
  
  cr <- do.call(countMethod, append(cArgs, list(votes = ballots)))
  if (!rankMethod %in% attributes(cr)$names) {
    stop(paste("countMethod", countMethod, "does not produce a",
               rankMethod))
  }
  
  nseats <- length(cr$elected)
  
  nib <- nrow(cr$data) - dstart
  if (nib > 0) {
    warning(paste(nib, "informal ballots were deleted."))
    if (nib > dstart) {
      dstart <- nib
    }
    if (nrow(cr$data) < 2) {
      stop("Insufficient ballots to run the test.")
    }
  }
  
  ballots <- cr$data
  nb <-
    nrow(ballots) ## nb < dstart, if there were any invalid ballots
  dstart <- nb ## our (possibly corrected) starting-point
  
  dlimit = min(dlimit, 2)
  
  if (is.null(dinc)) {
    dinc <- (dstart - dlimit + 4) %/% 10  ## deciles (roughly)
    if (dinc == 0) {
      dinc = 1
    }
  }
  stopifnot(dinc >= 0)
  
  if (is.null(drep)) {
    stopifnot(dinc != 0)
    drep <- trunc((dstart - dlimit) / dinc) + 1
  }
  
  result <- new_SafeRankExpt(
    rankNames = colnames(votes),
    marginNames = marginNames,
    countMethod = countMethod,
    rankMethod = rankMethod,
    datasetName = deparse1(substitute(votes)),
    ## filename or varname
    experimentalMethod = "testDeletions",
    nseats = nseats,
    countArgs = countArgs,
    otherFactors = list(
      dstart = dstart,
      dinc = dinc,
      dlimit = dlimit,
      drep = drep
    ),
    unitFactors = list(initSample = sv,
                       removedBallots = list())
  )
  
  if (is.null(exptName)) {
    exptName <-
      paste(LETTERS[sample(length(LETTERS), 3, replace = TRUE)],
            collapse = "")
  }
  exptID = paste0(exptName, 0)
  
  crRank <- extractRank(rankMethod, countMethod, cr)
  crMargins <- extractMargins(marginNames, rankMethod, cr)
  newResult <- append(list(exptID = exptID,
                           nBallots = nb),
                      append(crRank,
                             crMargins))
  result <- rbind.SafeRankExpt(result, newResult)
  
  if (!quiet) {
    cat(paste0("Number of ballots counted by ",
               countMethod, ":\n  ", nb))
  }
  
  if (drep > 1) {
    nbv <- dstart - dinc * (1:(drep - 1))
  } else {
    nbv <- NULL
  }
  nbv <- nbv[nbv > 1]
  
  nrep <- 0
  rBallots <- list()
  for (nb in nbv) {
    nrep <- nrep + 1
    exptID <- paste0(exptName, nrep)
    if (!quiet) {
      cat(ifelse((nrep %% 10) == 0, ",\n", ","), nb)
    }
    
    rbn <- sample(nrow(ballots), dinc)
    rBallots <- append(rBallots, rownames(ballots)[rbn])
    ballots <- ballots[-rbn,]
    cr <-
      do.call(countMethod, append(cArgs, list(votes = ballots)))
    
    ##TODO: consider adding cr$elected to the experimental record, to allow
    ##warning-free testing of elections which do not fill all available seats
    if (length(cr$elected) != nseats) {
      warning(
        cat(
          exptID,
          "elected",
          length(cr$elected),
          "candidates, but",
          paste0(exptName, 1),
          "elected",
          nseats,
          "candidates"
        )
      )
    }
    crRank <- extractRank(rankMethod, countMethod, cr)
    crMargins <- extractMargins(marginNames, rankMethod, cr)
    newResult <- append(list(exptID = exptID,
                             nBallots = nb),
                        append(crRank,
                               crMargins))
    result <- rbind.SafeRankExpt(result, newResult)
    stopifnot(is.SafeRankExpt(result))
  }
  
  uF <- attr(result, "unitFactors")
  uF$removedBallots <- rBallots
  attr(result, "unitFactors") <- uF

  if (!quiet) {
    cat("\n")
    print(summary(result))
  }
  stopifnot(is.SafeRankExpt(result))
  return(invisible(result))
}


#' Test the sensitivity of a result to tactical voting.
#'
#' Ballots are added until a specified number of simulated elections (`arep`)
#' have been held.   If a `favoured` candidate is specified, then the ballot-box
#' is stuffed with ballots awarding first-preference to this candidate.
#' Alternatively, a `tacticalBallot` may be specified.  If both `favoured` and
#' `tacticalBallot` are `NULL`, then a random candidate is selected as the
#' favoured one.
#'
#' @param votes A set of ballots, as in
#'   [vote_2.3.2](https://cran.r-project.org/web/packages/vote/index.html)
#' @param countMethod The name of a function which will count the ballots
#' @param countArgs List of args to be passed to countMethod (in addition to
#'   votes)
#' @param rankMethod Name of a ranking attribute in the output of countMethod
#' @param favoured Name of the candidate being "plumped".  If `NULL`, a random
#'   candidate is selected from among the candidates not initially top-ranked.
#'   All other candidates are fully-ranked at random, with an identical ballot
#'   paper being stuffed multiple times.  An integer value for `favoured` is
#'   interpreted as an index into the candidate names.
#' @param tacticalBallot A ballot paper i.e. a vector of length `ncol(ballots)`.
#'   If this argument is non-`NULL`, it takes precedence over `favoured` when the
#'   ballot box is being stuffed.
#' @param ainc Number of ballots to be added in each step
#' @param arep Maximum number of ballot-stuffed elections to run
#' @param exptName stem-name of experimental units *e.g.* "E".  If `NULL`, then
#'   a 3-character string of capital letters is chosen at random.
#' @param quiet `TRUE` to suppress all output
#' @param verbose `TRUE` to produce diagnostic output
#' @return A matrix of experimental results, of dimension \eqn{n} by \eqn{2m+1},
#'   where \eqn{n} is the number of elections and \eqn{m} is the number of
#'   candidates.  The first column is named "nBallots".  Other columns indicate
#'   the ranking of the eponymous candidate, and their margin over the
#'   next-lower-ranked candidate.
#' @export
#' @examples
#' data(food_election)
#' testAdditions(food_election, countArgs=list(complete.ranking=TRUE))
#' testAdditions(food_election, tacticalBallot=c(1,2,3,4,5), arep=2)
#'
testAdditions <- function(votes,
                          ainc = 1,
                          arep = NULL,
                          favoured = NULL,
                          tacticalBallot = NULL,
                          rankMethod = "safeRank",
                          countMethod = "stv",
                          countArgs = list(),
                          exptName = NULL,
                          quiet = FALSE,
                          verbose = FALSE) {
  if (is.null(arep)) {
    arep <- 1
  }
  stopifnot(arep > 0)
  if (is.null(ainc)) {
    ainc <- round(sqrt(nrow(votes)))
  }
  stopifnot(ainc >= 0)
  
  nc <- ncol(votes)
  nv <- nrow(votes)
  
  if (is.null(tacticalBallot)) {
    if (!is.null(favoured)) {
      fc <- ifelse(is.character(favoured),
                   which(names(crRank) == favoured),
                   favoured)
      stopifnot((fc >= 1) || (fc <= nc))
      favoured <- colnames(cr$data)[fc]
    } else {
      cl <- colnames(votes) ## choose a random candidate to favour
      favoured <- cl[sample(length(cl), size = 1)]
      fc <- which(colnames(votes) == favoured)
    }
    ## henceforth, favoured is a string and fc is an integer
    tacticalBallot <- sample(1:nc, nc) # random ballot
    tacticalBallot[fc] <- 0
    tacticalBallot <-
      rank(tacticalBallot) # favoured is most-preferred, other prefs are random
  } else {
    stopifnot((length(tacticalBallot) == nc))
  }
  names(tacticalBallot) <- colnames(votes)
  
  marginNames <- sapply(colnames(votes), function(x)
    paste0("m.", x))
  
  result <- new_SafeRankExpt(
    rankNames = colnames(votes),
    marginNames = marginNames,
    countMethod = countMethod,
    rankMethod = rankMethod,
    datasetName = deparse1(substitute(votes)),
    experimentalMethod = "testAdditions",
    countArgs = countArgs,
    nseats = integer(0),  # not yet known. NULL is hazardous in a DF or DT
    otherFactors = list(
      ainc = ainc,
      arep = arep,
      tacticalBallot = tacticalBallot
    ),
    unitFactors = list()
  )
  
  if (is.null(exptName)) {
    exptName <-
      paste(LETTERS[sample(length(LETTERS), 3, replace = TRUE)],
            collapse = "")
  }
  
  ## Suppress all output from counting unless verbose=TRUE
  cArgs <-
    append(countArgs, list(quiet = !verbose, verbose = verbose))
  
  cr <- do.call(countMethod, append(cArgs, list(votes = votes)))
  if (!rankMethod %in% attributes(cr)$names) {
    stop(paste("countMethod", countMethod, "does not produce a",
               rankMethod))
  }
  
  ## include the initial ballot count in the experimental record
  exptID = paste0(exptName, 0)
  crRank <- extractRank(rankMethod, countMethod, cr)
  crMargins <- extractMargins(marginNames, rankMethod, cr)
  newResult <- append(list(exptID = exptID,
                           nBallots = nrow(votes)),
                      append(crRank,
                             crMargins))
  result <- rbind.SafeRankExpt(result, newResult)
  nseats <- length(cr$elected) ## nseats value is inferred from election results
  attr(result, "nseats") <- nseats

  if (!quiet && verbose) {
    cat("Initial ranking by", countMethod, ":\n")
    print(crRank)
  }
  
  svotes <- votes ## simulated ballot box
  
  if (!quiet) {
    cat("\nAdding up to",
        arep * ainc,
        countMethod,
        "ballots = (",
        tacticalBallot,
        ")\n")
  }
  
  if (!quiet) {
    cat("Testing progress: ")
  }
  
  nadd <- 0
  for (repct in 1:arep) {
    dnames <- list(c((nv + nadd + 1):(nv + nadd + ainc)), names(tacticalBallot))
    svotes <- rbind(svotes,
                    matrix(
                      tacticalBallot,
                      nrow = ainc,
                      ncol = nc,
                      byrow = TRUE,
                      dimnames = dnames
                    )) ## stuffing the ballot box!
    nadd <- nadd + ainc
    if (!quiet) {
      cat(paste0(" ", repct))
      cat(ifelse(repct < arep, ifelse((repct %% 10) == 0, ",\n", ","), ""))
    }
    cr <- do.call(countMethod,
                  append(cArgs, list(votes = svotes)))
    
    exptID = paste0(exptName, repct)
    ##TODO: consider adding cr$elected to the experimental record, to allow
    ##warning-free testing of elections which do not fill all available seats
    if (length(cr$elected) != nseats) {
      warning(
        cat(
          exptID,
          "elected",
          length(cr$elected),
          "candidates, but",
          paste0(exptName, 0),
          "elected",
          nseats,
          "candidates"
        )
      )
    }
    crRank <- extractRank(rankMethod, countMethod, cr)
    crMargins <- extractMargins(marginNames, rankMethod, cr)
    newResult <- append(list(exptID = exptID,
                             nBallots = nrow(svotes)),
                        append(crRank,
                               crMargins))
    result <- rbind.SafeRankExpt(result, newResult)
    
  }
  
  if (!quiet) {
    cat("\n")
    print(summary(result))
  }
  return(invisible(result))
}


#' Bootstrapping experiment, with fractional counts of a ballot box.
#'
#' Starting from some number (`astart`) of randomly-selected ballots, an
#' increasingly-large collection of randomly-selected ballots are counted. The
#' ballots are chosen independently without replacement for each experimental
#' unit; if you want to count decreasingly-sized portions of a single sample of
#' ballots, use [testDeletions()].
#'
#' @param votes A numeric matrix: one row per ballot, one column per candidate
#' @param countMethod The name of a function which will count the ballots,
#'   *e.g.* "stv", "condorcet"
#' @param countArgs List of args to be passed to `countMethod` (in addition to
#'   `votes`)
#' @param rankMethod Name of a ranking attribute in the output of countMethod,
#'   e.g. "elected", "ranking", "safeRank".
#' @param astart Starting number of ballots (min 2)
#' @param ainc Number of ballots to be added in each step. Must be non-negative.
#' @param arep Number of repetitions of the test on each step. Required to be
#'   non-`NULL` if `ainc=0` && is.null(trep)`.
#' @param trep Limit on the total number of simulated elections. Required to be
#'   non-`NULL` if `ainc=0 && is.null(arep)`.
#' @param exptName stem-name of experimental units *e.g.* "E".  If `NULL`, then
#'   a 3-character string of capital letters is chosen at random.
#' @param quiet `TRUE` to suppress all output
#' @param verbose `TRUE` to produce diagnostic output
#' @return a [SafeRankExpt](new_SafeRankExpt.html) object of experimental
#'   results.
#' @export
#' @examples
#' data(food_election)
#' testFraction(food_election, countMethod="condorcet",
#'              countArgs=list(safety=0.5,complete.ranking=TRUE))
#' testFraction(dublin_west, astart=20, ainc=10, arep=2, trep=3, 
#'              countMethod="stv", rankMethod="elected", quiet=FALSE)
testFraction <- function(votes = NULL,
                         astart = NULL,
                         ainc = NULL,
                         arep = NULL,
                         trep = NULL,
                         rankMethod = "safeRank",
                         countMethod = "stv",
                         countArgs = list(),
                         exptName = NULL,
                         quiet = FALSE,
                         verbose = FALSE)
{
  stopifnot(!is.null(votes))
  nv <- nrow(votes)
  nc <- ncol(votes)
  
  if (is.null(astart) || (astart < 2)) {
    astart = 2
  }
  if (is.null(ainc)) {
    ainc <- nv %/% 10  ## deciles (roughly)
  }
  stopifnot(ainc >= 0)
  if (ainc == 0) {
    stopifnot(!is.null(arep) && !is.null(trep))
    if (is.null(arep)) {
      arep = trep
    } else if (is.null(trep)) {
      trep = arep
    }
  } else {
    if (!is.null(trep) && is.null(arep)) {
      arep = floor(trep / (floor((nv - astart) / ainc) + 1))
    } else if (is.null(arep)) {
      arep = 1
    }
    if (is.null(trep)) {
      trep = arep * (floor((nv - astart) / ainc) + 1)
    }
  }
  
  ## abort excessively-large experiments
  stopifnot(trep <= 1e6)
  
  if (ainc != 0) {
    nsteps <- min(floor(trep / arep), floor((nv - astart) / ainc))
  } else {
    nsteps <- 1
  }
  
  ## `nb` is a vector of distinct `nBallot` values for our experiment
  nb <- astart + ainc * (0:nsteps)
  stopifnot(length(nb) > 0 && !any(nb > nv) && !any(nb < 2))
  
  ## `nbb` is a vector of all `nBallot` values for our experiment
  nbb <- numeric(trep) ## preallocate, to avoid memory-thrashing
  i <- 0
  for (j in 1:(arep + 1)) {
    numToAdd <- min(trep - i, length(nb))
    if (numToAdd > 0) {
      nbb[(i + 1):(i + numToAdd)] <- nb[1:numToAdd]
      i <- i + numToAdd
    }
  }
  if (trep > i) {
    ## this case may arise when ainc, arep, and trep are all specified,
    ## because we do at most arep+1 repetitions at each step
    trep <- i  ## the total number of experiments we'll run
  }
  
  ## Suppress all output from counting unless verbose=TRUE
  cArgs <-
    append(countArgs, list(quiet = !verbose, verbose = verbose))
  
  if (!quiet) {
    cat("Progress in counting", countMethod, "ballots:\n")
  }
  
  if (is.null(exptName)) {
    exptName <-
      paste(LETTERS[sample(length(LETTERS), 3, replace = TRUE)],
            collapse = "")
  }
  
  marginNames <- sapply(colnames(votes), function(x)
    paste0("m.", x))
  
  result <- new_SafeRankExpt(
    rankNames = colnames(votes),
    marginNames = marginNames,
    countMethod = countMethod,
    rankMethod = rankMethod,
    datasetName = deparse1(substitute(votes)),
    experimentalMethod = "testFraction",
    countArgs = countArgs,
    nseats = integer(0),  # not yet known.  NULL is hazardous in a DF
    otherFactors = list(
      astart = astart,
      ainc = ainc,
      arep = arep
    ),
    unitFactors = list()
  )
  
  for (i in (1:trep)) {
    nb <- nbb[i]
    if (!quiet) {
      cat(paste0(" ", format(round(i / trep * 100, 1), digits = 4), "%"))
      cat(ifelse(i < trep,
                 ifelse((i %% 10) == 0, ",\n", ","),
                 ""))
    }
    
    selBallots <- sample(nv, nb)
    cr <- do.call(countMethod,
                  append(cArgs, list(votes = votes[selBallots,])))
    
    exptID = paste0(exptName, i)
    
    ##TODO: consider adding `cr$elected` to the experimental record, to allow
    ##warning-free testing of elections which are counted by methods which do
    ##not always fill all available seats.  Alternatively, revise
    ##`check.nseats()` so that its dependence on the counting method is
    ##explicit, rather than implicit in its parameter values (which introduces a
    ##hazard in the stochastic-experimentation harness, whenever `nseats` is not
    ##explicitly specified in `countArgs`).
    if (i == 1) {
      nseats <- length(cr$elected)
      attr(result, "nseats") <- nseats
    } else {
      if (length(cr$elected) != nseats) {
        warning(
          cat(
            exptID,
            "elected",
            length(cr$elected),
            "candidates, but",
            paste0(exptName, 0),
            "elected",
            nseats,
            "candidates"
          )
        )
      }
    }
    
    crRank <- extractRank(rankMethod, countMethod, cr)
    crMargins <- extractMargins(marginNames, rankMethod, cr)
    newResult <- append(list(exptID = exptID,
                             nBallots = nb),
                        append(crRank,
                               crMargins))
    result <- rbind.SafeRankExpt(result, newResult)
  }
  
  if (!quiet) {
    cat("\n")
    print(summary(result))
  }
  
  return(invisible(result))
}

#' Extract a ranking vector by name from the results of a ballot count
#'
#' @param rankMethod e.g. "elected"
#' @param countMethod e.g. "stv"
#' @param cr structure returned by a ballot-counting method
#'
#' @return a numeric ranking vector, in order of colnames(cr$data)
extractRank <- function(rankMethod, countMethod, cr) {
  if (!rankMethod %in% attributes(cr)$names) {
    stop(paste("countMethod",
               countMethod,
               "does not produce a",
               rankMethod))
  }
  if (rankMethod == "elected") {
    ## convert a list of names to a 1-2 ranking vector
    ranks <- c(ifelse(colnames(cr$data) %in% cr$elected, 2, 1))
  } else {
    ranks <- cr[[rankMethod]]
  }
  names(ranks) <- colnames(cr$data)
  return(ranks)
}

#' extract margins from the results of a ballot count
#'
#' @param marginNames list of colnames of the margins in our SafeRank result
#' @param rankMethod if "safeRank", margins are adjusted appropriately
#' @param cr structure returned by a ballot-counting method
#'
#' @return named list of margins
extractMargins <- function(marginNames, rankMethod, cr) {
  crMargins <- cr$margins
  if (rankMethod == "safeRank") {
    sRank <- cr$safeRank
    ## reverse the order of margins for tied candidates, so that candidates
    ## within a safeRank tie group have margins indicative of their relative
    ## strengths.  A margin of 0 is possible, and reveals a tied vote-count.
    ## Extremely small margins are indicative of floating-point roundoff errors.
    for (i in 1:length(sRank)) {
      tieMask <- sRank == i
      if (sum(tieMask) > 1) {
        crMargins[tieMask] <- rev(crMargins[tieMask])
      }
    }
  }
  names(crMargins) <- marginNames
  return(crMargins)
}

#' Constructor for the results of a SafeRank experiment
#' 
#' @param rankNames colnames for per-candidate ranks
#' @param marginNames colnames for per-candidate margins
#' @param countMethod secondary factor: counting method e.g. "stv"
#' @param rankMethod secondary factor: ranking method e.g. "elected"
#' @param datasetName secondary factor: name of the dataset of ballots
#' @param experimentalMethod secondary factor: name of the method which
#'   simulated these elections e.g. "testFraction"
#' @param nseats secondary factor: number of seats to be filled
#' @param countArgs secondary factor: args passed to countMethod
#' @param otherFactors other secondary factors, e.g. parameters to
#'   experimentalMethod
#' @param unitFactors per-unit factors derived from PRNG of the experimental
#'   harness, e.g describing the ballots randomly deleted during testDeletions 
#' @return object of class SafeRankExpt
#' 
#' @export
new_SafeRankExpt <- function(rankNames =          list(),
                             marginNames =        list(),
                             countMethod =        character(0),
                             rankMethod =         character(0),
                             datasetName =        character(0),
                             experimentalMethod = character(0),
                             countArgs =          list(),
                             nseats =             integer(0),
                             otherFactors =       list(),
                             unitFactors =        list()) {
  df <- data.frame(
    exptID   = matrix(character(0), ncol = 1),
    nBallots = matrix(integer(0),   ncol = 1),
    ranks    = matrix(integer(0),   ncol = length(rankNames)),
    margins  = matrix(double(0),    ncol = length(marginNames))
  )
  colnames(df) <- c("exptID", "nBallots", rankNames, marginNames)
  
  attr(df, "countMethod")        <- countMethod
  attr(df, "rankMethod")         <- rankMethod
  attr(df, "datasetName")        <- datasetName
  attr(df, "experimentalMethod") <- experimentalMethod
  attr(df, "countArgs")          <- countArgs
  attr(df, "startTime")          <- format(as.POSIXlt(Sys.time()))
  attr(df, "nseats")             <- nseats
  attr(df, "otherFactors")       <- otherFactors
  attr(df, "unitFactors")        <- unitFactors
  
  return(as.SafeRankExpt(df))
}

#' as.SafeRankExpt()
#' 
#' @param df data.frame object with all required attributes of a SafeRankExpt
#' @return SafeRankExpt object
as.SafeRankExpt <- function(df) {
  stopifnot(inherits(df, "data.frame"))
  attr(df, "class") <- c("SafeRankExpt", "data.frame")
  stopifnot(is.SafeRankExpt(df))
  return(df)
}

#' is.SafeRankExpt()
#'
#' @param x object of unknown class
#' @return TRUE if x is a SafeRankExpt object
#' @export
is.SafeRankExpt <- function(x) {
  stopifnot(inherits(x, "data.frame"))
  subclassAttrs <- c(
    "countMethod",
    "rankMethod",
    "datasetName",
    "experimentalMethod",
    "countArgs",
    "startTime",
    "nseats",
    "otherFactors",
    "unitFactors"
  )
  expectedAttrs <- append( names(attributes(data.frame(0))), subclassAttrs)
  missingAttrs <- setdiff(expectedAttrs, names(attributes(x)))
  extraAttrs <- setdiff(names(attributes(x)), expectedAttrs)
  if (length(missingAttrs) > 0) {
    warning(cat("\nMissing attributes:", missingAttrs))
  }
  if (length(extraAttrs) > 0) {
    warning(cat("\nAdditional attributes:", extraAttrs))
  }
  return((length(missingAttrs) == 0) &&
           inherits(x, "data.frame") &&
           inherits(x, "SafeRankExpt"))
}

#' add a row to a SafeRankExpt object
#'
#' @param object prior results of experimentation
#' @param row    new observations
#'
#' @return SafeRankExpt object with an additional row
#' 
rbind.SafeRankExpt <- function(object, row) {
  stopifnot(is.SafeRankExpt(object))
  ##TODO: optimise, if level 2 of the R Inferno is ever painfully hot
  result <- dplyr::bind_rows(object, row)
  stopifnot(identical(colnames(object), colnames(result)))
  ## rbind returns a base-class data.frame
  ## attr(result, "class") <- c("SafeRankExpt", "data.frame")
  stopifnot(is.SafeRankExpt(result)) 
  return(result)
}

#' summary method for SafeRankExpt
#'
#' @param object experimental results to be summarised
#' @param ... args for generic summary()
#'
#' @return summary.SafeRankExpt object
#' @export
summary.SafeRankExpt <- function(object, ...) {
  if (inherits(object, "data.table")) {
    x <- as.data.frame(object)
    warning("Since v0.99, SafeRankExpt is a data.frame")
  }
  stopifnot(is.SafeRankExpt(object))
  attr(object, "class") <- c("summary.SafeRankExpt", "data.frame")
  return(object)
}

#' Print method for summary.SafeRankExpt
#'
#' @param x experimental results
#' @param ... args for generic print()
#'
#' @return invisible(x), with side-effects to console
#' @importFrom knitr kable
#' @export
print.summary.SafeRankExpt <- function(x, ...) {
  stopifnot(inherits(x, "data.frame") && inherits(x, "summary.SafeRankExpt"))
  cat(
    paste0(
      "\nResults of ",
      attr(x, "experimentalMethod"),
      " at ",
      attr(x, "startTime"),
      "\n\nDataset = ",
      attr(x, "datasetName"),
      ", countMethod = ",
      attr(x, "countMethod"),
      ", rankMethod = ",
      attr(x, "rankMethod")
    )
  )
  cA <- attr(x, "countArgs")
  if (length(cA) > 0) {
    print(knitr::kable(matrix(
      cA,
      ncol = length(cA),
      byrow = TRUE,
      dimnames = list(c("countArgs"), names(cA))
    ),
    align = "r"))
  }
  oF <- attr(x, "otherFactors")
  if (length(oF) > 0) {
    print(knitr::kable(matrix(
      oF,
      ncol = length(oF),
      byrow = TRUE,
      dimnames = list(c("otherFactors"), names(oF))
    ),
    align = "r"))
  }
  uF <- attr(x, "unitFactors")
  if (length(uF) > 0) {
    cat("\nUnit factors: ")
    cat(names(uF), sep=", ")
    cat("\n")
  }
  cat("\nExperiment ID, number of ballots in simulated election,",
      "ranks, winning margins:")
  options(knitr.kable.NA = '')
  if (nrow(x) > 20) {
    print(knitr::kable(x[1:10,]))
    cat("...\n")
    nr <- nrow(x)
    print(knitr::kable(x[(nrow(x)-9):nrow(x),]))
  } else {
    print(knitr::kable(x))
  }
}

#' plot() method for the result of an experiment with varying numbers of ballots
#'
#' The "adjusted rank" of a candidate is their ranking \eqn{r} plus their
#' scaled "winning margin". The scaled margin is
#' \eqn{e^{-cx/\sqrt{n}}}, where \eqn{x} is the adjusted margin (i.e. the number
#' of votes by which this candidate is ahead of the next-weaker candidate,
#' adjusted for the number of ballots \eqn{n} and the number of seats \eqn{s}),
#' and \eqn{c>0} is the margin-scaling parameter `cMargin`.
#'
#' The default value of `cMargin=1.0` draws visual attention to candidates with
#' a very small winning margin, as their adjusted rank is very near to
#' \eqn{r+1}.  Candidates with anything more than a small winning margin have
#' only a small rank adjustment, due to the exponential scaling.
#' 
#' A scaling linear in \eqn{s/n} is applied to margins when `anBallots>0`.  Such
#' a linear scaling may be a helpful way to visualise the winning margins in STV
#' elections because the margin of victory for an elected candidate is typically
#' not much larger than the quota of \eqn{n/(s+1)} (Droop) or \eqn{n/s} (Hare).
#' The linear scaling factor is \eqn{as/n}, where \eqn{a} is the value of
#' `anBallots`, \eqn{s} is the number of seats, and \eqn{n} is the number of
#' ballots. For plotting on the (inverted) adjusted rank scale, the
#' linearly-scaled margin is added to the candidate's rank.  Note that the
#' linearly-scaled margins are zero when \eqn{a=0}, and thus have no effect on
#' the adjusted rank.  You might want to increase the value of `anBallots`,
#' starting from 1.0, until the winning candidate's adjusted rank is 1.0 when
#' all ballots are counted, then confirm that the adjusted ranks of other
#' candidates are still congruent with their ranking (i.e. that the
#' rank-adjustment is less than 1 in all cases except perhaps on an initial
#' transient with small numbers of ballots).
#' 
#' When both `anBallots` and `cMargins` are non-zero, the ranks are adjusted
#' with both exponentially-scaled margins and linearly-scaled margins. The
#' resulting plot would be difficult to interpret in a valid way.
#' 
#' Todo: Accept a list of SafeVoteExpt objects.
#'
#' Todo: Multiple counts with the same number of ballots could be summarised
#' with a box-and-whisker graphic, rather than a set of jittered points.
#'
#' Todo: Consider developing a linear scaling that is appropriate for
#' plotting stochastic experimental data derived from Condorcet elections.
#'
#' @param x object containing experimental results
#' @param facetWrap TRUE provides per-candidate scatterplots
#' @param anBallots,cMargin parameters in the rank-adjustment formula
#' @param xlab,ylab axis labels
#' @param title overall title for the plot.  Default: NULL
#' @param subtitle subtitle for the plot.  Default: value of nSeats and
#'   any non-zero rank-adjustment parameters
#' @param line TRUE will connect points with lines, and will disable jitter
#' @param pointSize diameter of points
#' @param ... params for generic plot()
#' @return graphics object, with side-effect in RStudio Plots pane
#'
#' @export
#' @importFrom stringr str_detect
#' @importFrom forcats fct_reorder
#' @importFrom dplyr mutate
#' @import ggplot2
#'   
plot.SafeRankExpt <-
  function(x,
           facetWrap = FALSE,
           anBallots = 0.0,
           cMargin = 1.0,
           xlab = "Ballots",
           ylab = "Adjusted Rank",
           title = NULL,
           subtitle = "(default)",
           line = TRUE,
           pointSize = 1,
           ...) {
    stopifnot(requireNamespace("stringr", quietly = TRUE))
    
    if (inherits(x, "data.table")) {
      x <- as.data.frame(x)
      warning("Since v0.99, SafeRankExpt is a data.frame")
    }
    stopifnot(is.SafeRankExpt(x))
    
    ## https://joshuacook.netlify.app/post/integer-values-ggplot-axis/
    integer_breaks <- function(n = 5, ...) {
      fxn <- function(x) {
        ## amended 10 Dec 2022 to use round() rather than floor()
        breaks <- round(pretty(x, n, ...))
        names(breaks) <- attr(breaks, "labels")
        breaks
      }
      return(fxn)
    }
    
    nseats <- attr(x,"nseats")
    stopifnot( !is.null(nseats) && (nseats > 0))
    
    if (subtitle == "(default)") {
      subtitle <- ""
      if (anBallots != 0) {
        subtitle <- paste(subtitle, "anBallots =", anBallots)
      }
      if (cMargin != 0) {
        subtitle <- paste(subtitle, "cMargin =", cMargin)
      }
      subtitle <- paste(subtitle, "nSeats =", nseats)
    }
    
    ## colnames of margins, ranks, and scores
    lmnames <- colnames(x)[stringr::str_detect(colnames(x), "^m.")]
    cnames <- unlist(lapply(lmnames, function(x)
      stringr::str_sub(x, 3, stringr::str_length(x))))
    mnames <- unlist(lmnames)
    snames <- unlist(lapply(cnames, function(x) paste0("s.", x)))
    
    ## scores <- exp(-cMargin * margins / sqrt{nBallots}
    ##
    ## Note that a small winning margin adds almost a full point of rank when
    ## `cMargin>0`, and any margin is a full point of rank when `cMargin==0`.
    scores <- as.data.table(x)
    scores[, (snames) := exp(-cMargin * .SD / sqrt(scores$nBallots)), 
           .SDcols = mnames]
    
    ## transformed scores of NA are interpreted as a rank-adjustment of 0.0
    for (j in snames) {
      data.table::set(scores, which(is.na(scores[[j]])), j, 0.0)
    }

    ## margins of NA are treated as 0.0 for linear scaling
    for (j in mnames) {
      data.table::set(scores, which(is.na(scores[[j]])), j, 0.0)
    }
    scores[, (mnames) := .SD * (anBallots * nseats / scores$nBallots), 
           .SDcols = mnames]
    
    ## subtract linearly-scaled margins from scores, and add ranks
    for (j in (1:length(snames))) {
      scores[, (snames[j]) := .SD - scores[[mnames[j]]] + scores[[cnames[[j]]]],
             .SDcols = snames[j]]
    }

    mscores <- 
      data.table::melt(
        scores,
        id.vars = c("exptID", "nBallots"),
        measure.vars = unlist(snames),
        variable.name = "Candidate",
        value.name = "Score"
      )
    
# https://github.com/STAT545-UBC/Discussion/issues/451#issuecomment-385731301
# https://stackoverflow.com/questions/9439256/
    mscores <- dplyr::mutate(
      mscores,
      Candidate = forcats::fct_reorder2(.data$Candidate,
                                        .data$nBallots,
                                        .data$Score,
                                        .desc = FALSE),
    )
    
    g <-
      ggplot(mscores,
             aes(
               x = .data$nBallots,
               y = .data$Score,
               colour = .data$Candidate
             )) +
      geom_point(position = ifelse(line, "identity", "jitter"),
                 size = pointSize) +
      labs(x = xlab,
           y = ylab,
           colour = "Candidate",
           title = title,
           subtitle = subtitle) +
      scale_y_reverse(breaks = integer_breaks())
    
    if (line) {
      g <- g + geom_line()
    }
    
    if (facetWrap) {
      g <- g + facet_wrap( ~ .data$Candidate)
    }
    
    return(g)
  }
