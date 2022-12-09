#' Assess the safety of a preliminary result for an election
#'
#' Ballots are deleted at random from the ballot-box, with election results
#' computed once per 'dinc' ballot-deletions.  The experiment terminates after a
#' specified number of ballots have been deleted, or a specified number of
#' ballot-counts have occurred.  Note: these ballot-counts are correlated.  Use
#' testFraction() to experiment with independently-drawn samples from the
#' ballot-box.
#'
#' @param votes A set of ballots
#' @param countMethod The name of a function which will count the ballots
#' @param countArgs List of args to be passed to countMethod (in addition to
#'   votes)
#' @param rankMethod Name of a ranking attribute in the output of countMethod
#' @param dstart Number of ballots in the first ballot-count (selected at random
#'   from votes, without replacement)
#' @param dinc Number of ballots to be deleted in subsequent steps
#' @param dlimit Maximum number of ballots to delete (in addition to dstart)
#' @param drep Maximum number of elections (required if dinc=0)
#' @param exptName stem-name of experimental units e.g. "E"
#' @param quiet TRUE to suppress all output
#' @param verbose TRUE to produce diagnostic output
#' @return SafeRankExpt object, describing this experiment and its results
#' @export
#' @import data.table
#' @examples
#' data(food_election)
#' testDeletions(food_election)
#' testDeletions(food_election, countMethod="stv",
#'   countArgs=list(complete.ranking=TRUE))
#' 

testDeletions <-
  function(votes = "food_election",
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

    marginNames <- sapply(colnames(votes), function(x) paste0("m.",x))
    
    if (is.null(dstart)) {
      dstart <- nrow(votes)
    }
    if (dstart < 2) {
      dstart <- 2
    }
    
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
    nb <- nrow(ballots) ## nb < dstart, if there were any invalid ballots
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
      experimentalMethod = "testDeletions",
      countArgs = countArgs,
      otherFactors = list(
        dstart = dstart,
        dinc = dinc,
        dlimit = dlimit,
        drep = drep),
      unitFactors = list(
        initSample = sv,
        removedBallots = list())
    )
  
    if (is.null(exptName)) {
      exptName <-
        paste(LETTERS[sample(length(LETTERS), 3, replace = TRUE)],
              collapse = "")
    }
    exptID = paste0(exptName,0)
    
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
    for (nBallots in nbv) {

      nrep <- nrep + 1
      exptID <- paste0(exptName,nrep)
      if (!quiet) {
        cat(ifelse((nrep %% 10) == 0, ",\n", ","), nBallots)
      }
      
      rbn <- sample(nrow(ballots), dinc)
      rBallots <- append(rBallots, rownames(ballots)[rbn])
      ballots <- ballots[-rbn,]
      cr <-
        do.call(countMethod, append(cArgs, list(votes = ballots)))
      
      crRank <- extractRank(rankMethod, countMethod, cr)
      crMargins <- extractMargins(marginNames, rankMethod, cr)
      newResult <- append(list(exptID = exptID, 
                               nBallots = nBallots), 
                          append(crRank, 
                                 crMargins))
      result <- rbind.SafeRankExpt(result, newResult)
      
    }
    
    uF <- attr(result, "unitFactors")
    uF$removedBallots <- rBallots
    attr(result, "unitFactors") <- uF
    ## copying result seems to repair a corruption in its attributes
    cResult <- copy(result)
    
    ## TODO: use data.frame rather than data.table internally, converting
    ## to a data.table only after all experimental data has been collected
    
    if (!quiet) {
      cat("\n")
      print(summary(cResult))
    }
    return(invisible(cResult))
  }

#' Test the sensitivity of a result to tactical voting.
#'
#' Ballots are added until a specified number of simulated elections (`arep`)
#' have been held   A tactic of "plumping" is used when stuffing the ballot
#' box, if a `favoured` candidate is specified.  Alternatively, a
#' `tacticalBallot` may be specified.
#'
#' @param votes A set of ballots
#' @param countMethod The name of a function which will count the ballots
#' @param countArgs List of args to be passed to countMethod (in addition to
#'   votes)
#' @param rankMethod Name of a ranking attribute in the output of countMethod
#' @param favoured Name of the candidate being "plumped".  If NULL, a random
#'   candidate is selected from among the candidates not initially top-ranked.
#'   All other candidates are fully-ranked at random, with an identical ballot
#'   paper being stuffed multiple times.  An integer value for 'favoured' is
#'   interpreted as an index into the candidate names.
#' @param tacticalBallot A ballot paper i.e. a vector of length ncol(ballots).
#'   If this argument is non-null, it takes precedence over 'favoured' when the
#'   ballot box is being stuffed.
#' @param ainc Number of ballots to be added in each step
#' @param arep Maximum number of ballot-stuffed elections to run
#' @param exptName stem-name of experimental units e.g. "E"
#' @param quiet TRUE to suppress all output
#' @param verbose TRUE to produce diagnostic output
#' @return A matrix of experimental results, of dimension n by 2m+1, where n is
#'   the number of elections and m is the number of candidates.  The first
#'   column is named "nBallots".  Other columns indicate the ranking of the
#'   eponymous candidate, and their margin over the next-lower-ranked candidate.
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
  
  marginNames <- sapply(colnames(votes), function(x) paste0("m.",x))
  
  result <- new_SafeRankExpt(
    rankNames = colnames(votes),
    marginNames = marginNames,
    countMethod = countMethod,
    rankMethod = rankMethod,
    datasetName = deparse1(substitute(votes)),
    experimentalMethod = "testAdditions",
    countArgs = countArgs,
    otherFactors = list(
      ainc = ainc,
      arep = arep,
      tacticalBallot = NULL
    ),
    unitFactors = NULL
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
  exptID = paste0(exptName,0)
  crRank <- extractRank(rankMethod, countMethod, cr)
  crMargins <- extractMargins(marginNames, rankMethod, cr)
  newResult <- append(list(exptID = exptID, 
                           nBallots = nrow(votes)), 
                      append(crRank, 
                             crMargins))
  result <- rbind.SafeRankExpt(result, newResult)
  
  if (!quiet && verbose) {
    cat("Initial ranking by", countMethod, ":\n")
    print(crRank)
  }
  
  nc <- ncol(votes)
  nv <- nrow(votes)
  svotes <- votes ## simulated ballot box
  
  if (is.null(tacticalBallot)) {
    if (!is.null(favoured)) {
      fc <- ifelse(is.character(favoured),
                   which(names(crRank) == favoured),
                   favoured)
      stopifnot((fc >= 1) || (fc <= nc))
      favoured <- colnames(cr$data)[fc]
    } else {
      cl <- colnames(votes) ## choose a random non-winner to favour
      cl <- cl[-which(cl==names(crRank[which(crRank==1)]))]
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
  
  oF <- attr(result, "otherFactors")
  oF$tacticalBallot <- fb
  attr(result, "otherFactors") <- oF

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
      cat(paste0(" ", repct))
      cat(ifelse(repct < arep, ifelse((repct %% 10) == 0, ",\n", ","), ""))
    }
    cr <- do.call(countMethod,
                     append(cArgs, list(votes = svotes)))

    exptID = paste0(exptName, repct)
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
#' Starting from some number ('astart') of randomly-selected ballots, an
#' increasingly-large collection of randomly-selected ballots are counted. The
#' ballots are chosen independently without replacement for each experimental
#' unit; if you want to count decreasingly-sized portions of a single sample of
#' ballots, use testDeletions().  The rankings and margins produced by each
#' simulated election are returned as a matrix with one row per election.
#'
#' @param votes A numeric matrix: one row per ballot, one column per candidate 
#' @param countMethod The name of a function which will count the ballots, e.g.
#'   "stv", "condorcet"
#' @param countArgs List of args to be passed to countMethod (in addition to
#'   votes)
#' @param rankMethod Name of a ranking attribute in the output of countMethod,
#'   e.g. "elected", "ranking", "safeRank".
#' @param astart Starting number of ballots (min 2)
#' @param ainc Number of ballots to be added in each step
#' @param arep Limit on the number of repetitions of the test. Required to be
#'   non-null if ainc==0.
#' @param exptName stem-name of experimental units e.g. "E"
#' @param quiet TRUE to suppress all output
#' @param verbose TRUE to produce diagnostic output
#' @return a matrix of experimental results, of dimension n by 2m+1, where n is
#'   the number of elections and m is the number of candidates.  The first
#'   column is named "nBallots".  Other columns indicate the ranking and margin
#'   of each candidate in each election.
#' @export
#' @examples
#' data(food_election)
#' testFraction(food_election, countMethod="condorcet",
#'              countArgs=list(safety=0.5,complete.ranking=TRUE))
#' testFraction(dublin_west, astart=20, ainc=20, arep=2, countMethod="stv",
#'              rankMethod="elected", quiet=FALSE)
testFraction <- function(votes,
                         astart = NULL,
                         ainc = NULL,
                         arep = NULL,
                         rankMethod = "safeRank",
                         countMethod = "stv",
                         countArgs = list(),
                         exptName = NULL,
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
  
  
  nb <- astart + ainc * (0:(arep - 1))
  if ((ainc != 0) && (arep > trunc((nv - astart) / ainc))) {
    nb <- nb[-which(nb >= nv)]
    nb <- c(nb, nv) ## use all ballots in the last experimental run
  }
  
  if (verbose && !quiet) {
    cat("\nSelecting an increasingly-large fraction of",
      nv,
      "ballots until the ultimate ranking is found.\n"
    )
  }
  if (!quiet) {
    cat("Fraction of", countMethod, "ballots:\n")
  }
  
  if (is.null(exptName)) {
    exptName <-
      paste(LETTERS[sample(length(LETTERS), 3, replace = TRUE)],
            collapse = "")
  }
  
  marginNames <- sapply(colnames(votes), function(x) paste0("m.",x))
  
  result <- new_SafeRankExpt(
    rankNames = colnames(votes),
    marginNames = marginNames,
    countMethod = countMethod,
    rankMethod = rankMethod,
    datasetName = deparse1(substitute(votes)),
    experimentalMethod = "testFraction",
    countArgs = countArgs,
    otherFactors = list(
      astart = astart,
      ainc = ainc,
      arep = arep
    ),
    unitFactors = NULL
  )
  
  nreps <- 0
  for (nBallots in nb) {
    nreps <- nreps + 1
    if (!quiet) {
      cat(paste0(" ", format(round(
        nBallots / nv * 100, 1
      ), digits = 4), "%"))
      cat(ifelse(nreps < length(nb),
                 ifelse((nreps %% 10) == 0, ",\n", ","), 
                 ""))
    }
    
    selBallots <- sample(nv, nBallots)
    cr <- do.call(countMethod,
                     append(cArgs, list(votes = votes[selBallots,])))
    
    exptID = paste0(exptName, nreps)
    crRank <- extractRank(rankMethod, countMethod, cr)
    crMargins <- extractMargins(marginNames, rankMethod, cr)
    newResult <- append(list(exptID = exptID, 
                             nBallots = nBallots), 
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
    ranks <- c(ifelse(colnames(cr$data) %in% cr$elected,2,1))
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
#' @param countArgs secondary factor: args passed to countMethod
#' @param otherFactors other secondary factors, e.g. parameters to
#'   experimentalMethod
#' @param unitFactors per-unit factors derived from PRNG of the experimental
#'   harness, e.g describing the ballots randomly deleted during testDeletions 
#' @return object of class SafeRankExpt
new_SafeRankExpt <-
  function(rankNames = NULL,
           marginNames = NULL,
           countMethod = NULL,
           rankMethod = NULL,
           datasetName = NULL,
           experimentalMethod = NULL,
           countArgs = list(),
           otherFactors = list(),
           unitFactors = list()) {
    
    dt <- data.table(
      exptID = matrix(character(0), ncol = 1),
      nBallots = matrix(integer(0),   ncol = 1),
      ranks    = matrix(integer(0),   ncol = length(rankNames)),
      margins  = matrix(double(0),    ncol = length(marginNames))
    )
    colnames(dt) <- c("exptID", "nBallots", rankNames, marginNames)
    setattr(dt, "countMethod",        countMethod)
    setattr(dt, "rankMethod",         rankMethod)
    setattr(dt, "datasetName",        datasetName)
    setattr(dt, "experimentalMethod", experimentalMethod)
    setattr(dt, "countArgs",          countArgs)
    setattr(dt, "startTime",          format(as.POSIXlt(Sys.time())))
    setattr(dt, "otherFactors", otherFactors)
    setattr(dt, "unitFactors",        unitFactors)
    class(dt) <- append("SafeRankExpt", class(dt))
    return(dt)
  }

#' is.SafeRankExpt()
#'
#' @param x object of unknown class
#' @return TRUE if x is a SafeRankExpt object
#' @export
is.SafeRankExpt <- function(x) {
  return(inherits(x, "SafeRankExpt"))
}

#' add a row to a SafeRankExpt object
#'
#' @param object prior results of experimentation
#' @param row    new observations
#'
#' @return updated SafeRankExpt object
#' 
#' @importFrom dplyr bind_rows
rbind.SafeRankExpt <- function(object, row) {
  stopifnot(is.SafeRankExpt(object))
  ##TODO: optimise this code, if level 2 of the R Inferno is ever painfully hot
  object = dplyr::bind_rows(object, row)
  stopifnot(is.SafeRankExpt(object))
  return(object)
}

#' summary method for SafeRankExpt
#'
#' @param object
#'  experimental results to be summarised
#' @param ... args for generic summary()
#'
#' @return summary.SafeRankExpt object
#' @export
summary.SafeRankExpt <- function(object, ...) {
  stopifnot(is.SafeRankExpt(object))
  class(object) <- append("summary.SafeRankExpt", class(object))
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
  cat("\nExperiment ID, number of ballots in simulated election, ranks, winning margins:")
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
#' The score of a candidate is their ranking plus their scaled "winning margin".
#' The scaled margin is \eqn{e^{-cx/\sqrt{n}}}, where \eqn{x} is the
#' unscaled margin (i.e. the number of votes by which this candidate is ahead of
#' the next-weaker candidate), \eqn{n} is the number of ballots in this
#' simulated election, and \eqn{c} is the scaling parameter `cmargin`.
#' 
#' Todo: invert the y axis so that the higher-ranked candidates are the "cream"
#' rather than the "dregs" on the chart.
#' 
#' Todo: list candidates in order of final ranking
#' 
#' Todo: amend so that testFraction(..., ainc=0) is a 2-d plot.
#' 
#' @param x object containing experimental results
#' @param facetWrap TRUE provides per-candidate scatterplots
#' @param cMargin adjustable parameter in scoring
#' @param xlab,ylab axis labels
#' @param point.size diameter of elected/eliminated points
#' @param ... params for generic plot()
#' @return graphics object, with side-effect in RStudio Plots pane
#'
#' @export
#' @importFrom stringr str_detect
#' 
plot.SafeRankExpt <-
  function(x,
           facetWrap = FALSE,
           cMargin = 0.25,
           xlab = "Ballots",
           ylab = "Adjusted Rank",
           point.size = 2,
           ...) {
    stopifnot(requireNamespace("ggplot2", quietly = TRUE))
    stopifnot(requireNamespace("stringr", quietly = TRUE))
    
    ## https://joshuacook.netlify.app/post/integer-values-ggplot-axis/
    integer_breaks <- function(n = 5, ...) {
      fxn <- function(x) {
        breaks <- floor(pretty(x, n, ...))
        names(breaks) <- attr(breaks, "labels")
        breaks
      }
      return(fxn)
    }
    
    ## colnames of margins, ranks, and scores
    lmnames <- colnames(x)[stringr::str_detect(colnames(x), "^m.")]
    cnames <- unlist(lapply(lmnames, function(x)
      stringr::str_sub(x, 3, stringr::str_length(x))))
    snames <- unlist(lapply(cnames, function(x)
      paste0("s.", x)))
    mnames <- unlist(lmnames)
    
    scores <- x[, ..mnames]
    setnames(scores, cnames)
    ## scale by 1/sqrt(n)
    scores <- scores / sqrt(x[, nBallots])  
    ## a small winning margin adds almost a full point of rank
    scores <- exp(-cMargin * scores)
    ## transformed margins of NA are set to 0.0
    for (j in cnames) {
      set(scores, which(is.na(scores[[j]])), j, 0.0)
    }
    ## scores += ranks
    scores <- scores + x[, .SD, .SDcols = cnames] 
    ## include descriptive info
    t <- x[, .SD, .SDcols = c("exptID", "nBallots")]
    scores <- cbind(t, scores)
    
    scores <-
      melt(
        scores,
        id.vars = c("exptID", "nBallots"),
        measure.vars = unlist(cnames),
        variable.name = "Candidate",
        value.name = "Score"
      )
    g <-
      ggplot2::ggplot(scores, aes(x = nBallots, y = Score, colour = Candidate))
    g <- g + geom_point()
    g <- g + scale_y_continuous(breaks = integer_breaks())
    if (facetWrap) {
      g <- g + facet_wrap(~ Candidate)
    }
    return(g)
  }
