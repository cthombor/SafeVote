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
    crMargins <- cr$margins
    names(crMargins) <- marginNames
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
      crMargins <- cr$margins
      names(crMargins) <- marginNames
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
  crMargins <- cr$margins
  names(crMargins) <- marginNames
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
    crMargins <- cr$margins
    names(crMargins) <- marginNames
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
    crMargins <- cr$margins
    names(crMargins) <- marginNames
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
    names(ranks) <- colnames(cr$data)
  } else {
    ## rearrange a numeric ranking vector, if necessary
    ranks <- cr[[rankMethod]][colnames(cr$data)]
  }
  return(ranks)
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
rbind.SafeRankExpt <- function(object, row) {
  stopifnot(is.SafeRankExpt(object))
  ## rbind() produces a SafeRankExpt object with no attributes
  ao <- attributes(object)
  object = rbind(object, row, use.names = TRUE)
  
  attributes(object) <- ao
  ## at this point 'object' is apparently corrupt, because the following attempt
  ## to create a new column (for analytic purposes) triggers a warning.
  ##
  ## > object[, s.Bonnie := Bonnie + m.Bonnie/nBallots] Warning message:
  ## In`[.data.table`(object, , `:=`(s.Bonnie, Bonnie + m.Bonnie/nBallots)) :
  ## Invalid .internal.selfref detected and fixed by taking a (shallow) copy of
  ## the data.table so that := can add this new column by reference. At an
  ## earlier point, this data.table has been copied by R (or was created
  ## manually using structure() or similar). Avoid names<- and attr<- which in R
  ## currently (and oddly) may copy the whole data.table. Use set* syntax
  ## instead to avoid copying: ?set, ?setnames and ?setattr. If this message
  ## doesn't help, please report your use case to the data.table issue tracker
  ## so the root cause can be fixed or this message improved.
  ##
  ## TODO: either do some more hacking (e.g. using setattr() to copy all
  ## object-level attributes individually into the modified data.table), or give
  ## up on data.table (which apparently is not an appropriate choice for data
  ## collection during stochastic experimentation, unless you collect your data
  ## in a transposed fashion (with experimental units on columns and variables
  ## on rows) *and* you do something to reliably work-around its lack of support
  ## for object-level attributes.
  
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
  return(invisible(object))
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