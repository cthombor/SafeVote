% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stv.R
\name{image.SafeVote.stv}
\alias{image.SafeVote.stv}
\title{visualisation of joint and marginal distributions in STV preferences}
\usage{
\method{image}{SafeVote.stv}(x, xpref = 2, ypref = 1, all.pref = FALSE, proportion = TRUE, ...)
}
\arguments{
\item{x}{STV results to be visualised}

\item{xpref, ypref}{candidates shown in a joint distribution plot}

\item{all.pref}{plot the joint distribution of two preferences (if
\code{all.pref=FALSE}) or the marginal distribution of all preferences (if
\code{all.pref=TRUE}).}

\item{proportion}{The joint distribution can be shown either as proportions
(if \code{proportion=TRUE}) or raw vote counts (if \code{proportion=FALSE}).}

\item{...}{args passed to fields::image.plot()}
}
\value{
image object, with side-effect in RStudio Plots pane
}
\description{
visualisation of joint and marginal distributions in STV preferences
}
