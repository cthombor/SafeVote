#' Tests from legacy code, ported to testthat 3e

testthat("default nseats, when testing stv() with five candidates", {
  skip_if_not_installed("vote")
  nc <- 5
  expect_equal(
    floor(nc / 2),
    SafeVote::check.nseats(
      nseats = NULL,
      ncandidates = nc,
      default = floor(nc/2)
    )
  )
})

testthat("default nseats, when using stv() to rank candidates", {
  skip_if_not_installed("vote")
  nc <- 5
  expect_equal(
    nc,
    SafeVote::check.nseats(
      nseats = NULL,
      ncandidates = nc,
      complete.ranking = TRUE,
      default = nc
    )
  )
})
