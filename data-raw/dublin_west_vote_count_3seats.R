# regression testing on dublin_west is very time-consuming, due to
# inefficiencies in vote::stv(), so we regress against a stored result

dublin_west_vote_count_3seats <- 
  vote::stv(dublin_west, nseats = 3, complete.ranking = TRUE)

usethis::use_data(dublin_west_vote_count_3seats, overwrite = TRUE)
