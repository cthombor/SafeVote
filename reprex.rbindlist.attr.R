xdf <- data.frame(data=c(0))
attr(xdf, "xa") <- 0
xdf <- dplyr::bind_rows(xdf, c(data=1))
# I expect attributes(xdf) to include "xa".  (PASS)
attributes(xdf)

xdf <- data.frame(data=c(0))
attr(xdf, "xa") <- 0
xdf <- rbind(xdf, c(data=1))
# I expect attributes(xdf) to include "xa".  (PASS)
attributes(xdf)

library(data.table)
xdt <- data.table(data=0)
setattr(xdt, "xa", 0)
xdt <- dplyr::bind_rows(xdt, c(data=1))
# I expect class(xdt) to have "data.table" as its first element (PASS)
class(xdt)
# I expect xdt to have an attribute "xa" (PASS)
attributes(xdt)
# I expect class(xdt) to have a valid .internal.selfref structure.  (FAIL)
data.table:::selfrefok(xdt)

library(data.table)
xdt <- data.table(data=0)
setattr(xdt, "xa", 0)
xdt <- rbindlist(list(xdt, data.table(data=1)))
# I expect class(xdt) to have a valid .internal.selfref structure.  (PASS)
data.table:::selfrefok(xdt)
# I expect attributes(xdt) to include "xa".  (FAIL)
attributes(xdt)

