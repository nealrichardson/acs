Sys.setlocale("LC_COLLATE", "C") ## What CRAN does; affects sort order
set.seed(999) ## To ensure that tests that involve randomness are reproducible

acs.fetch <- function (...) acs::acs.fetch(..., key=getOption("census.api.key"))
