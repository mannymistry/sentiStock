#' Get all News as far back as you can
#'
#' @param ticker : Ticker in question
#' @param days_back : numeber of days back you need to go
#'
#' @return
#' @export
#'
#' @examples
get_all_news <- function(ticker, days_back){
PRE_ALLOC_MAX <- 30
results <- vector(mode = "list", length = PRE_ALLOC_MAX)
i <- 1
from <- 0
repeat {
  res <-riingo::riingo_news(ticker=ticker, start_date=Sys.Date()-days_back,
                                 end_date=Sys.Date(), limit=1000, offset = from)
  results <- rbind(res, results)
  if (nrow(res) == 1000) {
    message("Fetching next 1000 records...")
    i <- i + 1
    from <-  from + 1000
  } else {
    break
  }
}
results
}


