#
# This is a Plumber API. You can run the API by clicking
# the 'Run API' button above.
#
# Find out more about building APIs with Plumber here:
#
#    https://www.rplumber.io/
#
source("simulation.R")
library(plumber)
library(stringr)
#* @filter cors
cors <- function(req, res) {
  print("cors filter")
  res$setHeader("Access-Control-Allow-Origin", "*") # Or whatever
  res$setHeader("Access-Control-Allow-Methods",'POST')
  res$setHeader("Access-Control-Allow-Headers", "Content-Type, Access-Control-Allow-Headers, Authorization, X-Requested-With")
  if (req$REQUEST_METHOD == "OPTIONS") {
    res$setHeader("Access-Control-Allow-Methods","*")
    res$setHeader("Access-Control-Allow-Headers", req$HTTP_ACCESS_CONTROL_REQUEST_HEADERS)
    res$status <- 200 
    return(list())
  } else {
    plumber::forward()
  }
}

#* Submit Simulation Inputs
#* @parser json
#* @parser form
#* @post /simulate
function (req, res) {
    body = req$body
    formBody <- parse_form(req);
    print(body)
    print(formBody)
   if (length(body) != 7) {
    msg <- str_interp("Invalid request body")
    res$status <- 400 # Bad request
    return(list(error=jsonlite::unbox(msg)))
  }

  if (is.null(body$n_feeds)) {
    msg <- str_interp("Please, add n_feeds")
    res$status <- 400 # Bad request
    return(list(error=jsonlite::unbox(msg)))
  }
  if (is.null(body$deposit_average)) {
    msg <- str_interp("Please, add deposit_average")
    res$status <- 400 # Bad request
    return(list(error=jsonlite::unbox(msg)))
  }
  if (is.null(body$deposit_spread)) {
    msg <- str_interp("Please, add deposit_spread")
    res$status <- 400 # Bad request
    return(list(error=jsonlite::unbox(msg)))
  }
  if (is.null(body$feed_average)) {
    msg <- str_interp("Please, add feed_average")
    res$status <- 400 # Bad request
    return(list(error=jsonlite::unbox(msg)))
  }
  if (is.null(body$feed_spread)) {
    msg <- str_interp("Please, add feed_spread")
    res$status <- 400 # Bad request
    return(list(error=jsonlite::unbox(msg)))
  }
  if (is.null(body$acc_MAD)) {
    msg <- str_interp("Please, add acc_MAD")
    res$status <- 400 # Bad request
    return(list(error=jsonlite::unbox(msg)))
  }
  if (is.null(body$sc_fee)) {
    msg <- str_interp("Please, add sc_fee")
    res$status <- 400 # Bad request
    return(list(error=jsonlite::unbox(msg)))
  }

 simulation <- oracle_simulation(
     body$n_feeds,
     body$deposit_average,
     body$deposit_spread,
     body$feed_average,
     body$feed_spread,
     body$acc_MAD,
     body$sc_fee
 )

 return (simulation)
}

