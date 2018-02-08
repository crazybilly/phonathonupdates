

#' Process Phonathon Reports
#'
#' @param directory
#' @param startdate
#' @param updatetypes
#'
#' @import dplyr
#' @import stringr
#' @import lubridate
#' @import muadc
#'
#' @return
#' @export
#'
processphonathon  <- function(directory = 'data', startdate = Sys.time(), updatetypes = c('badnumbers','wrongnumbers','confirmations','updates') ) {

  possibledateformats  <- c(
      "mdY"
    , "dmY"
    , "Ymd"

  )


  if(!stringr::str_detect(paste0(class(startdate), collapse= ","),  "POSIX|Date") ) {
    startdate  <- lubridate::parse_date_time(
        startdate
      , lubridate::guess_formats(jan1, order = possibledateformats)
    )
  }

  cutoff  <- startdate - lubridate::hours(10)

  allfiles  <- list.files(path = directory, full.names = T )

  fileinfo  <- file.info(allfiles)

  fileindex  <- fileinfo$mtime >= cutoff

  filestoread  <- allfiles[fileindex]


  purrr::map(filestoread, ~
    if(        stringr::str_detect(.x, stringr::regex("bad_phone"   , ignore_case = T)) ) {
      processbadnumbers(.x) %>% bind_rows
    } else if (stringr::str_detect(.x, stringr::regex("Wrong_Number", ignore_case= T) ) ) {
      processwrongnumbers(.x)%>% bind_rows
    } else if (stringr::str_detect(.x, stringr::regex("confirm"     , ignore_case= T) ) ) {
      processconfirmationupdates(.x)%>% bind_rows
    } else if (stringr::str_detect(.x, stringr::regex("_update.?_"  , ignore_case= T) ) ) {
      processudpates(.x)%>% bind_rows
    }
  ) %>%
  bind_rows


}
