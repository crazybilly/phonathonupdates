

#' Process Phonathon Reports
#'
#' @param directory the directory path where update files are stored. Defaults to getwd()/data
#' @param startdate a date or time. Files modified within 10 hours or earlier than this time will be processed. Defaults to now.
#' @param updatetypes a character vector listing which types of updates should be processed.
#'
#' @import dplyr
#' @import stringr
#' @import lubridate
#' @import muadc
#'
#' @return a single data frame with all processed updates bound together
#'
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
    if(
      stringr::str_detect(.x, stringr::regex("bad_phone"   , ignore_case = T))
      & 'badnumbers' %in% updatetypes
    ) {
        processbadnumbers(.x) %>% bind_rows

    } else if (
      stringr::str_detect(.x, stringr::regex("Wrong_Number", ignore_case= T) )
      & 'wrongnumbers' %in% updatetypes
    ) {
        processwrongnumbers(.x)%>% bind_rows

    } else if (
      stringr::str_detect(.x, stringr::regex("confirm"     , ignore_case= T) )
      & 'confirmations' %in% updatetypes
    ) {
        processconfirmationupdates(.x)%>% bind_rows

    } else if (
      stringr::str_detect(.x, stringr::regex("_update.?_"  , ignore_case= T) )
      & 'updates' %in% updatetypes
    ) {
        processudpates(.x)%>% bind_rows

    }
  ) %>%
  bind_rows


}
