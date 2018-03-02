

#' Process Phonathon Reports
#'
#' @param directory the directory path where update files are stored. Defaults to getwd()/data
#' @param saveto the file path where you want to save the file. Use NA to bypass saving the file
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
processphonathon  <- function(directory = 'data', saveto = str_replace_all(paste0("output/phonathonupdates-",Sys.time(),".csv"),":| ", "-"), startdate = Sys.time(), updatetypes = c('badnumbers','wrongnumbers','confirmations','updates') ) {

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

  # process each file appropriately
  results  <- purrr::map(filestoread, ~
    if(
      stringr::str_detect(.x, stringr::regex("bad_phone"   , ignore_case = T))
      & 'badnumbers' %in% updatetypes
    ) {
        message("Processing bad phone numbers - ",.x, "...\n")
        processbadnumbers(.x) %>% bind_rows

    } else if (
      stringr::str_detect(.x, stringr::regex("Wrong_Number", ignore_case= T) )
      & 'wrongnumbers' %in% updatetypes
    ) {
        message("Processing wrong numbers - ",.x, "...\n")
        processwrongnumbers(.x)%>% bind_rows

    } else if (
      stringr::str_detect(.x, stringr::regex("confirm"     , ignore_case= T) )
      & 'confirmations' %in% updatetypes
    ) {
        message("Processing confirmation updates - ",.x, "...\n")
        processconfirmationupdates(.x)%>% bind_rows

    } else if (
      stringr::str_detect(.x, stringr::regex("_update.?_"  , ignore_case= T) )
      & 'updates' %in% updatetypes
    ) {
        message("Processing update files - ",.x, "...\n")
        processudpates(.x)%>% bind_rows

    }
  )

  results2  <- bind_rows(results )


  # write the file
  if(!is.blank(saveto) ) {

    # create the directory if it doesn't exist
    if(!dir.exists(dirname(saveto)) ) {
      dir.create(dirname(saveto))
    }

     muadc::write.tidy(results2, saveto)

  }

  results2


}
