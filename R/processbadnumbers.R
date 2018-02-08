


#' Process Bad Numbers
#'
#' @param filelocation a character path to a file
#'
#' @return a data frame with one row per number that needs to be inactivated
#'
#' @import dplyr
#' @import tidyr
#' @import muadc
#'
#' @export
#'
processbadnumbers  <- function(filelocation) {

  badnumbers  <- muadc::read.tidy( filelocation )


  # what was the best existing number for each person
  badbest  <- badnumbers %>%
    select(pidm, bestnumber = bestexistingphonenumber) %>%
    mutate(
      bestnumber = case_when(
           bestnumber == "MU cell phone"     ~ "cellphone"
        , bestnumber == "MU home phone"     ~ "homephone"
        , bestnumber == "MU business phone" ~ "businessphone"
      )
      , best = T
    )

  # which number was actully succesful?
  badsuccessful  <- badnumbers %>%
    select(pidm, successfulnumber = successfulphone) %>%
    mutate(
      successfulnumber = case_when(
        successfulnumber == "Cell Phone"     ~ "cellphone"
        , successfulnumber == "Home Phone"     ~ "homephone"
        , successfulnumber == "MU business phone" ~ "businessphone"
      )
      , successful = T
    )

  # all numbers in long format
  badlong  <- badnumbers %>%
    select(pidm, cellphone, homephone, businessphone) %>%
    gather(phonetype,number,  -pidm) %>%
    filter(!is.blank(number)) %>%
    # was this number the best existing?
    left_join(badbest, by = c('pidm', 'phonetype' = 'bestnumber')) %>%
    # was this number the succesful one?
    left_join(badsuccessful, by = c('pidm', 'phonetype' = 'successfulnumber')) %>%
    # get the scores for each phone
    mutate(
      phonescore = case_when(
        phonetype == 'cellphone'     ~ 1
        , phonetype == 'homephone'     ~ 2
        , phonetype == 'businessphone' ~ 3
      )
    ) %>%
    group_by(pidm) %>%
    arrange(phonescore)  %>%
    ungroup()


  # calculate where the succesful number is
  #   all the numbers before this are bad
  badfilter  <- badlong %>%
    filter(successful) %>%
    left_join(badnumbers %>% select(pidm, successfulphoneformulanegativebads), by = 'pidm') %>%
    mutate(badscore = phonescore + successfulphoneformulanegativebads)

  # filter so we only display good numbers
  actualbad  <- badlong %>%
    left_join(badfilter %>% select(pidm, badscore), by = 'pidm') %>%
    filter(phonescore <= badscore) %>%
    left_join(badnumbers %>% select(pidm, comment = lastofcomment), by = 'pidm') %>%
    mutate(action = 'deactivate phone number') %>%
    select(pidm, action, phonetype, number, comment)

}
