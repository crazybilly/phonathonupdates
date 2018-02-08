

#' Process Updates Made During Confirmation
#'
#' @param filename a character path to a file
#'
#' @return a list of two data frames: newaddress = people with new addresses; newemail = people with new email addresses
#'
#' @import dplyr
#' @import tidyr
#' @import muadc
#'
#' @export
#'
processconfirmationupdates  <- function(filename) {


  confirmation  <- muadc::read.tidy('data/Updates_During_Confirmations.csv') %>%
    mutate(
      addrnew = address1 != commitaddress1
      , emailnew = email != commitemail
    )


  newaddresses  <- confirmation %>%
    filter(addrnew) %>%
    mutate(action = 'new address') %>%
    select(
      pidm
      , action
      , old_addr1 = address1
      , old_city  = city
      , old_st    = state
      , new_addr1 = commitaddress1
      , new_addr2 = commitaddress2
      , new_city  = commitcity
      , new_state = commitstate
      , new_zip   = commitzip
      , comment   = lastofcomment
    )

  newemail  <- confirmation %>%
    filter(emailnew) %>%
    mutate(action = 'new email') %>%
    select(
      pidm
      , action
      , old_email = email
      , new_email = commitemail
      , comment   = lastofcomment
    )

  list(newaddress = newaddresses, newemail = newemail)

}
