#' Process Update Files
#'
#' @param filename a character path to a file
#'
#' @return a list of four data frames: newaddress = people with new addresses; newemail = people with new email addresses; newjob = people with new jobs; newphone = new phone numbers
#'
#' @import dplyr
#' @import tidyr
#' @import muadc
#'
#' @export
#'
processudpates  <- function(filename) {


  updates  <- muadc::read.tidy( filename ) %>%
    mutate(
        addrnew = address1 != updatedaddress1& !is.blank(updatedaddress1)
      , jobnew  = (company != updatedemployer & !is.blank(updatedemployer) ) | (title != updatedjobtitle & !is.blank(updatedjobtitle))
      , emailnew = ( (tolower(personalemail) != tolower(updatedprefemail)) & !is.blank(updatedprefemail) )| (prefemailtype != updatedprefemailtype & !is.blank(updatedprefemailtype) )
      , phonenew = (!is.blank(updatedphone) | !is.blank(updatedprefphonetype) )
      , namenew = !is.blank(updatedname) | !is.blank(updatednamereason)
    )


  updateaddr  <-  updates %>%
    filter(addrnew) %>%
    mutate( action = 'new address') %>%
    select(
      pidm
      , action
      , old_addr1 = address1
      , old_city  = city
      , old_st    = state
      , new_addr1 = updatedaddress1
      , new_addr2 = updatedaddress2
      , new_city  = updatedcity
      , new_state = updatedstate
      , new_zip   = updatedzip
      , comment   = lastofcomment
    )


  updateemail  <- updates %>%
    filter(emailnew) %>%
    mutate(
      action = 'new email'
      , updatedprefemail = tolower(updatedprefemail)
    ) %>%
    select(
      pidm
      , action
      , old_email = email
      , new_email = updatedprefemail
      , new_email_type = updatedprefemailtype
      , comment   = lastofcomment
    )


  updatejob  <- updates %>%
    filter(jobnew) %>%
    mutate( action = 'new employment' ) %>%
    select(
      pidm
      , action
      , old_employer = company
      , old_title = title
      , new_employer = updatedemployer
      , new_title    = updatedjobtitle
      , comment   = lastofcomment
    )


  updatephone  <- updates %>%
    filter(phonenew) %>%
    mutate(action = 'new phone') %>%
    select(
      pidm
      , action
      , old_cell  = cellphone
      , old_home  = homephone
      , new_phone = updatedphone
      , new_phone_type = updatedprefphonetype
    )


  updatename  <- updates %>%
    filter(namenew) %>%
    mutate(
        action = 'new name'
      , old_name = paste(lastname, firstname, sep = ", ")
    ) %>%
    select(
      pidm
      , action
      , old_name
      , updatedname
      , updatednamereason
    )

  list(newaddress = updateaddr, newemail = updateemail, newjob = updatejob, newphone = updatephone, newname = updatename)


}
