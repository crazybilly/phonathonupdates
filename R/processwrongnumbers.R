

#' Process Wrong Number Reports
#'
#' @param filename a character path to a file
#'
#' @return a list of three data frames: wrong = wrong numbers; exclusions = NOC and NPS; deceased = folks to mark as deceased
#'
#' @import dplyr
#' @import tidyr
#' @import muadc
#'
#' @export
#'
processwrongnumbers  <- function(filename){

  wrongnumbers  <- muadc::read.tidy( filename )

  wrong  <- wrongnumbers %>%
    filter(lastofresult == 'Wrong#\\Disconnected') %>%
    select(pidm, ends_with('phone'), comment = lastofcomment) %>%
    gather(phonetype, number, -pidm, -comment) %>%
    mutate(action = 'deactivate phone number') %>%
    filter(!is.na(number)) %>%
    arrange(pidm) %>%
    select(pidm, action, phonetype, number, comment)


  exclusions  <- wrongnumbers %>%
    filter(lastofresult == 'Do Not Contact' | lastofresult == 'No Phone Solicitation') %>%
    mutate(action = 'add exclusion') %>%
    select(pidm, action, result = lastofresult, comment = lastofcomment)


  deceased  <- wrongnumbers %>%
    filter(lastofresult == 'Deceased') %>%
    mutate(action = 'mark deceased') %>%
    select(pidm, action, contains('name'), comment = lastofcomment)


  list(wrongnumbers = wrong, exclusions = exclusions, deceased = deceased)


}
