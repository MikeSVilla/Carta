#' Carta1
#'
#' @return
#' @export
#'
#' @examples may()
may <- function(){
list.of.packages <- c("svDialogs", "animation", "dplyr", "ggplot2", "pryr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
require(svDialogs)
require(animation)
require(dplyr)
require(ggplot2)
require(pryr)


message <- dlg_message('Quieres salir conmigo el próximo lunes 5 de junio?', 'yesno')$res
if ( message == 'yes' ) {
  dlg_message('Muy bien!'); dlg_message('Te vere en el café conquistador a las 12 pm 
  (el que esta cerca de la alhóndiga, a lado de El viejo Zaguán)'); dlg_message('En caso de que no puedas a esa hora hazmelo saber'); dlg_message('Mándame screenshot de tú respuesta :)') ;dlg_message('Aún no cierres el IDE, espera un poco ya que hay una sorpresa!')
  
  # Valentine's Day Heart Plot
  # Summit Consulting LLC

  # heart curve formula
  heart <- quote((x^2 + y^2 - 1)^3 - x^2 * y^3)

  # formula for heart curve at a given x
  heart_at_x <- function(x) {
    function(y) eval(substitute_q(heart, list(x = x)), list(y = y))
  }

  heart_x <- seq(-1.136, 1.136, 0.001)
  heart_y_lower <- sapply(heart_x, function(x) uniroot(heart_at_x(x), c(-2, 0.6))$root)
  heart_y_upper <- sapply(heart_x, function(x) uniroot(heart_at_x(x), c(0.6, 2))$root)

  # put together data frame
  heart_df <- data.frame(x = rep(heart_x, 2),
                         y = c(heart_y_lower, heart_y_upper))

  # show outline
  with(heart_df, plot(x, y))

  # create a data frame with one row per x, so we can fill in the heart
  heart_df_minmax <- data.frame(x = heart_x,
                                y_min = heart_y_lower,
                                y_max = heart_y_upper)

  set.seed(20150214)
} else {
  message2 <- dlg_message('Estás segura? :(', 'yesno')$res
  if ( message2 == 'yes' ) {
    message3 <- dlg_message('Estás SUPER segura? :(', 'yesno')$res
    if ( message3 == 'yes' ) {
      dlg_message('No debí programar esta respuesta jaja :,( pero ya no sabia que más hacer')
    } else {
      message <- dlg_message('Quieres salir conmigo el próximo lunes 5 de junio?', 'ok')$res
      if ( message == 'ok' ) {
        dlg_message('Muy bien!'); dlg_message('Te vere en el café conquistador a las 12 pm 
(el que esta cerca de la alhóndiga, a lado de El viejo Zaguán)'); dlg_message('En caso de que no puedas a esa hora hazmelo saber'); dlg_message('Mándame screenshot de tú respuesta :)') ;dlg_message('Aún no cierres el IDE, espera un poco ya que hay una sorpresa!')
        
        # Valentine's Day Heart Plot
        # Summit Consulting LLC
        
        # heart curve formula
        heart <- quote((x^2 + y^2 - 1)^3 - x^2 * y^3)

        # formula for heart curve at a given x
        heart_at_x <- function(x) {
          function(y) eval(substitute_q(heart, list(x = x)), list(y = y))
        }

        heart_x <- seq(-1.136, 1.136, 0.001)
        heart_y_lower <- sapply(heart_x, function(x) uniroot(heart_at_x(x), c(-2, 0.6))$root)
        heart_y_upper <- sapply(heart_x, function(x) uniroot(heart_at_x(x), c(0.6, 2))$root)

        # put together data frame
        heart_df <- data.frame(x = rep(heart_x, 2),
                               y = c(heart_y_lower, heart_y_upper))

        # show outline
        with(heart_df, plot(x, y))

        # create a data frame with one row per x, so we can fill in the heart
        heart_df_minmax <- data.frame(x = heart_x,
                                      y_min = heart_y_lower,
                                      y_max = heart_y_upper)

        set.seed(20150214)
      }
    }
  } else {
    message <- dlg_message('Quieres salir conmigo el próximo lunes 5 de junio?', 'ok')$res
    if ( message == 'ok' ) {
      dlg_message('Muy bien!'); dlg_message('Te vere en el café conquistador a las 12 pm 
 (el que esta cerca de la alhóndiga, a lado de El viejo Zaguán)'); dlg_message('En caso de que no puedas a esa hora hazmelo saber'); dlg_message('Mándame screenshot de tú respuesta :)') ;dlg_message('Aún no cierres el IDE, espera un poco ya que hay una sorpresa!')
      
      # Valentine's Day Heart Plot
      # Summit Consulting LLC
      
      # heart curve formula
      heart <- quote((x^2 + y^2 - 1)^3 - x^2 * y^3)

      # formula for heart curve at a given x
      heart_at_x <- function(x) {
        function(y) eval(substitute_q(heart, list(x = x)), list(y = y))
      }

      heart_x <- seq(-1.136, 1.136, 0.001)
      heart_y_lower <- sapply(heart_x, function(x) uniroot(heart_at_x(x), c(-2, 0.6))$root)
      heart_y_upper <- sapply(heart_x, function(x) uniroot(heart_at_x(x), c(0.6, 2))$root)

      # put together data frame
      heart_df <- data.frame(x = rep(heart_x, 2),
                             y = c(heart_y_lower, heart_y_upper))

      # show outline
      with(heart_df, plot(x, y))

      # create a data frame with one row per x, so we can fill in the heart
      heart_df_minmax <- data.frame(x = heart_x,
                                    y_min = heart_y_lower,
                                    y_max = heart_y_upper)

      set.seed(20150214)
    }
  }
}

}
