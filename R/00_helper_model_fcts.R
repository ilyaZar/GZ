################################################################################
######################## PGAS model functions  SV model ########################
################################################################################
#
#
#
#
#
f <- function(xa_tt, za, phi_xa, bet_xa) {
  # xt <- phi_x*xtt
  xt <- phi_xa*xa_tt + za %*% bet_xa
  # xt <- phi_x*xtt + 8*cos(1.2*t)
  # xt <- phi_x*xtt + 25*xtt/(1 + xtt^2)
  # xt <- phi_x*xtt + 25*xtt/(1 + xtt^2) + 8*cos(1.2*t)
  return(xt)
}
# g <- function(xt) {
#   yt <- xt^2/20
#   return(yt)
# }
