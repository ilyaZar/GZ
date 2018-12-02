################################################################################
######################## PGAS model functions  SV model ########################
################################################################################
#
#
#
#
#
f <- function(xtt, z, t, phi_x, bet_x) {
  # xt <- phi_x*xtt
  xt <- phi_x*xtt + z %*% bet_x
  # xt <- phi_x*xtt + 8*cos(1.2*t)
  # xt <- phi_x*xtt + 25*xtt/(1 + xtt^2) 
  # xt <- phi_x*xtt + 25*xtt/(1 + xtt^2) + 8*cos(1.2*t)
  return(xt)
}
# g <- function(xt) {
#   yt <- xt^2/20
#   return(yt)
# }
