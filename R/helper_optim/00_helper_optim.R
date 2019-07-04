gr.gb2 <- function(theta) {
  a <- theta[1]
  b <- theta[2]
  p <- theta[3]
  q <- theta[4]
  pr <- theta[5]
  t <- qgb2(pr, a, b, p, q)
  lcgb2 <- pgb2(t, a, b, p + 1 / a, q - 1 / a)
  return(-lcgb2)
}
scale.gb2 <- function(theta, m) {
  a <- theta[1]
  p <- theta[2]
  q <- theta[3]
  parb <- m/(beta(p + 1 / a, q - 1 / a) / beta(p, q))
  return(parb)
}
moment.gb2 <- function(theta, r) {
  a <- theta[1]
  b <- theta[2]
  p <- theta[3]
  q <- theta[4]
  if (q > r/a) {
    m.gb2 <- b^r*beta(p + r / a, q - r / a) / beta(p, q)
  }
  if (q <= r/a) {
    m.gb2 <- sum(rgb2(10^4, a, b, p, q)^r)/10^4
  }
  return(m.gb2)
}
gini_b2 <- function(theta) {
  p <- theta[1]
  q <- theta[2]
  2 * beta(2 * p, 2 * q - 1) / (p * beta(p, q)^2)
}

gini_sm <- function(theta) {
  a <- theta[1]
  q <- theta[2]
  1 - gamma(q) * gamma(2 * q - 1 / a) / (gamma(q - 1 / a) * gamma(2 * q))
}

gini_d <- function(theta) {
  a <- theta[1]
  p <- theta[2]
  gamma(p) * gamma(2 * p + 1 / a) / (gamma(p + 1 / a)*gamma(2 * p)) - 1
}

gini_ln <- function(theta) {
  s <- theta[1]
  2 * pnorm(s / 2^0.5) - 1
}
dmoment.gb2 <- function(theta, x, r) {
  a <- theta[1]
  b <- theta[2]
  p <- theta[3]
  q <- theta[4]
  dm.gb2 <- pgb2(x, a, b, p + r/a, q - r/a)
  return(dm.gb2)
}
lc.gb2 <- function(theta, pr) {
  a <- theta[1]
  b <- theta[2]
  p <- theta[3]
  q <- theta[4]
  t <- qgb2(pr, a, b, p, q)
  lcgb2 <- pgb2(t, a, b, p + 1 / a, q - 1 / a)
  return(lcgb2)
}
weight.mat.gb2 <- function(theta, pr) {
  a <- theta[1]
  b <- theta[2]
  p <- theta[3]
  q <- theta[4]
  if (q > 2/a) {
    e1 <- moment.gb2(theta, 1)
    e2 <- moment.gb2(theta, 2)
    w.cop  <- function(x, y) {
      e2*dmoment.gb2(theta, qgb2(x, a, b, p, q), 2) + (x * qgb2(x, a, b, p, q) - e1 * lc.gb2(theta, x)) *
        (qgb2(y, a, b, p, q) - y * qgb2(y, a, b, p, q) + e1 * lc.gb2(theta, y)) - qgb2(x, a, b, p, q) * e1 * lc.gb2(theta, x)
    }
    w.mat <- outer(c(pr, 0.999999999), c(pr, 0.999999999), w.cop) # Quantile of order p = 0.999999999 is used to approximate the maximum
    w.mat[lower.tri(w.mat)] <- t(w.mat)[lower.tri(w.mat)]
    y.cop <- matrix(cbind(diag(1/e1, length(pr)), -lc.gb2(theta, pr) / e1), length(pr), length(pr) + 1)
    return(solve(y.cop %*% w.mat %*% t(y.cop)))
  }
  else {
    stop('q <= 2/a')
  }
}

opt.gmm.gb2 <- function(x, y, init.est, cons.est, est.method = 1){

  minfun <- function(theta, x, y) {
    moments.gmm <- matrix((lc.gb2(theta, x) - y), length(x), 1)
    min.gmm <- t(moments.gmm) %*% weight.mat.gb2(cons.est, x) %*% moments.gmm
    return(min.gmm)
  }

  opt1 <- try(optim(init.est,
                    minfun,
                    gr = NULL,
                    x,
                    y,
                    method = "BFGS",
                    control = list(parscale = init.est, pgtol = 1e-08)))
  if (est.method == 2 | 'try-error' %in% class(opt1)) {
    opt1 <- try(optim(init.est,
                      minfun,
                      gr = NULL,
                      x,
                      y,
                      method = "L-BFGS-B",
                      lower = 0,
                      control = list(parscale = init.est, pgtol = 1e-08)))
  }

  if (!'try-error' %in% class(opt1)) {
    return(list(opt1 = opt1))
  }
}
simgini.gb2 <-  function(theta, size = 10^6) {
  if (length(theta) == 3 & sum(is.na(theta)) == 0) {
    a <- theta[1]
    b <- 1
    p <- theta[2]
    q <- theta[3]
  }
  if ((length(theta) == 4 & is.na(theta[2])) & sum(is.na(theta)) == 1) {
    a <- theta[1]
    b <- 1
    p <- theta[3]
    q <- theta[4]
  }
  if (length(theta) == 4 & sum(is.na(theta)) == 0) {
    a <- theta[1]
    b <- theta[2]
    p <- theta[3]
    q <- theta[4]
  }
  gini.t <- gini.gb2(a, p, q)
  if (is.nan(gini.t) | is.na(gini.t) | gini.t == 1) {
    sim <- rgb2(size, a, b, p, q)
    sim <- sim[is.finite(sim)]
    gini.t <- ineq::Gini(sim, na.rm = TRUE)
  }
  return(gini.t)
}
