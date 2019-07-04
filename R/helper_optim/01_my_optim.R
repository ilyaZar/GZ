my_optim <- function(y, x = rep(1/length(y), length(y)), gini.e, pc.inc = NULL,
          se.gmm = FALSE, se.nls = FALSE, se.scale = FALSE, N = NULL,
          nrep = 10^3, grid = 1:20, rescale = 1000, gini = FALSE) {
  if (length(y) != length(x)) {
    stop("x and y must be of the same length")
  }
  if (length(y) < 4) {
    stop("At least four points of the Lorenz curve are required to perform the estimation")
  }
  if (!is.numeric(gini.e)) {
    stop("Gini index is not numeric")
  }
  if (gini.e < 0 | gini.e > 1) {
    stop("Gini index must be between 0 and 1")
  }
  if (!is.numeric(pc.inc) & !is.null(pc.inc)) {
    stop("Per capita income is not numeric")
  }
  if (is.numeric(pc.inc)) {
    if (pc.inc <= 0) {
      stop("Per capita GDP should be positive")
    }
  }
  if (!is.numeric(rescale)) {
    stop("Rescale is not numeric")
  }
  if (rescale <= 0) {
    stop("Rescale must be positive")
  }
  if (sum(grid <= 0) != 0) {
    stop("Grid must be a secuence of positive numbers")
  }
  share <- as.vector(y[!is.na(y)])
  share <- as.numeric(share)/sum(share)
  share <- cumsum(share)[-length(share)]
  cprob <- as.vector(x[!is.na(x)])
  cprob <- as.numeric(cprob)/sum(cprob)
  cprob <- cumsum(cprob)[-length(cprob)]
  par.a <- rep(0, 4 * length(grid))
  par.p <- rep(0, 4 * length(grid))
  par.q <- rep(0, 4 * length(grid))
  rss <- rep(1000, 4 * length(grid))
  # browser()
  for (j in (1:length(grid))) {
    ginit <- function(p) {
      gini_b2(c(p, grid[j])) - gini.e
    }
    free.p <- try(suppressWarnings(uniroot(ginit, c(0.5,
                                                    1000))), silent = TRUE)
    if ("try-error" %in% class(free.p)) {
      ginit <- function(p) {
        gini_b2(c(grid[j], p)) - gini.e
      }
      free.p <- try(suppressWarnings(uniroot(ginit, c(0.5,
                                                      1000))), silent = TRUE)
      if (!"try-error" %in% class(free.p)) {
        regress <- try(suppressWarnings(nlsLM(share ~
                                                (lc.gb2(c(A, 1, P, Q), cprob)), algorithm = "port",
                                              start = list(A = 1, P = grid[j], Q = free.p$root),
                                              lower = c(0, 0, 0), control = nls.lm.control(maxiter = 1000))),
                       silent = TRUE)
        if (!"try-error" %in% class(regress)) {
          par.a[j] <- coef(regress)[1]
          par.p[j] <- coef(regress)[2]
          par.q[j] <- coef(regress)[3]
          rss[j] <- sum(resid(regress)^2)
        }
      }
    }
    else {
      regress <- try(suppressWarnings(nlsLM(share ~ (lc.gb2(c(A,
                                                              1, P, Q), cprob)), algorithm = "port", start = list(A = 1,
                                                                                                                  P = free.p$root, Q = grid[j]), lower = c(0, 0,
                                                                                                                                                           0), control = nls.lm.control(maxiter = 1000))),
                     silent = TRUE)
      if (!"try-error" %in% class(regress)) {
        par.a[j] <- coef(regress)[1]
        par.p[j] <- coef(regress)[2]
        par.q[j] <- coef(regress)[3]
        rss[j] <- sum(resid(regress)^2)
      }
    }
    ginit <- function(p) {
      gini_sm(c(p, grid[j])) - gini.e
    }
    free.p <- try(suppressWarnings(uniroot(ginit, c(0.5,
                                                    1000))), silent = TRUE)
    if ("try-error" %in% class(free.p)) {
      ginit <- function(p) {
        gini_sm(c(grid[j], p)) - gini.e
      }
      free.p <- try(suppressWarnings(uniroot(ginit, c(0.5,
                                                      1000))), silent = TRUE)
      if (!"try-error" %in% class(free.p)) {
        regress <- try(suppressWarnings(nlsLM(share ~
                                                (lc.gb2(c(A, 1, P, Q), cprob)), algorithm = "port",
                                              start = list(A = grid[j], P = 1, Q = free.p$root),
                                              lower = c(0, 0, 0), control = nls.lm.control(maxiter = 1000))),
                       silent = TRUE)
        if (!"try-error" %in% class(regress)) {
          par.a[length(grid) + j] <- coef(regress)[1]
          par.p[length(grid) + j] <- coef(regress)[2]
          par.q[length(grid) + j] <- coef(regress)[3]
          rss[length(grid) + j] <- sum(resid(regress)^2)
        }
      }
    }
    else {
      regress = try(suppressWarnings(nlsLM(share ~ (lc.gb2(c(A,
                                                             1, P, Q), cprob)), algorithm = "port", start = list(A = free.p$root,
                                                                                                                 P = 1, Q = grid[j]), lower = c(0, 0, 0), control = nls.lm.control(maxiter = 1000))),
                    silent = TRUE)
      if (!"try-error" %in% class(regress)) {
        par.a[length(grid) + j] <- coef(regress)[1]
        par.p[length(grid) + j] <- coef(regress)[2]
        par.q[length(grid) + j] <- coef(regress)[3]
        rss[length(grid) + j] <- sum(resid(regress)^2)
      }
    }
    ginit <- function(p) {
      gini_d(c(p, grid[j])) - gini.e
    }
    free.p <- try(uniroot(ginit, c(0.5, 1000)), silent = TRUE)
    if ("try-error" %in% class(free.p)) {
      ginit <- function(p) {
        gini_d(c(grid[j], p)) - gini.e
      }
      free.p <- try(uniroot(ginit, c(0.5, 1000)), silent = TRUE)
      if (!"try-error" %in% class(free.p)) {
        regress <- try(suppressWarnings(nlsLM(share ~
                                                (lc.gb2(c(A, 1, P, Q), cprob)), algorithm = "port",
                                              start = list(A = grid[j], P = free.p$root,
                                                           Q = 1), lower = c(0, 0, 0), control = nls.lm.control(maxiter = 1000))),
                       silent = TRUE)
        if (!"try-error" %in% class(regress)) {
          par.a[2 * length(grid) + j] <- coef(regress)[1]
          par.p[2 * length(grid) + j] <- coef(regress)[2]
          par.q[2 * length(grid) + j] <- coef(regress)[3]
          rss[2 * length(grid) + j] <- sum(resid(regress)^2)
        }
      }
    }
    else {
      regress <- try(suppressWarnings(nlsLM(share ~ (lc.gb2(c(A,
                                                              1, P, Q), cprob)), algorithm = "port", start = list(A = free.p$root,
                                                                                                                  P = grid[j], Q = 1), lower = c(0, 0, 0), control = nls.lm.control(maxiter = 1000))),
                     silent = TRUE)
      if (!"try-error" %in% class(regress)) {
        par.a[2 * length(grid) + j] <- coef(regress)[1]
        par.p[2 * length(grid) + j] <- coef(regress)[2]
        par.q[2 * length(grid) + j] <- coef(regress)[3]
        rss[2 * length(grid) + j] <- sum(resid(regress)^2)
      }
    }
    regress = try(suppressWarnings(nlsLM(share ~ (lc.gb2(c(A,
                                                           1, P, Q), cprob)), algorithm = "port", start = list(A = 1,
                                                                                                               P = 1, Q = grid[j]), lower = c(0, 0, 0), control = nls.lm.control(maxiter = 1000))),
                  silent = TRUE)
    if (!"try-error" %in% class(regress)) {
      par.a[3 * length(grid) + j] <- coef(regress)[1]
      par.p[3 * length(grid) + j] <- coef(regress)[2]
      par.q[3 * length(grid) + j] <- coef(regress)[3]
      rss[3 * length(grid) + j] <- sum(resid(regress)^2)
    }
  }
  # browser()
  rg <- which.min(rss)
  nls.rss <- min(rss)
  nls.a <- par.a[rg]
  nls.p <- par.p[rg]
  nls.q <- par.q[rg]
  if (nls.q <= 1/nls.a) {
    temp.b <- NA
    print("Unable to compute the scale parameter and the GMM estimation. Condition for the existence of the first moment violated: q <= 1 / a")
  }
  if (is.null(pc.inc)) {
    temp.b <- NA
    print("Unable to compute the scale parameter and the GMM estimation. Per capita GDP not provided")
  }
  if (!is.null(pc.inc) & (nls.q > 1/nls.a)) {
    incpc <- pc.inc/rescale
    temp.b <- scale.gb2(c(nls.a, nls.p, nls.q), incpc)
  }
  if (nls.q <= 2/nls.a) {
    print("Unable to compute GMM estimates of the parameters. Condition for the existence of the second moment violated: q <= 2 / a")
  }
  if (nls.q <= 2/nls.a | is.na(temp.b)) {
    gmm.coef <- matrix(NA, 1, 4)
    gmm.se <- matrix(NA, 1, 4)
    gmm.rss <- NA
  }
  else {
    regress <- try(opt.gmm.gb2(cprob, share, init.est = c(nls.a,
                                                          temp.b, nls.p, nls.q), cons.est = c(nls.a, temp.b,
                                                                                              nls.p, nls.q)))
    if ("try-error" %in% class(regress$opt1)) {
      print("Unable to compute GMM estimates of the parameters. The weight martrix cannot be inverted. Try changing the value of rescale")
      gmm.coef <- matrix(NA, 1, 4)
      gmm.se <- matrix(NA, 1, 4)
      gmm.rss <- NA
    }
    else {
      gmm.rss <- regress$opt1$value
      gmm.coef <- regress$opt1$par
      gmm.coef[2] <- scale.gb2(c(gmm.coef[1], gmm.coef[3],
                                 gmm.coef[4]), incpc)
      gmm.se <- matrix(NA, 1, 4)
    }
  }
  nls.estimation <- matrix(NA, 2, 4)
  nls.coef <- matrix(c(nls.a, temp.b, nls.p, nls.q), 1, 4)
  nls.se <- matrix(NA, 1, 4)
  if (se.nls == TRUE) {
    if (is.null(N)) {
      print("Unable to compute the standard errors. Please provide the sample size")
    }
    else {
      se.calc <- simsd.gb2(x = x, theta = nls.coef, N = N,
                           nrep = nrep, se.scale = se.scale)
      nls.se <- se.calc$nls.se
      if (!is.na(temp.b)) {
        if (se.gmm == TRUE) {
          gmm.se <- gmmse.gb2(gmm.coef, cprob, N)
        }
        if (se.scale == TRUE) {
          gmm.se[2] <- se.calc$sd.scale
        }
      }
    }
  }
  if (se.nls == FALSE & se.gmm == TRUE) {
    if (is.null(N)) {
      print("Unable to compute the standard errors. Please provide the sample size")
    }
    else {
      if (!is.na(temp.b)) {
        gmm.se <- gmmse.gb2(gmm.coef, cprob, N)
        if (se.scale == TRUE) {
          gmm.se[2] <- simsd.gb2(x = x, theta = nls.coef,
                                 N = N, nrep = nrep, se.scale = se.scale)$sd.scale
        }
      }
    }
  }
  nls.estimation[1, ] <- nls.coef
  nls.estimation[2, ] <- nls.se
  colnames(nls.estimation) <- c("a", "b", "p", "q")
  row.names(nls.estimation) <- c("Coef.", "se")
  gmm.estimation <- matrix(NA, 2, 4)
  gmm.estimation[1, ] <- gmm.coef
  gmm.estimation[2, ] <- gmm.se
  colnames(gmm.estimation) <- c("a", "b", "p", "q")
  row.names(gmm.estimation) <- c("Coef.", "se")
  grouped.data <- rbind(share, cprob)
  row.names(grouped.data) <- c("Income", "Population")
  if (gini == TRUE) {
    if (!is.na(gmm.rss)) {
      gmm.gini <- simgini.gb2(gmm.coef)
    }
    else {
      gmm.gini <- NA
    }
    nls.gini <- simgini.gb2(nls.coef)
    gini.estimation <- matrix(NA, 1, 3)
    colnames(gini.estimation) <- c("Survey", "NLS estimate",
                                   "GMM estimate")
    gini.estimation[1] <- gini.e
    gini.estimation[2] <- nls.gini
    gini.estimation[3] <- gmm.gini
    out2 <- list(grouped.data = grouped.data, distribution = "GB2",
                 nls.estimation = nls.estimation, nls.rss = nls.rss,
                 gmm.estimation = gmm.estimation, gmm.rss = gmm.rss,
                 gini.estimation = gini.estimation)
  }
  else {
    out2 <- list(grouped.data = grouped.data, distribution = "GB2",
                 nls.estimation = nls.estimation, nls.rss = nls.rss,
                 gmm.estimation = gmm.estimation, gmm.rss = gmm.rss)
  }
  return(out2)
}
