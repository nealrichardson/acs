setMethod("plot", signature(x = "acs"), function(x, conf.level = 0.95, err.col = "red",
    err.lwd = 1, err.pch = "-", err.cex = 2, err.lty = 2, x.res = 300, labels = "auto",
    by = "geography", true.min = T, ...) {
    # internal plot function to plot individual x-y plots with conf intervals, assume
    # that either i or j or length 1
    plot.xy.acs <- function(x, object, conf.int.upper, conf.int.lower, estimates,
        labels, xlab, ylab, ...) {
        if (missing(xlab))
            xlab <- ""
        if (missing(ylab))
            ylab <- ""
        plot(rep(x, 2), c(conf.int.upper, conf.int.lower), type = "n", xaxt = "n",
            xlab = xlab, ylab = ylab, ...)
        axis(side = 1, labels = labels, at = x, ...)
        lines(x = matrix(c(x, x, rep(NA, length(x))), ncol = length(x), byrow = T),
            matrix(c(conf.int.lower, conf.int.upper, rep(NA, length(x))), ncol = length(x),
                byrow = T), col = err.col, lwd = err.lwd, lty = err.lty)
        points(x, conf.int.upper, pch = err.pch, cex = err.cex, col = err.col)
        points(x, conf.int.lower, pch = err.pch, cex = err.cex, col = err.col)
        points(x, estimates, ...)
    }
    acs.density.plot <- function(x, type = "l", xlim, xlab = acs.colnames(x), ylab = "Density Distribution",
        conf.level, col = "black", err.col, err.lwd, err.lty, x.res, ...) {
        est <- estimate(x)
        err <- standard.error(x)
        if (missing(xlim))
            xlim <- c(est - (4 * err), est + (4 * err))
        x.vals <- seq(from = xlim[1], to = xlim[2], by = (xlim[2] - xlim[1])/x.res)
        plot(x.vals, dnorm(x.vals, mean = est, sd = err), type = type, xlab = xlab,
            ylab = ylab, col = col, ...)
        if (conf.level) {
            abline(v = qnorm(mean = est, sd = err, p = c(((1 - conf.level)/2), (1 -
                ((1 - conf.level)/2)))), col = err.col, lwd = err.lwd, lty = err.lty)
        }
    }
    i <- dim(x)[1]
    j <- dim(x)[2]
    if (length(x) == 1) {
        acs.density.plot(x, conf.level = conf.level, err.col = err.col, err.lwd = err.lwd,
            err.lty = err.lty, x.res = x.res, ...)
    } else if (i == 1 | j == 1) {
        con <- confint(x, level = conf.level)
        conf.int.upper <- NA
        conf.int.lower <- NA
        estimates <- NA
        if (i == 1) {
            # one row
            if (identical(labels, "auto"))
                labels <- acs.colnames(x)
            for (k in 1:j) {
                conf.int.upper[k] <- as.numeric(con[[k]][2])
                if (true.min == T) {
                  conf.int.lower[k] <- as.numeric(con[[k]][1])
                } else {
                  if (true.min == F) {
                    true.min <- 0
                  }
                  conf.int.lower[k] <- max(true.min, as.numeric(con[[k]][1]))
                }
                estimates[k] <- estimate(x)[1, k]
            }
        } else {
            if (identical(labels, "auto"))
                labels <- geography(x)[[1]]
            for (k in 1:i) {
                # one column
                conf.int.upper[k] <- as.numeric(con[[1]][k, 2])
                if (true.min == T) {
                  conf.int.lower[k] <- con[[1]][k, 1]
                } else {
                  if (true.min == F) {
                    true.min <- 0
                  }
                  conf.int.lower[k] <- max(true.min, con[[1]][k, 1])
                }
                estimates[k] <- estimate(x)[k, 1]
            }
        }
        plot.xy.acs(x = 1:max(i, j), object = x, conf.int.upper = conf.int.upper,
            conf.int.lower = conf.int.lower, estimates = estimates, labels = labels,
            ...)
    } else {
        if (by == "geography") {
            par(mfrow = c(i, 1))
            for (k in 1:i) {
                plot(x[k, ], sub = geography(x)[k, 1], conf.level = conf.level, err.col = err.col,
                  err.lwd = err.lwd, err.pch = err.pch, err.cex = err.cex, err.lty = err.lty,
                  labels = labels, ...)
            }
        } else if (by == "acs.colnames") {
            par(mfrow = c(1, j))
            for (k in 1:j) {
                plot(x[, k], sub = acs.colnames(x)[k], conf.level = conf.level, err.col = err.col,
                  err.lwd = err.lwd, err.pch = err.pch, err.cex = err.cex, err.lty = err.lty,
                  labels = labels, ...)
            }
        }
    }
})
