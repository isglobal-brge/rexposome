.correlation_circos_plot <- function(corr, desc, correlation.limits = list(
  list(d = '+', t = 0.5,  c = "#191970"),
  list(d = '+', t = 0.3,  c = "#4169E1"),
  list(d = '-', t = -0.3, c = "#DC143C"),
  list(d = '-', t = -0.5, c = "#8B0000")), show.legend = TRUE,
  cex.exposures = 0.50, cex.family = 0.55, ...){

  families <- as.character(desc$Family)
  col.f <- rainbow(length(unique(families)))
  names(col.f) <- sample(unique(families), length(unique(families)))
  col <- col.f[families]

  factors <- rownames(desc)
  factors <- factor(factors, levels = factors)

  if (show.legend) {
    nf <- layout(matrix(c(1,2,3,0), ncol = 2, nrow = 2, byrow = TRUE), c(3, 1), c(3, 1), TRUE)
  }

  par(mar = c(1,1,1,1))
  circlize::circos.initialize(factors = factors, xlim = c(0,1))
  circlize::circos.trackPlotRegion(factors = factors, ylim = c(0,1),
                                   bg.col = NA, bg.border = NA, track.height = 0.3)

  circlize::circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
    xlim = circlize::get.cell.meta.data("xlim")
    ylim = circlize::get.cell.meta.data("ylim")
    label = circlize::get.cell.meta.data("sector.index")
    circlize::circos.text(mean(xlim), ylim[1], label, facing = "clockwise",
                          adj = c(0,0.5), cex = cex.exposures)
  }, bg.border = NA)

  circlize::circos.trackPlotRegion(track.index = 2, factors = factors,
                                   ylim = c(0,1), bg.col = col, bg.border = NA, track.height = 0.05)

  leg.txt <- sapply(correlation.limits, function(lim) {
    s <- ifelse(lim$d == '+', '>', '<')
    for(ii in 1:(nrow(corr)-1)) {
      for(jj in (ii+1):nrow(corr)) {
        if (!is.na(corr[ii, jj])) {
            if (lim$d == "+" & corr[ii, jj] > lim$t) {
              circlize::circos.link(
                colnames(corr)[ii], 0.5, colnames(corr)[jj], 0.5, col = lim$c
              )
            } else if (lim$d == "-" & corr[ii, jj] < lim$t ) {
              circlize::circos.link(
                colnames(corr)[ii], 0.5, colnames(corr)[jj], 0.5, col = lim$c
              )
            }
        }
      }
    }
    c(paste("rho", s, lim$t, sep=" ", collapse=" "))
  })
  circlize::circos.clear()

  if (show.legend) {
    plot.new()
    par(mar=c(1,1,1,1))
    legend("center", legend = names(col.f), pch=16, title="Families Exp.",
           col=col.f, bty = "n", inset=c(-0.2,0), cex =  cex.family)
    plot(1, type="n", axes=FALSE, xlab="", ylab="")
    legend("center", legend = leg.txt, pch=16, title="Correlation", cex = cex.family,
           col=sapply(correlation.limits, "[[", "c"), bty = "n", horiz = TRUE)
  }
}
