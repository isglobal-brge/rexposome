.correlation_matrix_plot <- function(corr, desc, cex.exposures = 0.50, cex.family = 0.55, ...) {
  # plot - matrix of correlation
  corrplot::corrplot(as.matrix(corr)[rownames(desc), rownames(desc)],
                     method = "color",
                     mar = c(0.5,2,5,0),
                     tl.col = "black",
                     tl.cex = 0.55,
                     tl.pos = "n"
  )
  # /

  # y axis labels (exposures)
  desc$Exposure <- rownames(desc)
  par(xpd = TRUE)
  for(ii in 1:nrow(desc)) {
    text(y = ii, x = 0, labels = desc$Exposure[nrow(desc) - ii + 1], adj = 1,  cex = cex.exposures)
  }
  rm(ii)
  par(xpd = FALSE)
  # /

  # x axis lables (families)
  row.line <- nrow(corr) + 1
  row.fam  <- nrow(corr) + 2.5
  cnt <- sapply(unique(desc$Family), function(ff) sum(desc$Family == ff))
  names(cnt) <- unique(desc$Family)
  cnt2 <- cnt
  for(ii in 2:length(cnt)) {
    cnt2[ii] <- cnt2[ii] + cnt2[ii - 1]
  }
  cnt <- rbind(c(0,0), data.frame(cnt, cnt2))
  rm(cnt2, ii)

  par(xpd = TRUE)
  for(rr in 2:nrow(cnt)) {
    x = cnt[(rr-1):rr, "cnt2"]
    x[1] <- x[1] + 0.75
    x[2] <- x[2] + 0.25
    lines(x = x, y = c(row.line, row.line))
    if(x[2] - x[1] == 1 ) {
      x = x[2] - 1
    } else {
      x = floor(x[1] + (x[2] - x[1] - 1) / 2)
    }
    text(x = x, y = row.fam, cex = cex.family, labels = rownames(cnt)[rr], pos = 4, srt = 90)
  }
  par(xpd = FALSE)
  # /
}
