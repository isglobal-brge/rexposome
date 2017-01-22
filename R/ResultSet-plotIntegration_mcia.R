.plot_integration_mcia <- function(object, cmpX, cmpY, ...) {
    if(object@results[[1]]$result$coa[[1]]$nf < 2) {
        stop("Input ResultSet was created for less than 2 components.")
    }

    if(sum(c(cmpX, cmpY) <= object@results[[1]]$result$coa[[1]]$nf) != 2) {
        stop("Given component X or component Y (cmpX, cmpY) higher than number of axis in ResultSet.")
    }

    require(omicade4)
    plot(object@results[[1]]$result, axes = c(cmpX, cmpY), ...)
}

#
#
# plot_mca <- function(mcoin, axes=1:2, sample.lab=TRUE, sample.legend=TRUE,
#                      sample.color=1, phenovec=NULL, df.color=1, df.pch=NA,
#                      gene.nlab=0, ...) {
#     if (!inherits(mcoin, "mcia"))
#         stop("mcia object expected, please run mcia first")
#
#     ndata <- length(mcoin$coa)
#     eig <- mcoin$mcoa$pseudoeig
#     cov2 <- mcoin$mcoa$cov2[, axes] #pseueig
#
#
#
#     if (!length(sample.color) %in% c(1, nrow(mcoin$mcoa$SynVar)))
#         stop("length of sample.color should be either 1 or # of samples")
#     if (!length(df.color) %in% c(1, ndata))
#         stop("length of df.color should be either 1 or # of samples")
#
#     if (is.na(df.pch[1])) {
#         pch <- c(16, 17, 15, 18, 1, 2, 0, 5)
#         if (ndata > 8) {
#             pch <- rep(pch, ceiling(ndata/8))[1:ndata]
#             warning("more than 8 datasets in mcia, df.pch is recycled used")
#         } else {
#             pch <- pch[1:ndata]
#         }
#     } else {
#         if (length(df.pch) != ndata)
#             stop("the length of df.pch should be the same with datasets in mcia, recycled use df.pch is not allowed")
#         pch <- df.pch
#     }
#
#     layout(matrix(c(1,2,3,4), 2, 2, byrow = TRUE))
#     omicade4:::splot.sample.mcia(mcoin, axis1=axes[1], axis2=axes[2],
#                       col=sample.color, pch=pch,
#                       sample.lab=sample.lab,
#                       legend=sample.legend,
#                       sub="sample space",
#                       phenovec=phenovec)
#
#     omicade4:::splot.mol.mcia(mcoin, axis1=axes[1], axis2=axes[2],
#                               col=df.color, pch=pch, gene.nlab=gene.nlab)
#
#     #plot c(2, 1)
#     #   par(mar=c(3, 3, 3, 3))
#     #   barplot(eig, col="black",names.arg=paste("eig", 1:length(eig)))
#     #   legend(x="topright", pch=pch, col=df.color, legend=names(mcoin$coa), box.col="white")
#
#     nkeig <- ncol(mcoin$mcoa$Tli)
#     eig <- mcoin$mcoa$pseudoeig
#     proe <- eig/sum(eig)
#
#     par(mar=c(3, 4, 1, 4))
#     neig <- length(eig)
#     po <- barplot(eig, plot=FALSE)
#     barplot(eig,names.arg= 1:neig, col=c(rep("cyan", nkeig), rep("gray", neig-nkeig)), xlab="", ylab="Eigen Value")
#
#     par(new=T)
#     plot(cbind(po, proe), frame.plot=FALSE, pch=20, axes=FALSE, xlab="Eigen Vector", ylab="")
#     points(x=po, y=proe, pch=20)
#     lines(x=po, y=proe)
#     axis(side=4)
#     mtext(side=4, text="Percentage", line=2.5, cex=0.8)
#     legend(x="topright", pch=pch, col=df.color, legend=names(mcoin$coa), box.col="white")
#
#     #plot c(2, 2)
#     par(mar=c(4.5, 4.5, 0.5, 0.5))
#     plot(cov2, pch=".", main="", axes=TRUE, col=NA,
#          xlab=paste("pseudoeig", axes[1]), ylab=paste("pseudoeig", axes[2]))
#     ade4::scatterutil.grid(0)
#     points(cov2, pch=pch, col=df.color)
# }
#
#
# splot.sample.mcia <-
#     function(x, axis1=1, axis2=2,
#              col=1, pch=20,
#              sample.lab=TRUE,
#              legend=TRUE,
#              sub="",
#              phenovec=NULL) {
#
#         # plot matched samples from mcia
#         if (!inherits(x, "mcia"))
#             stop("x should be an object of class mcia!")
#         if (!is.null(phenovec))
#             phenovec <- as.factor(phenovec)
#         ndata <- length(x$coa)
#         dfxy <- x$mcoa$Tl1
#         syn <- x$mcoa$SynVar
#         pl <- pch
#         cl <- col
#
#         if (!is.null(phenovec) && length(phenovec) != nrow(syn))
#             stop("the length of phenovec should be the same with # of samples")
#         if (!axis1 %in% 1:ncol(dfxy))
#             stop("Uncharacterized axis selected by axis1")
#         if (!axis2 %in% 1:ncol(dfxy))
#             stop("Uncharacterized axis selected by axis1")
#         if (!length(col) %in% c(1, nrow(syn), length(levels(phenovec))))
#             stop("length of col should be either 1 or # of samples")
#         if (!length(pch) %in% c(1, ndata))
#             stop("length of pch should be either 1 or # of data frame")
#
#         sync <- c()
#         for (i in 1:(ndata)) {
#             sync <- rbind(sync, syn)
#         }
#         c <- x$mcoa$TL$"T"
#
#         if (length(col) == 1) {
#             if (is.null(phenovec))
#                 col <- rep(col, length(c)) else
#                     col <- c(phenovec)
#         } else if (length(col) == length(levels(phenovec))) {
#             if (!is.null(phenovec))
#                 col <- col[c(phenovec)]
#         } else
#             col <- rep(col, ndata)
#
#         if (length(pch) == 1)
#             pch <- rep(pch, length(c)) else
#                 pch <- rep(pch, table(c))
#
#         par(mar = c(0.1, 0.1, 0.1, 0.1))
#         coo <- scatterutil.base(dfxy = dfxy, xax = axis1, yax = axis2, sub = sub,
#                                 xlim = NULL, ylim = NULL, grid = TRUE,
#                                 addaxes = TRUE, cgrid = 1, include.origin = TRUE,
#                                 origin = c(0, 0), csub = 1.25, possub = "bottomleft",
#                                 pixmap = NULL, contour = NULL, area = NULL, add.plot = FALSE)
#
#         points(dfxy[, c(axis1, axis2)], pch=pch, col=col)
#         segments(sync[, axis1], sync[, axis2], dfxy[, axis1], dfxy[, axis2], col=col)
#
#         if (sample.lab) {
#             lab <- rownames(syn)
#             text(x=syn[, axis1], y=syn[, axis2], lab)
#         }
#
#         if (legend && any(c(length(cl) != 1, length(pl) != 1, !is.null(phenovec)))) {
#
#             ple <- c()
#             if (length(pl) != 1) {
#                 pl <- pl
#                 ple <- names(x$coa)
#             }
#
#             cle <- c()
#             if (length(cl) == nrow(syn)) {
#                 cl <- cl
#                 cle <- rownames(syn)
#             } else if (length(cl) == 1 && !is.null(phenovec)) {
#                 cl <- sort(unique(c(phenovec)))
#                 cle <- levels(phenovec)
#             } else if (length(cl) == length(levels(phenovec))) {
#                 cl <- cl
#                 cle <- levels(phenovec)
#             }
#
#             pch.i <- c(pl, rep(20, length(cl)))
#             if (length(pl)==1)
#                 col.i <- cl else
#                     col.i <- c(rep(1, ndata), cl)
#             le.i <- c(ple, cle)
#
#             legend("topleft", fill=FALSE, col=col.i, pch=pch.i, legend=le.i, border=F, bty="n")
#         }
#         box()
#     }
