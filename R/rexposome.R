#' rexposome: Package for exposome exploration and outcome data analysis
#'
#' #' @section exposures loading and exposures managment:
#' \code{rexposome} offers two methods to bring exposome data to R and
#' Bioconductor. \code{\link{read_exposome}} allows to read three txt-like
#' files (\code{.csv}, \code{.tsv}, ...) while \code{\link{load_exposome}}
#' is sued with \code{matrix} and \code{data.frame}s. The class obtained is
#' an \code{\link{ExposomeSet}}, a class based in \code{eSet} for exposome
#' data managment.
#'
#' @section exposures processing:
#' The packages offers a wide set of functions to preprocess exposome data.
#' Method \code{\link{trans}} allow to transforms the exposures, method
#' \code{\link{normalityTest}} allows to check for normality in exposome,
#' \code{\link{standardize}} allows to standardize the exposures, among others.
#' Finally, \code{\link{impute}} and \code{\link{ilod}} allow tu use \code{mice},
#' \code{Hmisc} and \code{imputeLCMD} for exposure missing data and exposure
#' under-lod data imputation.
#'
#' @section exposures analyses:
#' the two methods \code{\link{exwas}} and \code{\link{mexwas}} allows to test
#' the association between exposures and health outcomes (phenotpe data).
#'
#' @section exposures plotting:
#' The methods \code{\link{plotFamily}} allows to see how the exposures
#' behaves within families. \code{\link{plotCorrelation}} helps to understand
#' how exposures are related between themselves. \code{\link{plotClassification}}
#' allos to visually detect cuslters of samples that share the same pattern
#' of levels of exposures.
#'
#' @docType package
#' @name rexposome
#'
#' @import Biobase
#' @import utils
#'
#' @importClassesFrom Biobase eSet
#' @importClassesFrom MultiDataSet MultiDataSet
#'
#' @importFrom lsr cramersV
#' @importFrom FactoMineR PCA
#' @importFrom stringr str_pad
#' @importFrom circlize circos.initialize circos.trackPlotRegion get.cell.meta.data circos.text circos.link circos.clear
#' @importFrom corrplot corrplot
#' @importFrom ggplot2 ggplot aes aes_string
#' @importFrom ggplot2 geom_bar geom_tile geom_point geom_errorbar geom_errorbarh geom_abline geom_histogram
#' @importFrom ggplot2 geom_text geom_density geom_hline geom_vline geom_boxplot
#' @importFrom ggplot2 theme theme_bw theme_minimal labs
#' @importFrom ggplot2 xlab xlim ylab ylim ggtitle element_text element_line element_blank
#' @importFrom ggplot2 coord_flip coord_polar facet_wrap
#' @importFrom ggplot2 scale_fill_manual scale_fill_continuous scale_fill_discrete scale_fill_gradient2
#' @importFrom ggplot2 scale_y_continuous scale_fill_brewer
#' @importFrom reshape2 melt
#' @importFrom pryr named_dots
#' @importFrom mboost gamboost extract
#' @importFrom imputeLCMD impute.QRILC impute.MinProb
#' @importFrom scatterplot3d scatterplot3d
#' @importFrom glmnet cv.glmnet glmnet
#' @importFrom grid viewport grid.newpage pushViewport grid.layout
#' @importFrom gridExtra grid.arrange
#' @importFrom mice mice complete
#' @importFrom Hmisc impute
#' @importFrom gplots heatmap.2
#' @importFrom gtools quantcut
#' @importFrom scales percent_format
NULL

