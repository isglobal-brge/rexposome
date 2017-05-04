# rexposome

## Summary

`rexposome` is an R package for exposome characterization and exopsome-outcome test association. It depends in a series of third party R packages to provide:

  1. A basic pipeline for missing-data imputation in exposome, include the imputation of values under limit of detection.
  2. A series of functions to describe and characterize the exposome, including PCA in exposures and samples space, correlation between exposures and clustering of samples through exposure levels.
  3. Two different approaches to test the association exposome-diseasom in terms of Exposome-Wide Association Studies (ExWAS and mExWAS).
  
## Installation

`rexposome` requires R version equal or newer than 3.0. The following script allows to install `rexposome` dependencies:

```r
source( "http://bioconductor.org/biocLite.R" )

packages = c('Biobase', 'mice', 'MultiDataSet', 'lsr', 'FactoMineR',
  'stringr', 'circlize', 'corrplot', 'ggplot2', 'reshape2', 'pryr',
  'mboost', 'imputeLCMD', 'scatterplot3d', 'glmnet', 'gridExtra',
  'grid', 'Hmisc', 'gplots', 'gtools', 'scales'
)
for( pkg in packages ) {
  if( !pkg %in% rownames( installed.packages() ) ) {
    message( "Installing ", pkg )
    biocLite( pkg )
  }
}
```

The package can be installed using the R package `devtools`. `devtools` can be installed win the following code:

```r
install.packages("devtools")
```


Due to the publication process of `rexposome` at Biocondcuto, the requirment is set to R-3.4. For those using R-3.4, the following code installs `rexposome`:

```r
devtools::install_github("isglobal-brge/rexposome")
```

For those using an older version of R (but newer than R-3.o) the following code installs `rexposome`:

```r
devtools::install_github("isglobal-brge/rexposome", ref="R-3.0")
```
### Details

### Authors

  * Carles Hernandez-Ferrer | `carles.hernandez < at > isglobal < dot > org`
  * Juan R. Gonzalez | `juanr.gonzalez < at > isglobal < dot > org`

### Loading Exposome

In `rexposome` the _exposome_ is understood as a set of three files:

  1. The exposure file: A matrix files with the exposures' measurements, having the individuals as rows and the exposures as columns.
  2. The phenotype file (diseasome file): A matrix with the phenotypes or diseases description, having the individuals as rows and the phenotypes as columns.
  3. The description file: A table describing the exposures. It must have, at last, two columns: one with the exposures and one with the family/group of exposures. The order of the exposures must be the same as in exposure-file.

The _exposome_ is loaded from files using the function `read_exposome`. If the information is stored in standard R `data.frame`s, those can be converted to an `ExposomeSet` using the function `load_exposome`.

### Exposome Characterization

The characterization of the exposome is done with a bunch of functions that follows:

  * The functions `tableMissings` and `plotMissings` allows to quantify the amount of missing data.
  * The functions `tableLOD` and `plotLOD` allows to quantify the amount of measurements under limit of detection (if provided in description-file).
  * `normalityTest` allows to test normality on the exposures.
  * The two function `impute` and `ilod` allows to impute missing data and values under LOD.
  * `plotFamily` allows to draw a cumulative bar plot for categorical exposures and a box-plot for continuous exposures.
  * `correlation` allows to compute the correlation between exposures.
  * The function `plotCorrelation` allows to draw a circos or a matrix plot for the computed correlations.
  * `pca` performs a Principal Component Analysis in the exposome.
  * `plotPCA` and `plot3PCA` allows to draw a 2D and 3D scatter plot for the result of the PCA.
  * Functions `plotPHE` and `plotEXP` allows to draw the association and the correlation between phenotypes and exposures to the principal components.

### Exposome-Phenotype Association

  * The function `exwas` allows to perform an Exposome-Wide Association Study by testing the association of each exposure with a given phenotype.
  * `m_exwas` allows to perform a MultiVariate Exposome-Wide Association Study by using DSA or ElasticNet methods.
  * `plotExwas` allows to plot a Manhattan plot of the result of an ExWAS (for both `exwas` and `m_exwas`).
  * `plotEffect` allows to plot the effects of each exposure. It can also be used to compare two models.