# rexposome

master [![Travis-CI Build Status](https://travis-ci.org/carleshf/rexposome.svg?branch=master)](https://travis-ci.org/carleshf/rexposome) 
devel [![Travis-CI Build Status](https://travis-ci.org/carleshf/rexposome.svg?branch=devel)](https://travis-ci.org/carleshf/rexposome)

## Summary

`rexposome` is an R package for exposome characterization and exposome-omic data analysis. It depends in a series of third party R packages to provide:

  1. A basic pipeline for missing-data imputation in exposome, include the imputation of values under limit of detection.
  2. A series of functions to describe and characterize the exposome, including PCA in exposures and samples space, correlation between exposures and clustering of samples through exposure levels.
  3. Two different approaches to test the association exposome-diseasom in terms of Exposome-Wide Association Studies (ExWAS and mExWAS).
  4. A series of pipelines to test exposome-omic and diseasome-omic associations.
  5. [UNDER DEVELOPMENT] Two different approaches to integrate exposome with omic data.

## Installation

`rexposome` requires R version equal or newer than 3.3.0. The following script allows to install `rexposome` dependencies:

```r
source( "http://bioconductor.org/biocLite.R" )

packages = c('Biobase', 'mice', 'MultiDataSet', 'mvtnorm', 'lsr', 'BiocInstaller', 
    'FactoMineR', 'gridExtra', 'stringr', 'pryr', 'circlize', 'corrplot', 'ggplot2', 
    'reshape2', 'scales', 'pryr', 'mboost', 'imputeLCMD', 'snpStats', 'MEAL', 
    'limma', 'scatterplot3d', 'glmnet', 'omicade4', 'sva', 'ggrepel', 'PMA'
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

Once `devtools` and the dependences are installed, the following code installs `rexposome`:

```r
devtools::install_github("isglobal-brge/rexposome")
```

### Details

The installation of `rexposome` through `devtools` takes care of installing all the dependencies. The following bullets informs about the different dependencies of `rexposome`:

  * R version >= 3.3.0
  * R packages dependencies: `Biobase`, `mice`, `MultiDataSet`, `mvtnorm`, `lsr`
  * R packages imports: `Biobase`, `mice`, `MultiDataSet`, `mvtnorm`, `lsr`, `BiocInstaller`, 
    `FactoMineR`, `gridExtra`, `stringr`, `pryr`, `circlize`, `corrplot`, `ggplot2`, 
    `reshape2`, `scales`, `pryr`, `mboost`, `imputeLCMD`, `snpStats`, `MEAL`, 
    `limma`, `scatterplot3d`, `glmnet`, `omicade4`, `sva`, `ggrepel`, `PMA`
  * R packages suggestions: `flexmix`, `thestthat`

## Basic Guide

### Loading Exposome

In `rexposome` the _exposome_ is understood as a set of three files:

  1. The exposure file: A matrix files with the exposures' measurements, having the individuals as rows and the exposures as columns.
  2. The phenotype file (diseasome file): A matrix with the phenotypes or diseases description, having the individuals as rows and the phenotypes as columns.
  3. The description file: A table describing the exposures. It must have, at last, two columns: one with the exposures and one with the family/group of exposures. The order of the exposures must be the same as in exposure-file.

The _exposome_ is loaded from files using the function `read_exposome`.

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

### Exposome-Omic Association

  * Function `assocSNP` allows to perform a basic GWAS.
  * Function `assocGE` allows to perform a DE analysis in base of the exposures.
  * Function `assocME` allows to perform an EWAS for each exposure.
  * Function `assocPRT` allows to test the association of the protein quantification with the exposures levels.
  * `plotAssociation` allows to plot the result of all _assoc*_ functions.

### Exposome-Omic Integration

  * Function `crossomics` allows to perform a multi-omic integration join exposome by selecting one of the available methods (`"mcia"` or `"mcca"`).
