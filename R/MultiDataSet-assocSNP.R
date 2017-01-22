setMethod(
  f = "assocSNP",
  signature = "MultiDataSet",
  definition = function(object, design,  family = "binomial",
                        filter.callrate = 0.98, filter.maf = 0.05,
                        filter.pvalHWE = 0.001, ..., verbose = TRUE,
                        warnings = TRUE) {

      tsnp <- grep("snps", names(object))
      texp <- grep("exposures", names(object))
      tsnp <- names(object)[tsnp]
      texp <- names(object)[texp]

      if(length(tsnp) != 1) {
          stop("One of the tables must exists in 'MultiDataSet' as ",
               "'snps'")
      }
      if(length(texp) != 1) {
          stop("One of the tables must exists in 'MultiData'Set' as ",
               "'exposures'")
      }

      if(verbose) {
          message("The following tables will be used in the association ",
                  "process: ",paste0("'", paste(c(tsnp, texp), collapse="', '"), "'"))
      }

      if(warnings | verbose) {
          warning("Sets from 'MultiDataSet' will be reduced to common samples")
      }

      l1 <- sapply(sampleNames(object)[c(tsnp, texp)], length)
      object <- MultiDataSet:::commonSamples(object)
      l2 <- sapply(sampleNames(object)[c(tsnp, texp)], length)
      l3 <- mapply('-', l1, l2, SIMPLIFY = FALSE)

      if(verbose) {
          message(paste(unlist(l3), names(l3),
                        sep = " samples were reduced from ", collapse = ", "))
      }

      # ---------------------------------------------------------------------

      # Create phenotype from design
      design <- as.formula(design)
      select <- all.vars(design)
      exp.dt <- as.data.frame(object[[texp]])
      pheno <- rexposome:::.create_p(
          expo.dt = exp.dt,
          omic.p = pData(object[[tsnp]]),
          select = select
      )

      rownames(pheno) <- rownames(pData(object[[tsnp]]))
      if(sum(is.na(pheno)) != 0 & (warnings | verbose)) {
          warning("Given design ('", design, "') has ", sum(is.na(pheno)), " NA values")
      }
      if(class(pheno) == "character") {
          stop("Invalid value '", pheno, "' in 'design'.")
      }
      # -----------------------------------------------------------------

      # 3. ASSOCIATION
      # -----------------------------------------------------------------------
      ## No necessary to specify data (ind.info) since we are givint to the ---
      ## the full data as "phenotype" and as "snp.data" so the variable in ----
      ## "phenotype" will not be seek in "data". -------------------------------

      geno <- t(exprs(object[[tsnp]]))
      if(sum(is.na(geno)) != 0 & (warnings | verbose)) {
          warning("Given SnpSet contains NA values")
      }

      tests <- as(snpStats::snp.rhs.tests(design, family = family,
          data = pheno, snp.data = geno, ...), "data.frame")

      # 4. FILTER SNPs
      # --------------------------------------------------------------------------
      infoSnp <- snpStats::col.summary(geno)
      infoSnp$pvalHWE <- 1 - pnorm(infoSnp$z.HWE)
      useSnp  <- !is.na(infoSnp$MAF) & infoSnp$MAF > filter.maf & infoSnp$pvalHWE > filter.pvalHWE & infoSnp$Call.rate > filter.callrate
      tests <- tests[useSnp, ]


      # 5. GENRATE TABLE
      # --------------------------------------------------------------------------

      #snp.pval  <- snpStats::p.value(tests2, df = 1)
      ord   <- order(tests$p.value)
      snp.names <- rownames(tests)[ord]
      snp.pval <- tests$p.value[ord]

      tt <- data.frame(
          Name    = snp.names,
          PVal    = snp.pval,
          MAF     = infoSnp[snp.names, "MAF"],
          PValHWE = infoSnp[snp.names, "pvalHWE"]
      )

      rr <- list(list("result" = tt, "test" = tests))
    new("ResultSet",
        fun_origin = "assocSNP",
        class_origin = c("ExposomeSet", "SnpSet"),
        names = c(texp, tsnp),
        results = rr,
        fData = fData(object)[c(texp, tsnp)],
        sva=0
    )
})
