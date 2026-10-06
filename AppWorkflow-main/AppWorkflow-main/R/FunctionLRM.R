LRM_microbiome <- function(metadata, outcomes, predictors) {
  # check that all predictors are present in metadata
  tmp <- predictors[!predictors %in% colnames(metadata)]
  if (length(tmp) > 0) {
    return(paste("Error:", paste(tmp, collapse = ", "), "is/are not present in metadata."))
  }

  # check that all outcomes are present in metadata
  tmp <- outcomes[!outcomes %in% colnames(metadata)]
  if (length(tmp) > 0) {
    return(paste("Error:", paste(tmp, collapse = ", "), "is/are not present in metadata."))
  }

  # create a new dataframe containing only the predictors
  metadata_pr <- metadata[, predictors]
  # check variable class of predictors
  predictors_categoric <- colnames(metadata_pr)[sapply(metadata_pr, class) == "factor"]

  # Initialization of empty list that will be filled by the for cycle
  final_results <- list()

  # Start for cycle to compute
  for (model_outcome in outcomes) {
    print(paste("##### BEGIN: ", model_outcome, " ######"))
    # remove NAs
    metadata_model <- na.omit(metadata[, c(model_outcome, predictors)])

    # Definition of the map among the the variables and the dummy ones
    VarMap <- do.call(
      rbind,
      lapply(predictors, function(n) {
        df <- data.frame(key = n, value = n)
        if (is.factor(metadata_model[, n])) {
          levels(metadata_model[, n])[-1] -> lev
          dfLev <- data.frame(key = n, value = paste0(n, lev))
          df <- rbind(df, dfLev)
        }
        return(df)
      })
    )
    # Definition of variable set for linear regression model
    model <- as.formula(paste(model_outcome, "~ ", paste(unique(VarMap$key), collapse = " + ")))

    # Start lapply to compute models with or without intercept
    final_results[[model_outcome]] <- lapply(
      c(TRUE, FALSE),
      function(intercept) {
        # print(paste("##### BEGIN: intercept ",intercept, " ######"))
        # Regsubset function to exhaustively search through all possible subsets of predictors and fit a regression  model for each subset
        metadata_regsub <- regsubsets(model,
          data = metadata_model,
          really.big = F,
          nbest = 1,
          nvmax = NULL,
          intercept = intercept
        )
        # view summary of regression model
        lrm <- summary(metadata_regsub)

        # Function to extrapolate p-value from linear regression model
        lmp <- function(modelobject) {
          if (class(modelobject) != "lm") stop("Not an object of class 'lm' ")
          f <- summary(modelobject)$fstatistic
          p <- pf(f[1], f[2], f[3], lower.tail = F)
          attributes(p) <- NULL
          return(p)
        }

        # Automatic computation of all best models with increasing number of variables
        res <- lapply(1:dim(lrm$which)[1], function(TheBest) {
          names <- names(lrm$which[TheBest, ])[lrm$which[TheBest, ] == TRUE]

          VarsBest <- VarMap %>% filter(value %in% names)

          if (intercept) {
            form <- paste(model_outcome, "~ ", paste(c(unique(VarsBest$key)), collapse = " + "))
          } else {
            form <- paste(model_outcome, "~ 0 +", paste(c(unique(VarsBest$key)), collapse = " + "))
          }
          # print(form)

          best.model <- lm(form,
            data = metadata_model
          )
          list(
            form = form,
            names = names,
            VarsBest = unique(VarsBest$key),
            model = best.model,
            s = summary(best.model),
            c = confint(best.model),
            pvalue = lmp(best.model)
          )
        })

        # Creates a vector containing all models:
        # either min global p-value (if not significative) OR with best adj r squared (if significative)

        # sign_pvalue = sapply(1:length(res), function(i) res[[i]]$pvalue) < 0.05
        pvalues <- sapply(1:length(res), function(i) res[[i]]$pvalue)
        sign_pvalue <- pvalues < 0.05

        # for not signif models
        if (all(sign_pvalue == FALSE)) {
          min_pvalues <- which.min(pvalues)
          min_pvalues_summary <- res[[min_pvalues]]$s

          # Calcultaes vif
          if ((ncol(res[[min_pvalues]]$model$model) - 1) > 1) {
            vif <- car::vif(res[[min_pvalues]]$model)
          } else {
            vif <- 1
          }

          return(
            list(
              sign_model = sign_pvalue,
              best_model = min_pvalues_summary,
              vif = vif
            )
          )
        }

        # For significative models, select model with best adj. R squared
        bestAdjRsquared <- which.max(sapply(which(sign_pvalue), function(i) res[[i]]$s$adj.r.squared))
        bestAdjRsquared_summary <- res[[bestAdjRsquared]]$s

        # Calculates vif
        if ((ncol(res[[bestAdjRsquared]]$model$model) - 1) > 1) {
          vif <- car::vif(res[[bestAdjRsquared]]$model)
        } else {
          vif <- 1
        } # only one predictor variable -> the VIF is always equal to 1


        # Fills the list with objects for each outcome variable (Shannon, simpson, etc.)
        list(
          sign_model = sign_pvalue,
          best_model = bestAdjRsquared_summary,
          vif = vif
        )
      }
    )

    names(final_results[[model_outcome]]) <- c("Intercept", "NoIntercept")
    print(paste("##### END: ", model_outcome, " ######"))
  }

  # Returns the list with all objects for each outcome variable
  return(final_results)
}


## aggiungere help con spiegazione
