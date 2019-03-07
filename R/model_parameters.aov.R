#' ANOVAs Parameters
#'
#' Parameters of ANOVAs.
#'
#' @param model Object of class \link{aov}, \link{anova} or \code{aovlist}.
#' @param omega_squared Compute omega squared as indices of effect size. Can be \code{NULL}, "partial" (default) or "raw" for non-partial indices.
#' @param ... Arguments passed to or from other methods.
#'
#' @examples
#' \dontrun{
#' df <- iris
#' df$Sepal.Big <- ifelse(df$Sepal.Width >= 3, "Yes", "No")
#'
#' model <- aov(Sepal.Length ~ Sepal.Big, data=df)
#' model_parameters(model)
#'
#' model <- anova(lm(Sepal.Length ~ Sepal.Big, data=df))
#' model_parameters(model)
#'
#' model <-aov(Sepal.Length ~ Sepal.Big + Error(Species), data=df)
#' model_parameters(model)
#'
#' model <- anova(lme4::lmer(Sepal.Length ~ Sepal.Big + (1|Species), data=df))
#' model_parameters(model)
#'
#' }
#' @export
model_parameters.aov <- function(model, omega_squared = "partial", ...){
  parameters <- .extract_parameters_anova(model)

  if(!is.null(omega_squared)){
    if(omega_squared == "partial"){
      parameters$Omega_Squared <- omega_squared(model, partial=TRUE)
    } else{
      parameters$Omega_Squared <- omega_squared(model, partial=FALSE)
    }
  }
  return(parameters)
}



#' @rdname model_parameters.aov
#' @export
model_parameters.anova <- model_parameters.aov

#' @rdname model_parameters.aov
#' @export
model_parameters.aovlist <- model_parameters.aov












#' @keywords internal
.extract_parameters_anova <- function(model){

  # Processing
  if("aov" %in% class(model)){
    parameters <- as.data.frame(summary(model)[[1]])
    parameters$Parameter <- trimws(row.names(parameters))
  } else if("anova" %in% class(model)){
    parameters <- as.data.frame(model)
    parameters$Parameter <- trimws(row.names(parameters))
  } else if("aovlist" %in% class(model)){
    if (names(model)[1L] == "(Intercept)") {
      model <- model[-1L]
    }
    parameters <- data.frame()
    rowmax <- 0
    for(i in names(model)){
      temp <- as.data.frame(summary(model[[i]])[[1]])
      temp$Parameter <- trimws(row.names(temp))
      temp$Group <- i
      row.names(temp) <- 1:nrow(temp) + rowmax
      rowmax <- nrow(temp)
      if(nrow(parameters) == 0){
        parameters <- temp
      } else{
        parameters <- merge(parameters, temp, all=TRUE)
      }
    }
    parameters <- parameters[order(parameters$Group),]
  }

  # Rename
  names(parameters) <- gsub("Pr(>F)", "p", names(parameters), fixed=TRUE)
  names(parameters) <- gsub("Df", "DoF", names(parameters), fixed=TRUE)
  names(parameters) <- gsub("Chi.Df", "DoF", names(parameters), fixed=TRUE)
  names(parameters) <- gsub("Sum Sq", "Sum_Squares", names(parameters), fixed=TRUE)
  names(parameters) <- gsub("Mean Sq", "Mean_Square", names(parameters), fixed=TRUE)
  names(parameters) <- gsub("F value", "F", names(parameters), fixed=TRUE)
  names(parameters) <- gsub("Res.Df", "DoF_residual", names(parameters), fixed=TRUE)
  names(parameters) <- gsub("Sum of Sq", "Sum_Squares", names(parameters), fixed=TRUE)
  names(parameters) <- gsub("Chisq", "ChiSquare", names(parameters), fixed=TRUE)
  names(parameters) <- gsub("P(>|Chi|)", "p", names(parameters), fixed=TRUE)
  names(parameters) <- gsub("Pr(>Chi)", "p", names(parameters), fixed=TRUE)
  names(parameters) <- gsub("Pr..Chisq.", "p", names(parameters), fixed=TRUE)
  names(parameters) <- gsub("Pr..Chi.", "p", names(parameters), fixed=TRUE)
  names(parameters) <- gsub("Chi.sq", "ChiSquare", names(parameters), fixed=TRUE)
  names(parameters) <- gsub("LR.Chisq", "ChiSquare", names(parameters), fixed=TRUE)
  names(parameters) <- gsub("LR Chisq", "ChiSquare", names(parameters), fixed=TRUE)
  names(parameters) <- gsub("p.value", "p", names(parameters), fixed=TRUE)

  # Reorder
  row.names(parameters) <- NULL
  order <- c("Group", "Parameter", "Sum_Squares", "DoF", "Mean_Square", "F", "p")
  parameters <- parameters[order[order %in% names(parameters)]]

  return(parameters)
}



