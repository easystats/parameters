#' @keywords internal
.parameters_types <- function(name, data) {
  if (grepl(":", name)) {
    var <- utils::tail(unlist(strsplit(name, ":", fixed = TRUE)), 1)
    return(c("interaction", var))
  } else if (name == "(Intercept)") {
    return(c("intercept"))
  } else if (name %in% names(data)) {
    return(c("numeric", name))
  } else {
    facs <- data[sapply(data, is.factor)]
    facs_names <- c()
    for (fac in names(facs)) {
      facs_names <- c(facs_names, paste0(fac, unique(data[[fac]])))
      if (name %in% facs_names) {
        return(c("factor", fac))
      }
    }

    return("unknown")
  }
}

#' @keywords internal
.parameters_types_table <- function(param_names, param_values, data) {

  params_table <- data.frame(
    Parameter = param_names,
    Value = param_values,
    stringsAsFactors = FALSE
  )
  types <- lapply(params_table$Parameter, .parameters_types, data)

  type_list <- c()
  subparam_list <- c()
  for(i in types){
    if(length(i) == 1){
      type_list <- c(type_list, i[[1]])
      subparam_list <- c(subparam_list, NA)
    } else{
      type_list <- c(type_list, i[[1]])
      subparam_list <- c(subparam_list, i[[2]])
    }
  }
  params_table$Type <- type_list
  params_table$Variable <- subparam_list
  params_table
}
