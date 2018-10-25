######################################################################
########################## Class nacho_set ###########################
############################## Creation ##############################
######################################################################


### Class definition ###
#' nacho_set
#'
#' @slot access [character]
#' @slot housekeeping_genes [character]
#' @slot normalisation_method [character]
#' @slot remove_outliers [logical]
#' @slot n_comp [numeric]
#' @slot data_directory [character]
#' @slot pc_sum [data.frame]
#' @slot nacho  [data.frame]
#' @slot raw_counts  [data.frame]
#' @slot normalised_counts  [data.frame]
#'
#' @importFrom methods validObject new slotNames
setClass(
  Class = "nacho_set",
  representation = representation(
    access = "character",
    housekeeping_genes = "character",
    normalisation_method = "character",
    remove_outliers = "logical",
    n_comp = "numeric",
    data_directory = "character",
    pc_sum = "data.frame",
    nacho = "data.frame",
    raw_counts = "data.frame",
    normalised_counts = "data.frame"
  ),
  prototype = prototype(
    access = character(),
    housekeeping_genes = character(),
    normalisation_method = character(),
    remove_outliers = logical(),
    n_comp = numeric(),
    data_directory = character(),
    pc_sum = data.frame(),
    nacho = data.frame(),
    raw_counts = data.frame(),
    normalised_counts = data.frame()
  )# ,
  # validity = function (object) {
    # cat("**** validity nacho_set <empty> ****\n")
    # return(TRUE)
  # }
)


### Constructor ###
setGeneric(name = "new_nacho_set", def = function (access, housekeeping_genes, normalisation_method, remove_outliers, n_comp, data_directory, pc_sum, nacho, raw_counts, normalised_counts) {standardGeneric("new_nacho_set")})
setMethod(f = "new_nacho_set", signature = c("missing", "missing", "missing", "missing", "missing", "missing", "missing", "missing", "missing", "missing"), definition = function (access, housekeeping_genes, normalisation_method, remove_outliers, n_comp, data_directory, pc_sum, nacho, raw_counts, normalised_counts) {new("nacho_set")})
setMethod(f = "new_nacho_set", signature = c("ANY", "ANY", "ANY", "ANY", "ANY", "ANY", "ANY", "ANY", "ANY", "ANY"), definition = function (access, housekeeping_genes, normalisation_method, remove_outliers, n_comp, data_directory, pc_sum, nacho, raw_counts, normalised_counts) {
  if (missing(access)) {access <- character()}
  if (missing(housekeeping_genes)) {housekeeping_genes <- character()}
  if (missing(normalisation_method)) {normalisation_method <- character()}
  if (missing(remove_outliers)) {remove_outliers <- logical()}
  if (missing(n_comp)) {n_comp <- numeric()}
  if (missing(data_directory)) {data_directory <- character()}
  if (missing(pc_sum)) {pc_sum <- data.frame()}
  if (missing(nacho)) {nacho <- data.frame()}
  if (missing(raw_counts)) {raw_counts <- data.frame()}
  if (missing(normalised_counts)) {normalised_counts <- data.frame()}
  return(new("nacho_set", access = access, housekeeping_genes = housekeeping_genes, normalisation_method = normalisation_method, remove_outliers = remove_outliers, n_comp = n_comp, data_directory = data_directory, pc_sum = pc_sum, nacho = nacho, raw_counts = raw_counts, normalised_counts = normalised_counts))
})


### Is ###
setGeneric(name = "is_nacho_set", def = function (object) {standardGeneric("is_nacho_set")})
setMethod(f = "is_nacho_set", signature = "ANY", definition = function (object) {
  if (length(object)>1) {
    return(sapply(object, is_nacho_set))
  } else {
    return(class(object) == "nacho_set")
  }
})


### Show ###
#' show
#'
#' @param object [ANY]
setMethod(f = "show", signature = "nacho_set", definition = function (object){
  show_slot <- function (slot) {
    string_name <- gsub("^[^@]*@(.*)", "\\1", slot)
    eval_slot <- eval(parse(text = slot))
    tmp <- switch(EXPR = class(eval_slot),
      "matrix" = {
        cat(paste0(" ~ ", string_name, " : [", nrow(eval_slot), "x", ncol(eval_slot), "]", collapse = ""))
        if (all(dim(eval_slot)==0)) {
          cat("NA")
        } else {
          cat("\n")
          show_nrow <- seq(min(5, nrow(eval_slot)))
          show_ncol <- seq(min(5, ncol(eval_slot)))
          short_object <- eval_slot[show_nrow, show_ncol]
          if (is.null(rownames(short_object))) {
            rownames(short_object) <- seq(nrow(short_object))
          }
          if (is.null(colnames(short_object))) {
            colnames(short_object) <- seq(ncol(short_object))
          }
          format_show_output <- format(cbind(c("", rownames(short_object)), rbind(colnames(short_object), format(short_object, digits = 4))), justify = "centre")
          if (nrow(short_object)!=nrow(eval_slot)) {
            format_show_output <- rbind(format_show_output, c(".", sapply(seq(colnames(short_object)), function (iCol) {paste0(rep(".", nchar(format_show_output[1, 1])), collapse = "")})))
          }
          if (ncol(short_object)!=ncol(eval_slot)) {
            format_show_output <- cbind(format_show_output, c(".", rep(paste0(rep(".", nchar(format_show_output[1, 1])), collapse = ""), nrow(format_show_output)-1)))
          }
          cat(paste0("     ", apply(format(format_show_output, justify = "centre"), 1, paste, collapse = " "), "\n", collapse = ""))
        }
        cat("\n")
      },
      "data.frame" = {
        cat(paste0(" ~ ", string_name, " : [", nrow(eval_slot), "x", ncol(eval_slot), "]", collapse = ""))
        if (all(dim(eval_slot)==0)) {
          cat(" NA")
        } else {
          cat("\n")
          show_nrow <- seq(min(5, nrow(eval_slot)))
          show_ncol <- seq(min(5, ncol(eval_slot)))
          short_object <- eval_slot[show_nrow, show_ncol]
          if (is.null(rownames(short_object))) {
            rownames(short_object) <- seq(nrow(short_object))
          }
          if (is.null(colnames(short_object))) {
            colnames(short_object) <- seq(ncol(short_object))
          }
          format_show_output <- format(cbind(c("", rownames(short_object)), rbind(colnames(short_object), format(short_object, digits = 4))), justify = "centre")
          if (nrow(short_object)!=nrow(eval_slot)) {
            format_show_output <- rbind(format_show_output, c(".", sapply(seq(colnames(short_object)), function (iCol) {paste0(rep(".", nchar(format_show_output[1, 1])), collapse = "")})))
          }
          if (ncol(short_object)!=ncol(eval_slot)) {
            format_show_output <- cbind(format_show_output, c(".", rep(paste0(rep(".", nchar(format_show_output[1, 1])), collapse = ""), nrow(format_show_output)-1)))
          }
          cat(paste0("     ", apply(format(format_show_output, justify = "centre"), 1, paste, collapse = " "), "\n", collapse = ""))
        }
        cat("\n")
      },
      "numeric" = {
        cat(paste0(" ~ ", string_name, " : ", collapse = ""))
        if (length(eval_slot) == 0) {
          cat("NA")
        } else {
          if (length(eval_slot)>1) {
            cat(paste0("[", length(eval_slot), "] ", paste0(format(head(eval_slot), digits = 4), collapse = " ")))
          } else {
            cat(format(eval_slot, digits = 4))
          }
        }
        cat("\n")
      },
      "character" = {
        cat(paste0(" ~ ", string_name, " : ", collapse = ""))
        if (length(eval_slot) == 0) {
          cat("NA")
        } else {
          if (length(eval_slot)>1) {
            cat("[", length(eval_slot), "] \"", paste0(head(eval_slot), collapse = "\" \""), "\"", sep = "")
          } else {
            cat(paste0("\"", eval_slot, "\""))
          }
        }
        cat("\n")
      },
      {
        cat(paste0(" ~ ", string_name, " : ", collapse = ""))
        if (length(eval_slot) == 0) {
          cat("NA")
        } else {
          if (length(eval_slot)>1) {
            cat(paste0("[", length(eval_slot), "] ", paste0(head(eval_slot), collapse = " ")))
          } else {
            cat(eval_slot)
          }
        }
        cat("\n")
      }
    )
    return(invisible())
  }
  show_object <- function (object) {
    cat("  ~~~ Class:", class(object), "~~~\n")
    string_name <- paste0("object@", slotNames(object))
    trash <- sapply(string_name, show_slot)
    return(invisible())
  }
  show_object(object)
  return(invisible(object))
})


### Get ###
#' "["
#'
#' @param x [ANY]
#' @param i [numeric]
#' @param j [numeric]
#' @param drop [logical]
setMethod(f = "[", signature = "nacho_set", definition = function (x, i, j, drop){
  switch(EXPR = i,
    "access" = {
      if (missing(j)) {
        return(x@access)
      } else {
        if (j>length(x@access)) {
          stop("[nacho_set:get] indice out of limits")
        } else {
          return(x@access[j])
        }
      }
    },
    "housekeeping_genes" = {
      if (missing(j)) {
        return(x@housekeeping_genes)
      } else {
        if (j>length(x@housekeeping_genes)) {
          stop("[nacho_set:get] indice out of limits")
        } else {
          return(x@housekeeping_genes[j])
        }
      }
    },
    "normalisation_method" = {
      if (missing(j)) {
        return(x@normalisation_method)
      } else {
        if (j>length(x@normalisation_method)) {
          stop("[nacho_set:get] indice out of limits")
        } else {
          return(x@normalisation_method[j])
        }
      }
    },
    "remove_outliers" = {
      if (missing(j)) {
        return(x@remove_outliers)
      } else {
        if (j>length(x@remove_outliers)) {
          stop("[nacho_set:get] indice out of limits")
        } else {
          return(x@remove_outliers[j])
        }
      }
    },
    "n_comp" = {
      if (missing(j)) {
        return(x@n_comp)
      } else {
        if (j>length(x@n_comp)) {
          stop("[nacho_set:get] indice out of limits")
        } else {
          return(x@n_comp[j])
        }
      }
    },
    "data_directory" = {
      if (missing(j)) {
        return(x@data_directory)
      } else {
        if (j>length(x@data_directory)) {
          stop("[nacho_set:get] indice out of limits")
        } else {
          return(x@data_directory[j])
        }
      }
    },
    "pc_sum" = {return(x@pc_sum)},
    "nacho" = {return(x@nacho)},
    "raw_counts" = {return(x@raw_counts)},
    "normalised_counts" = {return(x@normalised_counts)},
    stop("[nacho_set:get] ", i, " is not a \"nacho_set\" slot")
  )
})


### Set ###
#' "[<-"
#'
#' @param x [ANY]
#' @param i [numeric]
#' @param j [numeric]
#' @param value [ANY]
setMethod(f = "[<-", signature = "nacho_set", definition = function (x, i, j, value){
    switch(EXPR = i,
    "access" = {
      if (missing(j)) {
        x@access <- value
      } else {
        if (j>length(x@access)) {
          stop("[nacho_set:set] indice out of limits")
        } else {
          x@access[j] <- value
        }
      }
    },
    "housekeeping_genes" = {
      if (missing(j)) {
        x@housekeeping_genes <- value
      } else {
        if (j>length(x@housekeeping_genes)) {
          stop("[nacho_set:set] indice out of limits")
        } else {
          x@housekeeping_genes[j] <- value
        }
      }
    },
    "normalisation_method" = {
      if (missing(j)) {
        x@normalisation_method <- value
      } else {
        if (j>length(x@normalisation_method)) {
          stop("[nacho_set:set] indice out of limits")
        } else {
          x@normalisation_method[j] <- value
        }
      }
    },
    "remove_outliers" = {
      if (missing(j)) {
        x@remove_outliers <- value
      } else {
        if (j>length(x@remove_outliers)) {
          stop("[nacho_set:set] indice out of limits")
        } else {
          x@remove_outliers[j] <- value
        }
      }
    },
    "n_comp" = {
      if (missing(j)) {
        x@n_comp <- value
      } else {
        if (j>length(x@n_comp)) {
          stop("[nacho_set:set] indice out of limits")
        } else {
          x@n_comp[j] <- value
        }
      }
    },
    "data_directory" = {
      if (missing(j)) {
        x@data_directory <- value
      } else {
        if (j>length(x@data_directory)) {
          stop("[nacho_set:set] indice out of limits")
        } else {
          x@data_directory[j] <- value
        }
      }
    },
    "pc_sum" = {x@pc_sum <- value},
    "nacho" = {x@nacho <- value},
    "raw_counts" = {x@raw_counts <- value},
    "normalised_counts" = {x@normalised_counts <- value},
    stop("[nacho_set:set] ", i, " is not a \"nacho_set\" slot")
  )
  validObject(x)
  return(invisible(x))
})


### Summary ###
#' summary
#'
#' @param object [ANY]
setMethod(f = "summary", signature = "nacho_set", definition = function (object){
  if (missing(object)){
    stop("[nacho_set:summary] \"object\" is missing", call. = FALSE)
    return(invisible())
  }
  warning("[nacho_set:summary] No summary method defined for \"nacho_set\" object!", call. = FALSE)
  return(invisible(object))
})
