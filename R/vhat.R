
# data = d, INDEX = ~xcat == "cat1", weights = "w"

#' Estimated variance of a total estimation for a strata
#' 
#' @param data a data frame of individual data. In this version, data should be
#'   the complete dataset of the sample. 
#' @param INDEX a right hand formula whose result is a logical vector that 
#'  indicates the observations for which the variance has to be estimated 
#'  (see the examples below)
#' @param weights character string with the name of the column of weights
#' 
#' @examples 
#' str(d)
#' 
#' vhat_strata(d, INDEX = ~xcat == "cat1", weights = "w")
#' 
#' @export  
vhat_strata <- function(data, INDEX, weights){
  w <- data[[weights]]
  mh <- nrow(data)
  stopifnot(mh > 1)
  Nh <- sum(w)
  
  indicatrice <- as.integer(eval(INDEX[[2]], envir = data))
  indicatrice_w <- indicatrice * w
  
  Nh_i <- sum(indicatrice_w)
  
  vhat <- mh / (mh - 1) * (1 - mh / Nh) * sum((indicatrice_w - Nh_i / mh)^2)

  vhat
}



# data = d, INDEX = ~xcat == "cat1", weights = "w", strata = "s"

#' Estimated variance of a total estimation, with strata
#' 
#' @inheritParams vhat_strata
#' @param strata character string with the name of the column that contains the strata
#' 
#' @examples 
#' str(d)
#' 
#' # Without strata (strata = NULL by default)
#' vhat(d, INDEX = ~xcat == "cat1", weights = "w")
#' 
#' # With strata (column "s")
#' vhat(d, INDEX = ~xcat == "cat1", weights = "w", strata = "s")
#' 
#' @export
vhat <- function(data, INDEX, weights, strata = NULL){
  if(is.null(strata)){
    vhat <- vhat_strata(data = data, INDEX = INDEX, weights = weights)
  }else{
    strata_vector <- data[[strata]]
    if(is.factor(strata_vector)) strata_vector <- droplevels(strata_vector)
    
    data_strata <- split(data, f = strata_vector)
    
    vhat_list <- lapply(data_strata, 
                        function(x) vhat_strata(data = x, INDEX = INDEX, weights = weights))
    vhat <- sum(unlist(vhat_list))
  }
  
  vhat
}
