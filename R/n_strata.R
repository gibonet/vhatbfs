#' Estimate of number of observations in strata of data
#'
#' @param data a data frame
#' @param weights character string of the name of the weights column
#' @param strata character string of the name of the strata column
#'
#' @examples
#' str(d)
#' n_strata(d, weights = "w", strata = "s")
#'
#' @export
n_strata <- function(data, weights, strata) {
  strata_vector <- data[[strata]]
  if (is.factor(strata_vector)) strata_vector <- droplevels(strata_vector)

  data_strata <- split(data, f = strata_vector)

  mh <- sapply(data_strata, nrow)
  Nh <- sapply(data_strata, function(x) sum(x[[weights]]))
  data.frame(mh = mh, Nh = Nh)
}
# tmp <- vhatbfs:::n_strata(d, weights = "w", strata = "s")

#' Stima della varianza della stima di un totale di uno strato dei dati
#'
#' @param data un data frame con il sottoinsieme dei dati per cui si vuole
#'   stimare la varianza della stima di un totale. Da notare la differenza
#'   rispetto all'argomento data della funzione \code{\link{vhat_strata}},
#'   dove data rappresenta tutto il dataset, mentre qui rappresenta solo
#'   il sottoinsieme dei dati la funzione indicatrice vale uno.
#' @param weights stringa con il nome della colonna dei dati che contiene i
#'   pesi individuali
#' @param mh numero di osservazioni dello strato (vettore numerico di 
#'   lunghezza uno).
#' @param Nh stima degli effettivi dello strato (vettore numerico di 
#'   lunghezza uno).
#'   In pratica è la somma dei pesi di campionamento dello strato.
#'
#' @examples
#' tmp <- n_strata(d, weights = "w", strata = "s")
#' tmp
#' tmp$s <- 1L:3L
#'
#' vhat_strata2(d[d$s == 2 & d$xcat == "cat2", ],
#'   weights = "w", mh = tmp$mh[2], Nh = tmp$Nh[2]
#' )
#'
#' @export
vhat_strata2 <- function(data, weights, mh, Nh) {
  k <- nrow(data)

  w <- data[[weights]]

  Nhat <- sum(w)

  primo_termine <- (mh - k) * (Nhat / mh)^2

  secondo_termine <- sum((w - Nhat / mh)^2)

  termine <- primo_termine + secondo_termine
  res <- mh / (mh - 1) * (1 - mh / Nh) * termine
  res
}
# vhatbfs:::vhat_strata2(d[d$s == 2 & d$xcat == "cat2", ], weights = "w", mh = tmp$mh[2], Nh = tmp$Nh[2])
# vhat_strata(d[d$s == 2, ], ~xcat == "cat2", weights = "w")


#' Stima della varianza della stima del totale di un vettore x (uno strato)
#'
#' @param x un vettore numerico con gli elementi di uno strato (di solito i
#'   pesi di campionamento)
#' @param mh numero di osservazioni dello strato (vettore numerico 
#'   di lunghezza uno).
#' @param Nh stima degli effettivi dello strato (vettore numerico di 
#'   lunghezza uno).
#'   In pratica è la somma dei pesi di campionamento dello strato.
#'
#' @examples
#' tmp <- n_strata(d, weights = "w", strata = "s")
#' tmp
#' tmp$s <- 1L:3L
#' vhat_strata3(d$w[d$s == 2 & d$xcat == "cat2"], mh = tmp$mh[2], Nh = tmp$Nh[2])
#'
#' @export
vhat_strata3 <- function(x, mh, Nh) {
  k <- length(x)

  Nhat <- sum(x)

  primo_termine <- (mh - k) * (Nhat / mh)^2

  secondo_termine <- sum((x - Nhat / mh)^2)

  termine <- primo_termine + secondo_termine
  res <- mh / (mh - 1) * (1 - mh / Nh) * termine
  res
}
# vhatbfs:::vhat_strata3(d$w[d$s == 2 & d$xcat == "cat2"], mh = tmp$mh[2], Nh = tmp$Nh[2])
# vhat_strata(d[d$s == 2, ], ~xcat == "cat2", weights = "w")




#' Stima della varianza della stima di un totale di uno strato dei dati
#'
#' Come vhat_strata2 ma mh e Nh sono due colonne dei dati di partenza.
#'
#' @param data un data frame con il sottoinsieme dei dati per cui si vuole
#'   stimare la varianza della stima di un totale. Da notare la differenza
#'   rispetto all'argomento data della funzione \code{\link{vhat_strata}},
#'   dove data rappresenta tutto il dataset, mentre qui rappresenta solo
#'   il sottoinsieme dei dati la funzione indicatrice vale uno.
#' @param weights stringa con il nome della colonna dei dati che contiene i
#'   pesi individuali
#' @param mh nome della colonna dei dati che contiene il numero di osservazioni
#'   dello strato. Deve essere un vettore numerico con lo stesso valore ripetuto
#'   il numero di righe dei dati.
#' @param Nh nome della colonna dei dati che contiene la stima degli effettivi
#'   dello strato. Come mh, deve essere lo stesso valore ripetuto n volte, dove
#'   n è il numero di righe dei dati.
#'
#' @examples
#' tmp <- n_strata(d, weights = "w", strata = "s")
#' tmp$s <- 1L:3L
#' d <- merge(d, tmp, by = "s", all.x = TRUE)
#'
#' # Adesso d ha le colonne mh e Nh
#' vhat_strata4(d[d$s == 2 & d$xcat == "cat2", ], weights = "w", mh = "mh", Nh = "Nh")
#'
#' @export
vhat_strata4 <- function(data, weights, mh, Nh) {
  k <- nrow(data)

  w <- data[[weights]]
  mh <- data[[mh]]
  Nh <- data[[Nh]]

  mh <- unique(mh)
  Nh <- unique(Nh)

  stopifnot(length(mh) == 1 || length(Nh) == 1)

  Nhat <- sum(w)

  primo_termine <- (mh - k) * (Nhat / mh)^2

  secondo_termine <- sum((w - Nhat / mh)^2)

  termine <- primo_termine + secondo_termine
  res <- mh / (mh - 1) * (1 - mh / Nh) * termine
  res
}
# tmp <- vhatbfs:::n_strata(d, weights = "w", strata = "s")
# tmp$s <- 1L:3L
# d <- merge(d, tmp, by = "s", all.x = TRUE)
# vhatbfs:::vhat_strata4(d[d$s == 2 & d$xcat == "cat2", ], weights = "w", mh = "mh", Nh = "Nh")




#' Stima della varianza della stima del totale di un vettore x (uno strato)
#'
#' come \code{\link{vhat_strata3}} ma con nh e Nh che possono anche essere
#' vettori della stessa lunghezza di x (ma devono avere un valore unico...).
#'
#' @param x un vettore numerico con gli elementi di uno strato (di solito i
#'   pesi di campionamento)
#' @param mh vettore con il numero di osservazioni dello strato (può essere
#'   un vettore di lunghezza uno oppure un vettore più lungo ma con un valore
#'   unico ripetuto).
#' @param Nh vettore con la stima degli effettivi dello strato (può essere
#'   un vettore di lunghezza uno oppure un vettore più lungo ma con un valore
#'   unico ripetuto). In pratica è la somma dei pesi di campionamento dello
#'   strato.
#'
#' @examples
#' tmp <- n_strata(d, weights = "w", strata = "s")
#' tmp$s <- 1L:3L
#' d <- merge(d, tmp, by = "s", all.x = TRUE)
#'
#' # Adesso d ha le colonne mh e Nh
#' head(d)
#'
#' k <- with(d, s == 2 & xcat == "cat2")
#' vhat_strata5(d$w[k], d$mh[k], d$Nh[k])
#'
#' @export
vhat_strata5 <- function(x, mh, Nh) {
  k <- length(x)
  mh <- unique(mh)
  Nh <- unique(Nh)

  stopifnot(length(mh) == 1 || length(Nh) == 1)
  Nhat <- sum(x)


  primo_termine <- (mh - k) * (Nhat / mh)^2

  secondo_termine <- sum((x - Nhat / mh)^2)

  termine <- primo_termine + secondo_termine
  res <- mh / (mh - 1) * (1 - mh / Nh) * termine
  res
}
# k <- with(d, s == 2 & xcat == "cat2")
# vhatbfs:::vhat_strata5(d$w[k], d$mh[k], d$Nh[k])


# vhat2: come vhat ma con vhat_strata2
# anzi, con vhat_strata4

#' Estimated variance of a total estimation, with strata
#'
#' @inheritParams vhat_strata4
#' @param strata character string with the name of the column that
#'   contains the strata
#'
#' @examples
#' str(d)
#'
#' tmp <- n_strata(d, weights = "w", strata = "s")
#' tmp$s <- 1L:3L
#' d <- merge(d, tmp, by = "s", all.x = TRUE)
#'
#' # Adesso d ha le colonne mh e Nh
#' str(d)
#' vhat2(data = d[d$xcat == "cat2", ], weights = "w", mh = "mh", Nh = "Nh", strata = "s")
#'
#' @export
vhat2 <- function(data, weights, mh, Nh, strata = NULL) {
  # stopifnot(length(mh) == length(Nh))

  strata_v <- data[[strata]]
  strata_unique <- unique(strata_v)
  v_strata <- vector(mode = "numeric", length = length(strata_unique))

  for (i in seq_along(v_strata)) {
    v_strata[[i]] <- vhat_strata4(
      data[strata_v == strata_unique[i], c(weights, mh, Nh)],
      weights = weights, mh = mh, Nh = Nh
    )
  }

  res <- sum(v_strata)

  res
}
# vhatbfs:::vhat2(data = d[d$xcat == "cat2", ], weights = "w", mh = "mh", Nh = "Nh", strata = "s")
# vhat(data = d, INDEX = ~xcat == "cat2", weights = "w", strata = "s")


# vhat3: come vhat e vhat 2 ma con vhat_strata5 (argomenti: tutti vettori)

#' Estimated variance of a total estimation, with strata (vector version)
#'
#' @inheritParams vhat_strata5
#' @param strata vettore che contiene le informazioni relative agli strati
#' @examples
#' tmp <- n_strata(d, weights = "w", strata = "s")
#' tmp$s <- 1L:3L
#' d <- merge(d, tmp, by = "s", all.x = TRUE)
#'
#' # Adesso d ha le colonne mh e Nh
#' head(d)
#'
#' k <- with(d, xcat == "cat2")
#' vhat3(d$w[k], d$mh[k], d$Nh[k], d$s[k])
#'
#' @export
vhat3 <- function(x, mh, Nh, strata) {
  strata_unique <- unique(strata)
  v_strata <- vector(mode = "numeric", length = length(strata_unique))

  for (i in seq_along(v_strata)) {
    k <- strata == strata_unique[i]
    v_strata[[i]] <- vhat_strata5(x[k], mh = mh[k], Nh = Nh[k])
  }

  res <- sum(v_strata, na.rm = TRUE)

  res
}
