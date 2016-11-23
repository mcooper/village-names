Moran.I.Alt <- function (gram, binmat, weight, S1, S2, s.sq, na.rm = FALSE, alternative = "two.sided"){
  x <- as.numeric(binmat[ ,gram])
  n <- length(x)
  ei <- -1/(n - 1)
  m <- mean(x)
  y <- x - m
  cv <- sum(weight * y %o% y)
  v <- sum(y^2)
  obs <- (n/s) * (cv/v)
  k <- (sum(y^4)/n)/(v/n)^2
  sdi <- sqrt((n * ((n^2 - 3 * n + 3) * S1 - n * S2 + 3 * s.sq) - 
                 k * (n * (n - 1) * S1 - 2 * n * S2 + 6 * s.sq))/((n - 
                                                                     1) * (n - 2) * (n - 3) * s.sq) - 1/((n - 1)^2))
  alternative <- match.arg(alternative, c("two.sided", "less", 
                                          "greater"))
  pv <- pnorm(obs, mean = ei, sd = sdi)
  if (alternative == "two.sided") 
    pv <- if (obs <= ei) 
      2 * pv
  else 2 * (1 - pv)
  if (alternative == "greater") 
    pv <- 1 - pv
  data.frame(gram = gram, observed = obs, expected = ei, sd = sdi, p.value = pv, stringsAsFactors = F)
}