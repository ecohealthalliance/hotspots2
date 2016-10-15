


winsorize <- function (x, prob = 0.5, multiple = 3) {
  if (length(multiple) != 1 || multiple <= 0) {
    stop("bad value for 'multiple'")
  }
  med <- median(x, na.rm = TRUE)
  y <- x - med # Center so that median = 0
  sc <- qad(y, prob, center=0, na.rm = TRUE) * multiple # Create scaled cutoff
  y[y > sc] <- sc # Remove values outside the scaled cutoff
  y[y < -sc] <- -sc
  y + med # De-center and return data.
}

qad <- function (x, prob = 0.5, center = median(x), constant = 1.4826, na.rm = FALSE) 
{
    if (na.rm) 
        x <- x[!is.na(x)]
    as.numeric(constant * quantile(abs(x - center), probs = prob))
}