concat_factor <- function(f1, f2) {
  f1 = factor(f1)
  f2 = factor(f2)
  
  factor(c(levels(f1)[f1], levels(f2)[f2]))
}