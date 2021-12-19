# p_lin: Predict Linear
# Returns a 100 row (x,y) data frame, which describe a line.
p_lin <- function(df, response, term) {
  min = min(df[[term]]) * 100
  max = max(df[[term]]) * 100
  fm <- lm(as.formula(paste(response, "~", term)), data = df)
  df_p <- data.frame(x = (min:max)/100)
  df_p[[term]] <- (min:max)/100
  curve <- data.frame(x = (min:max)/100, y = predict(fm, df_p))
  return(curve)
}


p_supsmu <- function(df, response, term, b=10) {
  curve = supsmu(df[[term]], df[[response]], bass=b)
  return(curve)
}

n_clip <- function(x, a = 0, b = 1) {
  ifelse(x <= a,  a, ifelse(x >= b, b, x))
}