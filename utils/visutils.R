# p_lin: Predict Linear
# Returns a 100 row (x,y) data frame, which describe a line.
p_lin <- function(df, response, term) {
  fm <- lm(as.formula(paste(response, "~", term)), data = df)
  df_p <- data.frame(x = (1:100)/100)
  df_p[[term]] <- (1:100)/100
  curve <- data.frame(x = (1:100)/100, y = predict(fm, df_p))
  return(curve)
}

p_supsmu <- function(df, response, term, b=10) {
  curve = supsmu(df[[term]], df[[response]], bass=b)
  return(curve)
}