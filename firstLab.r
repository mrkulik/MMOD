middle_Squere_Method <- function(n) {
  value <- sample(9999999:99999999, 1)
  values <- vector()
  for (i in 1: n) {
    values <- c(values, value) 
    value <- middle_Square(value)
  }
  return(values / 1000000000)
}

middle_Square <- function(value) {
  square = value ^ 2
  extended = extend(square)
  middle = substring(extended, 5, 13)
  return(as.integer(middle))
}

extend <- function(value) {
  splited_chars <- format(value, scientific = FALSE)
  if (nchar(splited_chars) < 16) {
    for(value in 0: (16 - nchar(splited_chars))) {
      splited_chars <- paste0("0", splited_chars)
    }
  }
  return(splited_chars)
}

multiplicative_Method <- function(m, k, n) {
  value <- sample(1:99999999, 1)
  values <- vector()
  for (i in 1: n) {
    value <- (k * value) %% m
    values <- c(values, value)
  }
  return(values)
}

test <- function(values) {
  print(values)
  n = length(values)
  ifelse( n <= 100, hist(values, breaks = sqrt(n)), hist(values, breaks = 2*log(n)))
  hist(values, breaks = sqrt(n))
  mo = sum(values) / n
  disp = var(values) * (n-1) / n
  x = values[1:n/2]
  y = values[n/2:n]
  print(paste("MO: ", mo))
  print(paste("DISP: ", disp))
  print(paste("Correlation: ", cor(x, y)))
}

n = 100000
result = middle_Squere_Method(n)
print(result)
test(result)

m = 8954325732
k = 4532
result = multiplicative_Method(m, k = k, n = n) / m
#print(result)
#test(result)
