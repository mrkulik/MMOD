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
  value <- sample(1:9999, 1)
  values <- vector()
  for (i in 1: n) {
    value <- (k * value) %% m
    values <- c(values, value)
  }
  return(values)
}

alpha = 0.01

findEvInterval <- function(varience, ev, n) {
  t = sqrt(varience) * 2.7850 / sqrt(n - 1) #значение из таблицы стьюдента
  leftE = ev - t
  rightE = ev + t
  print(paste(leftE, "<= E <", rightE))
}

findVarInterval <- function(varience, n) {
  leftV = (n - 1) * varience / qchisq((1 + alpha) / 2, n - 1)
  rightV = (n - 1) * varience / qchisq((1 - alpha) / 2, n - 1)
  print(paste(leftV, "<= V <", rightV))
}

inverseNormal <- function(x) { # апроксимированная обратная функция нормального распределния
  t = sqrt(-2 * log(1 - x))
  return(t - (2.30753 + 0.2706 * t) / (1 + 0.99229 * t + 0.04481 * t ^ 2))
}

testing <- function(nums, name) {
  n = length(nums)
  ifelse( n <= 100, hist(nums, probability = FALSE, breaks = sqrt(n), main = paste("Histogram for", name, " ")), hist(nums, probability = FALSE, breaks = 2*log(n), main = paste("Histogram for", name, " ")))
  ev = sum(nums) / n
  varience = var(nums)
  print(name)
  findEvInterval(varience, ev, length(nums))
  findVarInterval(varience, length(nums))
  print(paste("E: ", ev))
  print(paste("V: ", varience))
}

n = 100
m = 102348091
k = 12526
numsFromMultiplicativeMethod = multiplicative_Method(m, k = k, n = n) / m
y_nums = mapply(inverseNormal, numsFromMultiplicativeMethod)

y_count = length(y_nums)
ranges_table = table(cut(y_nums, breaks = sqrt(y_count), ordered_result = TRUE))
#print(ranges_table)
tableNames = names(ranges_table)
#print(tableNames)

i = 1
totalProb = 0
chiSqr = 0
for (normCount in myNormByCount) { #
  name = tableNames[i]
  substringName = substring(tableNames[i], 2, nchar(name) - 1)
  splitted = strsplit(substringName, ",")
  left = as.double(unlist(splitted)[[1]])
  right = as.double(unlist(splitted)[[2]])
  probabilityInStandardNorm = pnorm(right) - pnorm(left)
  totalProb = totalProb + probabilityInStandardNorm 
  nextChi = (probabilityInStandardNorm - normCount / n) ^ 2 / probabilityInStandardNorm
  chiSqr = chiSqr + nextChi
  i = i + 1
}

print(paste("Chi squere: ", n * chiSqr))
print(paste("Total probability: ", totalProb))

testing(a, "normal from uniform")
#testing(rnorm(n), "normal")