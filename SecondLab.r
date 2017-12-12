
midSquere <- function(num) {
  squere = num ^ 2
  filledToEight = fillToEight(squere)
  fourDigits = substring(filledToEight, 3, 6)
  return(as.integer(fourDigits))
}

fillToEight <- function(num) {
  characters <- format(num, scientific = FALSE)
  for(num in 0: (8 - nchar(characters))) {
    characters <- paste0("0", characters)
  }
  return(characters)
}

midSquereMethod <- function(n) {
  currentNumber <- sample(1000:9999, 1)
  numbers <- vector()
  for (i in 1:n) {
    numbers <- c(numbers, currentNumber)
    currentNumber <- midSquere(currentNumber)
  }
  return(numbers)
}

multiplicateMethod <- function(m, k, n) {
  numbers <- vector()
  currentNumber <- sample(1:9999, 1)
  for (i in 1: n) {
    currentNumber <- (k * currentNumber) %% m
    numbers <- c(numbers, currentNumber)
  } 
  return(numbers)
}

alpha = 0.01

findEvInterval <- function(varience, ev, n) {
  t = sqrt(varience) * 2.580 / sqrt(n - 1)
  leftE = ev - t
  rightE = ev + t
  print(paste(leftE, "<= E <=", rightE))
}

findVarInterval <- function(varience, n) {
  leftV = (n - 1) * varience / qchisq((1 + alpha) / 2, n - 1)
  rightV = (n - 1) * varience / qchisq((1 - alpha) / 2, n - 1)
  print(paste(leftV, "<= V <=", rightV))
}

testing <- function(nums, name) {
  n = length(nums)
  hist(nums, probability = FALSE, breaks = 2*log(n), main = paste("Histogram for", name, " "))
  ev = sum(nums) / n
  varience = var(nums)
  print(name)
  findEvInterval(varience, ev, length(nums))
  findVarInterval(varience, length(nums))
  print(paste("E: ", ev))
  print(paste("V: ", varience))
}

inverseNormal <- function(x) {
  t = sqrt(-2 * log(1 - x))
  return(t - (2.30753 + 0.2706 * t) / (1 + 0.99229 * t + 0.04481 * t ^ 2))
}

n = 999
m = 102348091543
k = 12526
numsFromMultiplicativeMethod = multiplicateMethod(m, k = k, n = n) / m
a = mapply(inverseNormal, numsFromMultiplicativeMethod)

myNormByCount = table(cut(a, breaks = 2*log(length(a)), ordered_result = TRUE))
tableNames = names(myNormByCount)
i = 1
totalProb = 0
chiSqr = 0
for (normCount in myNormByCount) {
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
print(paste("Chi table", qchisq(.99, 23)))
print(paste("Total probability: ", totalProb))

testing(a, "My normal")
#testing(rnorm(n), "normal")