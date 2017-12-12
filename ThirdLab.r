
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

multiplicateMethod <- function(m, k, n) {
  numbers <- vector()
  currentNumber <- sample(1:9999, 1)
  for (i in 1: n) {
    currentNumber <- (k * currentNumber) %% m
    numbers <- c(numbers, currentNumber)
  } 
  return(numbers)
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
  hist(nums, probability = FALSE, breaks = 3, main = paste("Histogram for", name, " "))
  ev = sum(nums) / n
  varience = var(nums)
  findEvInterval(varience, ev, length(nums))
  findVarInterval(varience, length(nums))
  print(paste("E: ", ev))
  print(paste("V: ", varience))
}

generateNums <- function(n) {
  m = 102348091
  k = 12526
  return(multiplicateMethod(m, k = k, n = n) / m)
}

vector1 <- c(0.1,0.1,0.07)
vector2 <- c(0.2,0.05,0.065,0.3,0.05,0.065)
P <- array(c(vector1,vector2),dim = c(3,3))
Ai = c(1, 3, 4) 
Bi = c(4, 6, 7) 
sum(P) == 1

Px = rowSums(P)

xDistribution <- function(i) {
  return(sum(Pxi[1:i]))
}

Py = P / Px

yDistribution <- function(i, j) {
  return(sum(Py[i,1:j]))
}

findK <- function(arr, randomValue) {
  curSum <- 0
  k = 1
  for (num in arr) {
    curSum = curSum + num
    if (randomValue <= curSum) {
      return(k)
    }
    k = k + 1
  }
}

X = c()
Y = c()
probs = array(0, c(3, 3))
xNums = generateNums(1000)
yNums = generateNums(1000)
for (num in 1: length(xNums)) {
  xk = findK(Px, xNums[num])
  yk = findK(Py[xk,], yNums[num])
  probs[xk, yk] = probs[xk, yk] + 1
  X = c(X, Ai[xk])
  Y = c(Y, Bi[yk])
}

testing(X, "X")
testing(Y, "Y")
print(paste("Correlation coefiicient:", cor(Y, X)))
empericalProbs = probs / length(xNums)

