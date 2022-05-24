dataHospital <- read.csv('data_hospital.csv')

age <- dataHospital$age
trestbps <- dataHospital$trestbps
chol <- dataHospital$chol

printDelimiterWithNewLines <- function() {
  cat("\n\n=========================================================================\n\n")
}

printEmptyLine <- function() {
  cat("\n")
}

printCorellationCoefficient <- function(vector1, vector2, name1, name2) {
  correlation <- cor(vector1, vector2)
  print(
    paste("Coefficient of correlation between ", name1, " and ", name2, " is: ", correlation)
  )
}

printPValue <- function(vector1, vector2, name1, name2) {
  res <- cor.test(vector1, vector2)
  pValue <- res$p.value
  print(
    paste("P-value of ", name1, " and ", name2, " is: ", pValue)
  )
}

printDeterminationCoefficient <- function(vector1, vector2, name1, name2) {
  linModel <- lm(vector1 ~ vector2)
  print(
    paste(
      "Coefficient of determination for '", name1, "' and '", name2, "': ",
      summary(linModel)$r.squared
    )
  )
}

printMultipleCorrelationCoefficient <- function(
  dependentVectorName,
  independentVectorsNames
) {
  formula <- reformulate(independentVectorsNames, dependentVectorName)
  model <- lm(formula)
  vect <- model$model[, dependentVectorName]
  print(
    paste(
      "Multiple correlation coefficient with '", dependentVectorName, "' as dependent variable: ",
      cor(vect, model$fitted.values)
    )
  )
}

analyze <- function() {
  printCorellationCoefficient(age, trestbps, "age", "trestbps")
  printCorellationCoefficient(age, chol, "age", "chol")
  printCorellationCoefficient(chol, trestbps, "chol", "trestbps")
  printDelimiterWithNewLines()
  printPValue(age, trestbps, "age", "trestbps")
  printPValue(age, chol, "age", "chol")
  printPValue(chol, trestbps, "chol", "trestbps")
  printDelimiterWithNewLines()

  printDeterminationCoefficient(age, trestbps, 'age', 'trestbps')
  printDeterminationCoefficient(age, chol, 'age', 'chol')
  printDeterminationCoefficient(trestbps, chol, 'trestbps', 'chol')

  printDelimiterWithNewLines()

  printMultipleCorrelationCoefficient('age', c("trestbps", "chol"))
  printMultipleCorrelationCoefficient('trestbps', c('age', 'chol'))
  printMultipleCorrelationCoefficient('chol', c('trestbps', 'age'))

  printDelimiterWithNewLines()

  age.model <- lm(age ~ trestbps + chol)
  trestbps.model <- lm(trestbps ~ age + chol)
  chol.model <- lm(chol ~ trestbps + age)
  ageMulCorTest <- cor.test(age.model$model$age, age.model$fitted.values)
  ageMulPValue <- ageMulCorTest$p.value
  print(
    paste(
      "P-value for 'age' as dependent variable: ",
      ageMulPValue
    )
  )
  trestBpsMulCorTest <- cor.test(trestbps.model$model$trestbps, trestbps.model$fitted.values)
  trestbpsMulPValue <- trestBpsMulCorTest$p.value
  print(
    paste(
      "P-value for 'trestbps' as dependent variable: ",
      trestbpsMulPValue
    )
  )
  cholMulCorTest <- cor.test(chol.model$model$chol, chol.model$fitted.values)
  cholMulPValue <- cholMulCorTest$p.value
  print(
    paste(
      "P-value for 'chol' as dependent variable: ",
      cholMulPValue
    )
  )
  printDelimiterWithNewLines()
  ageTrestbpsCholLM <- lm(age ~ trestbps + chol)
  print(
    paste(
      "Coefficient of determination for 'age' as dependent variable: ",
      summary(ageTrestbpsCholLM)$r.squared
    )
  )
  ageCholTrestbpsLM <- lm(chol ~ age + trestbps)
  print(
    paste(
      "Coefficient of determination for 'chol' as dependent variable: ",
      summary(ageCholTrestbpsLM)$r.squared
    )
  )
  trestbpsCholAgeLM <- lm(trestbps ~ chol + age)
  print(
    paste(
      "Coefficient of determination for 'trestbps' as dependent variable: ",
      summary(trestbpsCholAgeLM)$r.squared
    )
  )
  printDelimiterWithNewLines()
}

analyze()
