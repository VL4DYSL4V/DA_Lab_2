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

printMultipleDeterminationCoefficient <- function(
  dependentVectorName,
  independentVectorsNames
) {
  formula <- reformulate(independentVectorsNames, dependentVectorName)
  model <- lm(formula)
  print(
    paste(
      "Coefficient of determination for '", dependentVectorName, "' as dependent variable: ",
      summary(model)$r.squared
    )
  )
}

printMultiplePValue <- function(
  dependentVectorName,
  independentVectorsNames
) {
  formula <- reformulate(independentVectorsNames, dependentVectorName)
  model <- lm(formula)
  vect <- model$model[, dependentVectorName]

  mulCorTest <- cor.test(vect, model$fitted.values)
  mulPValue <- mulCorTest$p.value
  print(
    paste(
      "P-value for '", dependentVectorName, "' as dependent variable: ",
      mulPValue
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

  printMultiplePValue('age', c("trestbps", "chol"))
  printMultiplePValue('trestbps', c('age', 'chol'))
  printMultiplePValue('chol', c('trestbps', 'age'))

  printDelimiterWithNewLines()

  printMultipleDeterminationCoefficient('age', c('trestbps', 'chol'))
  printMultipleDeterminationCoefficient('chol', c('age', 'trestbps'))
  printMultipleDeterminationCoefficient('trestbps', c('chol', 'age'))

  printDelimiterWithNewLines()
}

analyze()
