dataHospital <- read.csv('data_hospital.csv')

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

analyze <- function() {
  printCorellationCoefficient(dataHospital$age, dataHospital$trestbps, "age", "trestbps")
  printCorellationCoefficient(dataHospital$age, dataHospital$chol, "age", "chol")
  printCorellationCoefficient(dataHospital$chol, dataHospital$trestbps, "chol", "trestbps")
  printDelimiterWithNewLines()
  printPValue(dataHospital$age, dataHospital$trestbps, "age", "trestbps")
  printPValue(dataHospital$age, dataHospital$chol, "age", "chol")
  printPValue(dataHospital$chol, dataHospital$trestbps, "chol", "trestbps")
  printDelimiterWithNewLines()
  age <- dataHospital$age
  trestbps <- dataHospital$trestbps
  chol <- dataHospital$chol
  age.model <- lm(age ~ trestbps + chol)
  print(
    paste(
      "Multiple correlation coefficient with 'age' as dependent variable: ",
      cor(age.model$model$age, age.model$fitted.values)
    )
  )
  trestbps.model <- lm(trestbps ~ age + chol)
  print(
    paste(
      "Multiple correlation coefficient with 'trestbps' as dependent variable: ",
      cor(trestbps.model$model$trestbps, trestbps.model$fitted.values)
    )
  )
  chol.model <- lm(chol ~ trestbps + age)
  print(
    paste(
      "Multiple correlation coefficient with 'chol' as dependent variable: ",
      cor(chol.model$model$chol, chol.model$fitted.values)
    )
  )
  printDelimiterWithNewLines()
  ageTrestbpsLM <- lm(age ~ trestbps)
  print(
    paste(
      "Coefficient of determination for 'age' and 'trestbps': ",
      summary(ageTrestbpsLM)$r.squared
    )
  )
  ageCholLM <- lm(age ~ chol)
  print(
    paste(
      "Coefficient of determination for 'age' and 'chol': ",
      summary(ageCholLM)$r.squared
    )
  )
  trestbpsCholLM <- lm(trestbps ~ chol)
  print(
    paste(
      "Coefficient of determination for 'trestbps' and 'chol': ",
      summary(trestbpsCholLM)$r.squared
    )
  )
  printDelimiterWithNewLines()
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
}

analyze()
