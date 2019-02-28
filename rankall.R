# file contains rankall function from Programming Assignment 3, with associated test.

#
# Determine, for each state, the hospital with a particular rank in mortality for
# various outcomes.
#
# outcome(character): One of 'heart failure', 'heart attack', 'pneumonia'
# num(integer | 'best' | 'worst'): desired rank in state
#
# result(data.frame): 'hospital' of the desired rank, by 'state'.
#
rankall <- function(outcome, num = "best") {
  # print(paste('rankall outcome:', outcome, 'num:', num))

## Read outcome data

  outcomeOfCare <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

## Check that num and outcome are valid

  # check outcomes
  outcomes <- c('heart attack', 'heart failure', 'pneumonia')

  if (! outcome %in% outcomes) {
    stop('invalid outcome')
  }

  # check num
  if (!(num == 'best' || num == 'worst')) {
    # must be numeric and within range
    if (is.na(suppressWarnings(as.numeric(num))) ||
      (as.numeric(num) <= 0)) {
      stop('invalid num')
    }
  }

## For each state, find the hospital of the given rank

  # map outcome to column name
  fullColumn <- c(
    "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack",
    "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure",
    "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
  )
  names(fullColumn) <- outcomes

  # get and clean values for a state
  outcomeColumn <- fullColumn[outcome]
  stateValues <- outcomeOfCare[, c('State', 'Hospital.Name', outcomeColumn)]
  cleanStateValues <- stateValues[stateValues[,outcomeColumn] != 'Not Available',]
  cleanStateValues[,outcomeColumn] <- as.numeric(cleanStateValues[,outcomeColumn])

## Return a data frame with the hospital names and the
## (abbreviated) state name
  sortedStateIndices <- order(
    cleanStateValues$State,
    cleanStateValues[,outcomeColumn],
    cleanStateValues$Hospital.Name)
  sortedStateValues <- cleanStateValues[sortedStateIndices,]

  # fill a data frame with NA values
  state <- unique(outcomeOfCare$State)
  hospital <- rep(NA, times = length(state))
  df <- data.frame(state, hospital = as.character(hospital), row.names = state, stringsAsFactors = FALSE)

  # loop through states, filling in rank
  if (num == 'best') {
    index <- 1
  } else if (num != 'worst') {
    index <- as.numeric(num)
  }
  for (stateName in state) {
    sortedState <- sortedStateValues[sortedStateValues$State == stateName, 'Hospital.Name']
    if (num == 'worst') {
      index <- length(sortedState)
    }
    if (index <= length(sortedState)) {
      df[[stateName, 'hospital']] <- sortedState[index]
    }
  }
  df
}

# test function 'rankall' according to examples given in programming assignment
test <- function() {
  print('testing rankall')

  # helper function to test for calling errors
  checkParameters <- function (outcomeName, num='best') {
    result <- tryCatch({
      rankall(outcomeName, num)
      stop('no error')
    }, error = function(err) {
      # print(paste('error caught: ', err))
      cerr <-  as.character(err)
      # outcome error
      if (length(grep('invalid outcome', cerr)) == 1) {
        return('outcomeError')
      }
      # num error
      if (length(grep('invalid num', cerr)) == 1) {
        return('numError')
      }
      # successful result
      if (length(grep('no error', cerr)) == 1) {
        return('noError')
      }
      # we don't expect this
      return(err)
    })
  }

  # valid results
  validParameters <- list(
    c('heart failure', 'best'),
    c('pneumonia', 'worst'),
    c('heart attack', 2)
  )
  for (parms in validParameters) {
    result <- checkParameters(parms[1], parms[2])
    if (result != 'noError') stop(paste('failed to detect valid got:', result))
  }

  # outcome error handling
  if (checkParameters('invalid') != 'outcomeError') stop('failed to detect invalid outcome')

  # num error handling
  if (checkParameters('heart attack', num = 'garbage') != 'numError') stop('failed to detect invalid num: garbage')
  if (checkParameters('heart attack', 0) != 'numError') stop('failed to detect invalid num: 0')

  # Sample results from assignment
  testCases <- list(
    c('heart attack', '20', 'AK', 'NA'),
    c('heart attack', '20', 'CT', 'MIDSTATE MEDICAL CENTER'),
    c('pneumonia', 'worst', 'WV', 'PLATEAU MEDICAL CENTER'),
    c('heart failure', 'NULL', 'WA', 'HARBORVIEW MEDICAL CENTER'),
    c('heart failure', 'NULL', 'VA', 'SENTARA POTOMAC HOSPITAL')
  )

  for (case in testCases) {
    if (case[2] != 'NULL')  {
      result <- rankall(case[1], case[2])
    } else {
      result <- rankall(case[1])
    }
    stateResult <- result[result$state == case[3], 'hospital']
    print(paste('stateResult', stateResult))
    if (is.na(stateResult)) {
      stateResult <- 'NA'
    }
    if (stateResult != case[4]) {
      stop(paste('Failed test for state', case[3]))
    } else {
      print(paste('test passed for state: ', case[3]))
    }
  }

  print('all tests passed')
}