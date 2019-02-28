# file contains the function rankhospital and associated tests

# Get the hospital name, for a particular outcome and rank order, in a state.
#
# parm(character) state: 2-letter state abbreviation
# parm(character) outcome: the condition to test, one of 'heart failure', 'heart attack', or 'neumonia'
# parm(number | 'best' | 'worst') num: the ranking in the state, lower is better.
rankhospital <- function(stateName, outcome, num = "best") {
  # print(paste('rankhospital: ', stateName, outcome, num))
## Read outcome data
  outcomeOfCare <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

## Check that num, state and outcome are valid

  # check state
  if (! stateName %in% unique(outcomeOfCare$State)) {
    stop('invalid state')
  }

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

## Return hospital name in that state with the given rank

  # map outcome to column name
  fullColumn <- c(
    "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack",
    "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure",
    "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
  )
  names(fullColumn) <- outcomes

  # get and clean values for a state
  outcomeColumn <- fullColumn[outcome]
  stateValues <- outcomeOfCare[outcomeOfCare$State == stateName, c('Hospital.Name', outcomeColumn)]
  cleanStateValues <- stateValues[stateValues[,outcomeColumn] != 'Not Available',]
  cleanStateValues[,outcomeColumn] <- as.numeric(cleanStateValues[,outcomeColumn])

  # order state values by outcomeColumn, then by name
  sortedStateIndices <- order(cleanStateValues[,outcomeColumn], cleanStateValues$Hospital.Name)
  sortedStateValues <- cleanStateValues[sortedStateIndices,]
  # print(sortedStateValues)
  if (num == 'best') {
    return(sortedStateValues[1, 'Hospital.Name'])
  } else if (num == 'worst') {
    return(sortedStateValues[nrow(sortedStateValues), 'Hospital.Name'])
  } else if (as.numeric(num) > nrow(sortedStateValues)) {
    return(NA)
  } else {
    num <- as.numeric(num)
    result <- sortedStateValues[num, 'Hospital.Name']
    return(sortedStateValues[num, 'Hospital.Name'])
  }
}

# test function 'rankHospital' according to examples given in programming assignment
test <- function() {

  # helper function to test for calling errors
  checkParameters <- function (stateName, outcomeName, num='best') {
    result <- tryCatch({
      rankhospital(stateName, outcomeName, num)
      stop('no error')
    }, error = function(err) {
      # print(paste('error caught: ', err))
      cerr <-  as.character(err)
      # state error
      if (length(grep('invalid state', cerr)) == 1) {
        return('stateError')
      }
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
    c('WA', 'heart attack', 1),
    c('RI', 'heart failure', 'best'),
    c('RI', 'pneumonia', 'worst'),
    c('PR', 'heart attack', 1)
  )
  # print(validParameters)
  for (parms in validParameters) {
    # print(paste(parms[1], parms[2]))
    result <- checkParameters(parms[1], parms[2], parms[3])
    if (result != 'noError') stop(paste('failed to detect valid got:', result))
  }

  # state error handling
  if (checkParameters('XX', 'heart attack') != 'stateError') stop('failed to detect invalid state name')

  # outcome error handling
  if (checkParameters('RI', 'invalid') != 'outcomeError') stop('failed to detect invalid outcome')

  # num error handling
  if (checkParameters('RI', 'heart attack', num = 'garbage') != 'numError') stop('failed to detect invalid num: garbage')
  if (checkParameters('RI', 'heart attack', 0) != 'numError') stop('failed to detect invalid num: 0')

  # demo results from assignment
  testCases <- list(
    c('TX', 'heart failure', 'FORT DUNCAN MEDICAL CENTER', 'best'),
    c('TX', 'heart failure', 'FORT DUNCAN MEDICAL CENTER', 1),
    c('TX', 'heart failure', 'TOMBALL REGIONAL MEDICAL CENTER', 2),
    c('TX', 'heart failure', 'CYPRESS FAIRBANKS MEDICAL CENTER', 3),
    c('TX', 'heart failure', 'DETAR HOSPITAL NAVARRO', 4),
    c('TX', 'heart failure', 'METHODIST HOSPITAL,THE', 5),
    c('TX', 'heart failure', 'MISSION REGIONAL MEDICAL CENTER', 6),
    c("MD", "heart attack", "HARFORD MEMORIAL HOSPITAL", "worst")
  )
  for (case in testCases) {
    # print(paste('case:', case[1], case[2]))
    if (rankhospital(case[1], case[2], case[4]) != case[3]) {
      stop(paste('testCase failed, rank:',case[4]))
    }
  }
  if (!is.na(rankhospital("MN", "heart attack", 5000))) {
    stop('too large rank should be NA')
  }
  print('all tests passed');
}