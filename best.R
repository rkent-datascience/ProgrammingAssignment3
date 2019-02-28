
  # check states# Determine the best hospital in a state for a given outcome
#
# param(character) - the two-letter state code
# outcome(character) - one of "heart attack", "heart failure", or "pneumonia".
#
# returns(character) - name of the hospital with the best (lowest) mortality
#
best <- function(stateName, outcome) {
  # print(paste('stateName:', stateName, 'outcome:', outcome))

## Read outcome data
  outcomeOfCare <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

## Check that state and outcome are valid

  # check states
  if (! stateName %in% unique(outcomeOfCare$State)) {
    stop('invalid state')
  }

  # check outcomes
  outcomes <- c('heart attack', 'heart failure', 'pneumonia')

  if (! outcome %in% outcomes) {
    stop('invalid outcome')
  }

## Return hospital name in that state with lowest 30-day death

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
  cleanStateValues = stateValues[stateValues[,outcomeColumn] != 'Not Available',]
  cleanStateValues[,outcomeColumn] <- as.numeric(cleanStateValues[,outcomeColumn])

  # search for lowest value
  minValue <- 10000.0 # a very large number
  hospitalName <- character(0)
  for (index in 1:nrow(cleanStateValues)) {
    if (minValue > cleanStateValues[index, outcomeColumn]) {
      minValue <- cleanStateValues[index, outcomeColumn]
      hospitalName <- cleanStateValues[index, 'Hospital.Name']
    } else if (minValue == cleanStateValues[index, outcomeColumn]) {
      hospitalName <- c(hospitalName, cleanStateValues[index, 'Hospital.Name'])
    }
  }
  hospitalName <- sort(hospitalName)
  hospitalName[1]
}

# test function 'best' according to examples given in programming assignment
test <- function() {

  # helper function to test for calling errors
  checkParameters <- function (stateName, outcomeName) {
    result <- tryCatch({
      best(stateName, outcomeName)
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
    c('WA', 'heart attack'),
    c('RI', 'heart failure'),
    c('RI', 'pneumonia'),
    c('PR', 'heart attack')
  )
  # print(validParameters)
  for (parms in validParameters) {
    # print(paste(parms[1], parms[2]))
    result <- checkParameters(parms[1], parms[2])
    if (result != 'noError') stop(paste('failed to detect valid got:', result))
  }

  # state error handling
  if (checkParameters('XX', 'heart attack') != 'stateError') stop('failed to detect invalid state name')

  # outcome error handling
  if (checkParameters('RI', 'invalid') != 'outcomeError') stop('failed to detect invalid outcome')

  #check tie results
  if (best('UT', 'pneumonia') != 'LDS HOSPITAL') {
    print(best('UT', 'pneumonia'))
    stop('incorrect UT tie result')
  }
  if (best('MA', 'pneumonia') != 'FALMOUTH HOSPITAL') {
    stop('incorrect MA tie result')
  }

  # sample results from ProgrammingAssignment3.pdf
  testCases <- list(
    c("TX", "heart attack", "CYPRESS FAIRBANKS MEDICAL CENTER"),
    c("TX", "heart failure", "FORT DUNCAN MEDICAL CENTER"),
    c("MD", "heart attack", "JOHNS HOPKINS HOSPITAL, THE"),
    c("MD", "pneumonia", "GREATER BALTIMORE MEDICAL CENTER")
  )
  for (testCase in testCases) {
    if (best(testCase[1], testCase[2]) != testCase[3]) {
      stop(paste('testCase error:', testCase[1], testCase[2]))
    }
  }

  # state error handling
  if (checkParameters('BB', 'heart attack') != 'stateError') stop('failed to detect invalid state name')

  # outcome error handling
  if (checkParameters('NY', 'hert attack') != 'outcomeError') stop('failed to detect invalid outcome')

  print('all tests passed');
}