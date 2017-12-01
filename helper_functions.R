sapply(array, function(x) {
  df.procrastination$Occupation[x]
})

findOccupation <- function(a) {
  sapply(a, function(x) {
    df.procrastination$Occupation[x]
  })
}

find <- function(pattern) {
  array = grep(df.procrastination$Occupation, pattern = pattern)
  findOccupation(a = array)
}

