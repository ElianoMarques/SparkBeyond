#' Sample data by ratio
#' @param data dataframe to be sampled
#' @return a sampled dataset
sampleData = function(data, ratio = 0.8) {
  dataSize = nrow(data)
  sampleSize = ceiling(ratio * dataSize)
  data[sample(1:dataSize, sampleSize),]
}

#' Sample data by ratio
#' @param data dataframe to be sampled
#' @param columnName name of column with with labels by which to modify label distributions
#' @param labels Optional. labels
#' @return a sampled dataset with the requested distributions
#' @examples
#' hist(sampleDataByClass(getData("titanic_train"), "survived", c(0,1), c(3,1))$survived, plot = FALSE)$counts
sampleDataByClass = function(data, columnName, labels = NA, desiredDistribution = NA) {
  dataSize = nrow(data)

  if (is.na(labels) || is.na(desiredDistribution)) {
    labels = unique(data[,columnName])
    desiredDistribution = rep(1, length(labels))
  }
  if (length(labels) != length(desiredDistribution)) stop ("The size of labels and desiredDistribution do not match")

  sumDesiredDistribution = sum(desiredDistribution)
  adjustedDesiredDistribution = desiredDistribution/sumDesiredDistribution

  labIndices = sapply(labels, function(label) {which(data[,columnName]==label)})
  originalDistribution = sapply(labIndices, function(indices) {length(indices)} / dataSize)

  distRatios = mapply(function(original, desired) {original / desired}, originalDistribution, adjustedDesiredDistribution)

  minRatioKey = which.min(distRatios)
  minRatio = distRatios[minRatioKey]
  minRatioKeyDesired = adjustedDesiredDistribution[minRatioKey]
  minRatioKeyExpectedFreq = minRatio *  minRatioKeyDesired

  expectedDistribution = sapply(adjustedDesiredDistribution, function(desiredFreq) {minRatioKeyExpectedFreq * desiredFreq / minRatioKeyDesired})

  samplingProbabilities = mapply(function(originalFreq, expected) {expected / originalFreq}, originalDistribution, expectedDistribution)

  sampled = mapply(function(indices, prob) {sample(indices, prob*length(indices))}, labIndices, samplingProbabilities, SIMPLIFY=FALSE)
  data[unlist(sampled),]
}

#' Split data to train + test datasets by ratio
#' @param data dataframe to be splitted
#' @return a list of two elements with train dataframe as first element, and test dataframe as second element
trainTestSplit = function(data, ratio = 0.8){
  dataSize = nrow(data)
  sampleSize = ceiling(ratio * dataSize)
  trainIndices = sample(1:dataSize, sampleSize)
  list(data[trainIndices,], data[-trainIndices,])
}
