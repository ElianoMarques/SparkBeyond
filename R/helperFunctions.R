#' sampleData
#'
#' Sample data by ratio
#' @param data dataframe to be sampled
#' @param ratio the ratio of data to keep
#' @param seed - Optional. A single numeric value representing the seed for the random sampling. It is guaranteed that for the same data and the same seed, the same sampling will be generated.
#' @return a sampled dataset
sampleData = function(data, ratio = 0.8, seed = 1) {
  set.seed(seed)
  dataSize = nrow(data)
  sampleSize = ceiling(ratio * dataSize)
  data[sample(1:dataSize, sampleSize),]
}

#' sampleData
#'
#' Sample data by an absolute row count
#' @param data dataframe to be sampled
#' @param count the number of elements to keep
#' @param seed - Optional. A single numeric value representing the seed for the random sampling. It is guaranteed that for the same data and the same seed, the same sampling will be generated.
#' @return a sampled dataset
sampleDataAbsolute = function(data, count, seed = 1) {
  set.seed(seed)
  dataSize = nrow(data)
  if (count > dataSize) print("The requested size is larger than the input size - input size will be used instead.")
  data[sample(1:dataSize, min(dataSize, count)),]
}

#' sampleDataByClass
#'
#' Under/over sample data based on labels of \code{columnName}.
#' @param data dataframe to be sampled
#' @param columnName name of column with with labels by which to modify label distributions
#' @param labels Optional. labels in columnName?learn.file for which \code{desiredDistribution} will relate to. Default NA will assume equal distributions between all labels.
#' @param desiredDistribution Optional. The requested distribution ratio of \code{labels}. Default NA will assume equal distributions between all labels.
#' @param seed - Optional. A single numeric value representing the seed for the random sampling. It is guaranteed that for the same data and the same seed, the same sampling will be generated.
#' @return a sampled dataset with the requested distributions
#' @examples
#' hist(sampleDataByClass(getData("titanic_train"), "survived", c(0,1), c(3,1))$survived, plot = FALSE)$counts
sampleDataByClass = function(data, columnName, labels = NA, desiredDistribution = NA, seed = 1) {
  set.seed(seed)
  dataSize = nrow(data)

  if (is.na(labels) || is.na(desiredDistribution)) {
    labels = unique(data[,columnName])
    desiredDistribution = rep(1, length(labels))
  }
  if (length(labels) != length(desiredDistribution)) stop ("The size of labels and desiredDistribution do not match")

  sumDesiredDistribution = sum(desiredDistribution)
  adjustedDesiredDistribution = desiredDistribution/sumDesiredDistribution

  labIndices = sapply(labels, function(label) {which(colsWhiteList(data,columnName)==label)})
  originalDistribution = sapply(labIndices, function(indices) {length(indices)} / dataSize)

  distRatios = mapply(function(original, desired) {original / desired}, originalDistribution, adjustedDesiredDistribution)

  minRatioKey = which.min(distRatios)
  minRatio = distRatios[minRatioKey]
  minRatioKeyDesired = adjustedDesiredDistribution[minRatioKey]
  minRatioKeyExpectedFreq = minRatio *  minRatioKeyDesired

  expectedDistribution = sapply(adjustedDesiredDistribution, function(desiredFreq) {minRatioKeyExpectedFreq * desiredFreq / minRatioKeyDesired})

  samplingProbabilities = round(mapply(function(originalFreq, expected) {expected / originalFreq}, originalDistribution, expectedDistribution), digits=8)

  sampled = mapply(function(indices, prob) {sample(indices, prob*length(indices))}, labIndices, samplingProbabilities, SIMPLIFY=FALSE)
  data[unlist(sampled),]
}

#' trainTestSplit
#'
#' Split data to train + test datasets by ratio
#' @param data dataframe to be splitted
#' @param seed - Optional. A single numeric value representing the seed for the random sampling. It is guaranteed that for the same data and the same seed, the same sampling will be generated.
#' @return a list of two elements with train dataframe as first element, and test dataframe as second element
trainTestSplit = function(data, ratio = 0.8, seed = 1){
  set.seed(seed)
  dataSize = nrow(data)
  sampleSize = ceiling(ratio * dataSize)
  trainIndices = sample(1:dataSize, sampleSize)
  list(train = data[trainIndices,], test = data[-trainIndices,])
}
