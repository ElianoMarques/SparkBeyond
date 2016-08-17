

CONTEXT_DEFINITION_CLASS = "contextDefinition"
DATA_FRAME_SOURCE_CLASS = "dataFrameSource"

isContextDefinitionObject = function(c) CONTEXT_DEFINITION_CLASS %in% class(c)
isDataFrameSourceObject = function(c) "data.frame" %in% class(c) || DATA_FRAME_SOURCE_CLASS %in% class(c)

#' Available context types
#' 
#' \code{contexts$codeFile(codeFile)}
#' \itemize{
#'  \item{"codeFile"}{url to the file containing additional functions}
#' }
contexts = list(

	codeFile = function(codeFile, name=NULL) {
		contextDefinition = list(
			url = codeFile,
			name = name
		)
		class(contextDefinition) = c("codeFileContextDefinition", CONTEXT_DEFINITION_CLASS)
		contextDefinition
	},
	
	word2Vec = list(
		trainOnData = function(data, keyColumns, name=NULL) {
			if(!isDataFrameSourceObject(data)) {
				stop("Data must be of type data.frame")
			}
			contextDefinition = list(
				data = data,
				keyColumns = keyColumns,
				name = name
			)
			class(contextDefinition) = c("word2VecBasedOnDataContextDefinition", CONTEXT_DEFINITION_CLASS)
			contextDefinition
		},
		
		pretrainedS3 = function(modelName="Wikipedia") {
			contextDefinition = list(
				modelName = modelName
			)
			class(contextDefinition) = c("word2VecPretrainedS3ContextDefinition", CONTEXT_DEFINITION_CLASS)
			contextDefinition
		},
		
		pretrainedLocal = function(data, name=NULL) {
			if(!isDataFrameSourceObject(data)) {
				stop("Data must be of type data.frame")
			}
			contextDefinition = list(
				data = data,
				name = name
			)
			class(contextDefinition) = c("word2VecPretrainedLocalContextDefinition", CONTEXT_DEFINITION_CLASS)
			contextDefinition
		}
	),
	
	featuresFromRevision = function(revision) {
		contextDefinition = list(
			revision = revision
		)
		class(contextDefinition) = c("featuresFromRevisionContextDefinition", CONTEXT_DEFINITION_CLASS)
		contextDefinition
	},
	
	openStreetMap = function(filePath, name=NULL) {
		contextDefinition = list(
			filePath = filePath,
			name = name
		)
		class(contextDefinition) = c("openStreetMapContextDefinition", CONTEXT_DEFINITION_CLASS)
		contextDefinition
	}
)
