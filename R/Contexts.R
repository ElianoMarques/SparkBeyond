#' Container for context definition helpers
#' 
#' @description
#' \itemize{
#'   \item codeFile(codeFile, name) add functions from external code file
#'   \item featuresFromRevision(revision, name) start the learning with features discovered in a previous revision
#'   \item openStreetMap(filePath, name) define a context based on Open Street Map file
#'   \item shapeFile(filePath, name) define a context based on Shape File. filePath should point to a <FILE_NAME>.shp file.
#'   Additionally at least 2 files should be present in the same directory: <FILE_NAME>.shx, <FILE_NAME>.dbf.
#'   \item word2Vec can be used in several ways:
#'   \itemize{
#'     \item pretrainedS3(modelName, name) Word2Wec using pretrained models from S3
#'     \item pretrainedLocal(data, name) Word2Wec using supplied pretrained vectors
#'     \item trainOnData(data, keyColumns, name) Word2Wec trained on the supplied data
#'   }
#' }
#' 
#' @usage
#' contexts$codeFile(codeFile, name)
#'   codeFile - url to a file containing the functions
#'   name (optional) - name of the context
#'   
#' contexts$featuresFromRevision(revision, name)
#'   revision - revision to take the features from
#'   name (optional) - name of the context
#'   
#' contexts$openStreetMap(filePath, name)
#'   filePath - local path to the OSM file
#'   name (optional) - name of the context
#'   
#' contexts$shapeFile(filePath, name)
#'   filePath - local path to the shape file
#'   name (optional) - name of the context
#'
#' contexts$word2Vec$pretrainedS3(modelName, name)
#'   modelName name of one of the pretrained models. Available models: "Wikipedia", "Wikipedia-Gigaword", "CommonCrawl", "Twitter", "GoogleNews"
#'   name (optional) - name of the context
#'   
#' contexts$word2Vec$pretrainedLocal(data, name)
#'   data a data frame which contains texts column followed by Word2Vec vector columns
#'   name (optional) - name of the context
#'   
#' contexts$word2Vec$trainOnData(data, keyColumns, name)
#'   data - dataframe based on which a Word2Vec model will be created
#'   keyColumns - names of columns to be used for building word2Vec model
#'   name (optional) - name of the context
#' 
#' @examples
#' Features from revision example, reusing features discovered in revision 5
#' \donttest{
#'   model = learn(
#'     projectName = "titanic_using_featuresFromRevision",
#'     trainData = data,
#'     target = "survived",
#'     contextDatasets = list(contexts$featuresFromRevision(revision = 5)))
#' }
#' @format 
#' 
contexts = list(

	codeFile = function(codeFile, name=NULL) {
		contextDefinition = list(
			url = codeFile,
			name = name
		)
		class(contextDefinition) = c("codeFileContextDefinition", "contextDefinition")
		contextDefinition
	},
	
	word2Vec = list(
		trainOnData = function(data, keyColumns, name=NULL) {
			if(!any(c("data.frame", "dataFrameSource")  %in% class(data))) {
				stop("data must be of type data.frame")
			}

			contextDefinition = list(
				data = data,
				keyColumns = keyColumns,
				name = name
			)
			class(contextDefinition) = c("word2VecBasedOnDataContextDefinition", "contextDefinition")
			contextDefinition
		},
		
		pretrainedS3 = function(modelName="Wikipedia", name=NULL) {
			contextDefinition = list(
				modelName = modelName,
				name = name
			)
			class(contextDefinition) = c("word2VecPretrainedS3ContextDefinition", "contextDefinition")
			contextDefinition
		},
		
		pretrainedLocal = function(data, name=NULL) {
			if(!any(c("data.frame", "dataFrameSource")  %in% class(data))) {
				stop("data must be of type data.frame")
			}
			
			contextDefinition = list(
				data = data,
				name = name
			)
			class(contextDefinition) = c("word2VecPretrainedLocalContextDefinition", "contextDefinition")
			contextDefinition
		}
	),
	
	featuresFromRevision = function(revision, name=NULL) {
		contextDefinition = list(
			revision = revision,
			name = name
		)
		class(contextDefinition) = c("featuresFromRevisionContextDefinition", "contextDefinition")
		contextDefinition
	},
	
	openStreetMap = function(filePath, name=NULL) {
		contextDefinition = list(
			filePath = filePath,
			name = name
		)
		class(contextDefinition) = c("openStreetMapContextDefinition", "contextDefinition")
		contextDefinition
	},
	
	shapeFile = function(filePath, name=NULL) {
		contextDefinition = list(
			filePath = filePath,
			name = name
		)
		class(contextDefinition) = c("shapeFileContextDefinition", "contextDefinition")
		contextDefinition
	}
)
