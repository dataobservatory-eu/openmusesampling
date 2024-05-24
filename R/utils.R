#' @title XSD time
#' @description
#' Create a character string with an XSD symbol representation of time.
#' @return A character string with an XSD symbol representation of time.
#' @export
xsd_time <- function() {
  xsd_time <- gsub(" ", "T", as.character(as.POSIXct(Sys.time(), tz = "UTC", format="%Y-%m-%d %H:%M:%s")))
  paste0('"', strsplit(xsd_time, "[.]")[[1]][1], 'Z"^^xsd:dateTime')
}


#' @title Create a provenance statement
#' @param startedAtTime Time when the collection process was started.
#' @param endedAtTime Time when the process ended.
#' @param wasAssociatedWith An ORCiD ID, like `<https://orcid.org/0000-0009-9999-8888>`.
#' @return A character string of a provenance statement in Turtle (ttl) format.
#' @examples
#' # example code
#' start_time <- Sys.time()
#' Sys.sleep(1.2)
#' end_time <- Sys.time()
#' provenance_statement (startedAtTime = start_time,
#'                       endedAtTime = end_time,
#'                       wasAssociatedWith = '<https://orcid.org/0000-0009-9999-8888>',
#'                       package = 'spotifyr')
#' @export

provenance_statement <- function(startedAtTime, endedAtTime,
                                 wasAssociatedWith = '<https://orcid.org/0000-0001-7513-6760>',
                                 package = "spotifyr" ) {
  sI <- sessionInfo()
  RSoftwareAgent <- gsub(" ", "_", trimws(strsplit(sI$R.version$version.string, "\\(")[[1]][1]))
  RSoftwareAgentDef <- paste0(':',
                              RSoftwareAgent, ' a prov:SoftwareAgent; rdfs:label "',
                              sI$R.version$version.string, '" . ' )

  if ( package %in% names(sI$otherPkgs)) {

    spotifyrSoftwareAgent <- sI$otherPkgs$spotifyr$Package

    spotifyrSoftwareAgentDef <- paste0(':', spotifyrSoftwareAgent,
                                       ' a prov:SoftwareAgent; rdfs:label "',
                                       sI$otherPkgs$spotifyr$Title,". Version: ",
                                       sI$otherPkgs$spotifyr$Version, '" . ' )
  }

  definition_statements <- paste0(
    ':', wasAssociatedWith, ' a prov:Agent . ',
    RSoftwareAgentDef,
    spotifyrSoftwareAgentDef
  )

  definition_statements

  collection_statement <- paste0(':collectionActivity a prov:Activity; prov:startedAtTime: ',
                                 startedAtTime, '; prov:endedAtTime: ', endedAtTime,
                                 '; wasAssociatedWith: ', wasAssociatedWith,
                                 '; wasAssociatedWith: ', spotifyrSoftwareAgent,
                                 '; wasAssociatedWith: ', RSoftwareAgent, '.')

  paste0(definition_statements, collection_statement)
}
