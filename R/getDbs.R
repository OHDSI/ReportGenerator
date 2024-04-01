getDbs <- function(
  resultsSchema,
  server,
  username,
  password,
  dbms
){
  
  connectionDetails <- DatabaseConnector::createConnectionDetails(
    dbms = dbms,
    server = server,
    user = username,
    password = password
  )
  
con <- DatabaseConnector::connect(
  connectionDetails = connectionDetails
  )
on.exit(DatabaseConnector::disconnect(con))

dbDetails <- data.frame(
  CDM_SOURCE_ABBREVIATION = c(
    "AMBULATORY EMR", "IBM CCAE", "German DA",
    "JMDC", "Optum EHR", "OPTUM Extended SES", "IBM MDCD",
    "IBM MDCR"
  ),
  type = c('us ehr', 'us claims', 'non-us claims',
           "non-us claims", 'us ehr', 'us claims', 'us claims',
           'us claims')
)

sql <- "select CDM_SOURCE_ABBREVIATION from @schema.database_meta_data;"
sql <- SqlRender::render(
  sql = sql,
  schema = resultsSchema
  )
res <- DatabaseConnector::querySql(con, sql)
dbs <- merge(res, dbDetails)$type

types <- lapply(unique(dbs), function(type){sum(dbs == type)})
names(types) <- unique(dbs)

return(types)
}
