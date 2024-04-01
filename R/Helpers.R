# Copyright 2023 Observational Health Data Sciences and Informatics
#
# This file is part of ReportGenerator
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

#' removeSpaces
#'
#' @description
#' Removes spaces and replaces with under scroll
#'
#' @details
#' Removes spaces and replaces with under scroll
#' 
#' @param x A string
#' @return
#' A string without spaces
#'
#' @export
removeSpaces <- function(x){
  return(gsub(' ', '_', x))
}

getTopPredictors <- function(
    server,
    username,
    password,
    dbms,
    schema,
    targetId,
    outcomeId
){
  
  connectionDetails <- DatabaseConnector::createConnectionDetails(
    dbms = dbms, 
    user = username, 
    password = password, 
    server = server 
  )
  
  connection <- DatabaseConnector::connect(
    connectionDetails = connectionDetails
  )
  on.exit(DatabaseConnector::disconnect(connection))
  
  # get performance_id
  sql <- "
  select * from 
  
  (select d.cdm_source_abbreviation,
  tars.tar_start_day, tars.tar_start_anchor,
  tars.tar_end_day, tars.tar_end_anchor,
  cov.*,
  ROW_NUMBER() OVER(
  PARTITION BY p.performance_id, d.cdm_source_abbreviation,
  tars.tar_start_day, tars.tar_start_anchor,
  tars.tar_end_day, tars.tar_end_anchor
  ORDER BY ABS(cov.covariate_value) DESC
  ) AS rn
  
  from @schema.plp_model_designs md 
  
  inner join @schema.plp_performances p
  on md.model_design_id = p.model_design_id
  
  inner join @schema.plp_covariate_summary cov
  on p.performance_id = cov.performance_id
  
  inner join @schema.plp_database_details dd
  on dd.database_id = p.development_database_id
  
  inner join @schema.database_meta_data d
  on d.database_id = dd.database_meta_data_id 
  
  inner join @schema.plp_tars tars
  on tars.tar_id = p.tar_id 
  
  inner join @schema.plp_cohorts t
  on t.cohort_id = md.target_id
  
  inner join @schema.plp_cohorts o
  on o.cohort_id = md.outcome_id
  
  where o.cohort_definition_id = @outcome_id
  and t.cohort_definition_id = @target_id) temp
  
  where rn <= 100;"
  
  sql <- SqlRender::render(
    sql = sql,
    schema = schema,
    outcome_id = outcomeId,
    target_id = targetId
  )
  
  res <- DatabaseConnector::querySql(
    connection = connection, 
    sql = sql
  )

  
  return(res)

}

getTopCovariates <- function(
    server,
    username,
    password,
    dbms,
    schema,
    targetId,
    outcomeId
    ){
  
  connectionDetails <- DatabaseConnector::createConnectionDetails(
    dbms = dbms, 
    user = username, 
    password = password, 
    server = server 
  )
  
  connection <- DatabaseConnector::connect(
    connectionDetails = connectionDetails
  )
  on.exit(DatabaseConnector::disconnect(connection))
  #mean = n*p, sd = n*p*(1-p), mean = sum
  # p = sum/N, N = sum/p
  sql <- "select 
  run_id,
  database_id, 
  covariate_id, 
  case when (1/2*(average_wo*(1-average_wo) + average_wno*(1-average_wno))) !=0 
  then abs(average_wo - average_wno)/ sqrt(1/2*(average_wo*(1-average_wo) + average_wno*(1-average_wno)))
  else abs(average_wo - average_wno)
  end as smd
  
  from 
  
  (select c.run_id, c.database_id, c.covariate_id, 
  max(case when cois.cohort_type = 'TnO' then c.average_value  else 0 end) as average_wo,
  max(case when cois.cohort_type = 'TnOc' then c.average_value  else 0 end) as average_wno
  
  from @schema.c_covariates c 
  
  inner join
  
  (select run_id, database_id, cohort_definition_id, cohort_type
  from @schema.c_cohort_details 
  where 
  cohort_type in ('TnO', 'TnOc')
  and target_cohort_id = @target_id 
  and outcome_cohort_id = @outcome_id) cois
  
  on c.run_id = cois.run_id
  and c.database_id = cois.database_id 
  and c.cohort_definition_id = cois.cohort_definition_id
  
  group by c.run_id, c.covariate_id, c.database_id
  ) dif
  ;"
  
  sql <- SqlRender::render(
    sql, 
    target_id = targetId,
    outcome_id = outcomeId,
    schema = schema
      )
  
  res <- DatabaseConnector::querySql(connection, sql)
  res <- res[res$SMD > 0.1,]
  
  return(res)
}

getTars <- function(
    data,
    tarColumnNames = c("tarStartWith","tarStartOffset","tarEndWith","tarEndOffset")
    ){
  tar <- data %>% 
    dplyr::select(tarColumnNames)
  
  tar <- unique(tar)
  tar <- lapply(
    X = 1:nrow(tar), 
    FUN = function(i){as.list(tar[i,])}
    )
  return(tar)
}

getAnalyses <- function(
    server,
    username,
    password,
    dbms,
    schema
){
  
  connectionDetails <- DatabaseConnector::createConnectionDetails(
    dbms = dbms, 
    user = username, 
    password = password, 
    server = server 
  )
  
  connection <- DatabaseConnector::connect(
    connectionDetails = connectionDetails
    )
  on.exit(DatabaseConnector::disconnect(connection))
  
  tables <- DatabaseConnector::getTableNames(
    connection = connection, 
    databaseSchema = schema 
  )
  
  resultsRun <- unique(
    unlist(
      lapply(strsplit(tables, '_'), function(x) x[[1]][1])
    )
  )
  
  analyses <- data.frame(
    prefix = c('cd','cg','cm', 'sccs', 'plp', 'c', 'ci'),
    name = c('cohort diagnostics', 'cohort generator',
             'cohort method', 'self controlled case series',
             'patient level prediction', 
             'characterization', 'cohort incidence')
  )
  
  return(analyses[analyses$prefix %in% resultsRun,])
}

getCohortName <- function(
    server,
    username,
    password,
    dbms,
    schema,
    cohortId
    ){
  
  connectionDetails <- DatabaseConnector::createConnectionDetails(
    dbms = dbms, 
    user = username, 
    password = password, 
    server = server 
  )
  
  connection <- DatabaseConnector::connect(
    connectionDetails = connectionDetails
  )
  on.exit(DatabaseConnector::disconnect(connection))
  
  sql <- 'select 
  cohort_name 
  from @schema.cg_cohort_definition
  where cohort_definition_id = @cohort_id
  ;'
  
  sql <- SqlRender::render(
    sql = sql,
    schema = schema,
    cohort_id = cohortId
  )
  
  result <- DatabaseConnector::querySql(
    connection = connection, 
    sql = sql, 
    snakeCaseToCamelCase = T
  )
  
  return(result$cohortName[1])
}

getCohortDescription <- function(
    server,
    username,
    password,
    dbms,
    schema,
    cohortId
){
  
  connectionDetails <- DatabaseConnector::createConnectionDetails(
    dbms = dbms, 
    user = username, 
    password = password, 
    server = server 
  )
  
  connection <- DatabaseConnector::connect(
    connectionDetails = connectionDetails
  )
  on.exit(DatabaseConnector::disconnect(connection))
  
  # if subset get parent json until when not subset 
  sql <- 'select 
  json 
  from @schema.cg_cohort_definition
  where cohort_definition_id = @cohort_id
  ;'
  
  sql <- SqlRender::render(
    sql = sql,
    schema = schema,
    cohort_id = cohortId
  )
  
  result <- DatabaseConnector::querySql(
    connection = connection, 
    sql = sql, 
    snakeCaseToCamelCase = T
  )
  
  return(result$json[1])
}

getIncidenceRates <- function(
    server,
    username,
    password,
    dbms,
    schema,
    targetId,
    outcomeId
){
  
  connectionDetails <- DatabaseConnector::createConnectionDetails(
    dbms = dbms, 
    user = username, 
    password = password, 
    server = server 
  )
  
  connection <- DatabaseConnector::connect(
    connectionDetails = connectionDetails
  )
  on.exit(DatabaseConnector::disconnect(connection))
  
  
  sql <- 'select d.cdm_source_abbreviation as database, i.* 
    from @schema.ci_INCIDENCE_SUMMARY i
    inner join @schema.@database_table_name d
    on d.database_id = i.database_id
    where target_cohort_definition_id in (@target_id)
    and outcome_cohort_definition_id in (@outcome_id)
    ;'

  sql <- SqlRender::render(
    sql = sql,
    schema = schema,
    target_id = targetId,
    outcome_id = outcomeId,
    database_table_name = 'database_meta_data'
  )

  result <- DatabaseConnector::querySql(
    connection = connection, 
    sql = sql, 
    snakeCaseToCamelCase = T
  )
  
  # format the tar
  #result$tar <- paste0('(',result$tarStartWith, " + ", result$tarStartOffset, ') - (', result$tarEndWith, " + ", result$tarEndOffset, ')')
  #result <- result %>% 
  #  dplyr::select(-c("tarStartWith","tarStartOffset","tarEndWith","tarEndOffset", "tarId", "subgroupName"))
  
  result[is.na(result)] <- 'Any'
  result <- unique(result)
  
  return(result)
}

getCounts <- function(
    server,
    username,
    password,
    dbms,
    schema,
    targetId,
    outcomeId
    ){
  
  connectionDetails <- DatabaseConnector::createConnectionDetails(
    dbms = dbms, 
    user = username, 
    password = password, 
    server = server 
  )
  
  connection <- DatabaseConnector::connect(
    connectionDetails = connectionDetails
  )
  on.exit(DatabaseConnector::disconnect(connection))
  
  
  sql <- "
  select 
  d.CDM_SOURCE_ABBREVIATION as database_name,
  cd.COHORT_DEFINITION_ID,
  cd.COHORT_TYPE,
  s.RISK_WINDOW_START,
  s.RISK_WINDOW_END,
  s.START_ANCHOR,
  s.END_ANCHOR,
  cc.ROW_COUNT,
  cc.PERSON_COUNT

  from 
  @schema.c_cohort_counts cc
  inner join
  @schema.database_meta_data d
  on cc.database_id = d.database_id
  
  inner join
  @schema.c_COHORT_DETAILS cd
  
  on cd.COHORT_DEFINITION_ID = cc.COHORT_DEFINITION_ID
  and cd.database_id = cc.database_id 
  and cd.run_id = cc.run_id
  
  inner join @schema.c_settings s
  on s.run_id = cc.run_id
  and s.database_id = cc.database_id
  
  where cd.TARGET_COHORT_ID = @target_id 
  and cd.OUTCOME_COHORT_ID = @outcome_id
  and cd.COHORT_TYPE in ('TnOc', 'TnO')
  ;"
  
  sql <- SqlRender::render(
    sql = sql,
    schema = schema,
    target_id = targetId,
    outcome_id = outcomeId
  )
  
  result <- DatabaseConnector::querySql(
    connection = connection, 
    sql = sql, 
    snakeCaseToCamelCase = T
  )
  
  result <- tidyr::pivot_wider(
    data = result, 
    id_cols = c(
      'databaseName',
      'riskWindowStart',
      'riskWindowEnd',
      'startAnchor',
      'endAnchor'
    ), 
    names_from = c('cohortType'), 
    values_from = 'rowCount'
  )
  
  return(result)
}

getDemographics <- function(
    server,
    username,
    password,
    dbms,
    schema,
    targetId,
    outcomeId
){
  
  connectionDetails <- DatabaseConnector::createConnectionDetails(
    dbms = dbms, 
    user = username, 
    password = password, 
    server = server 
  )
  
  connection <- DatabaseConnector::connect(
    connectionDetails = connectionDetails
  )
  on.exit(DatabaseConnector::disconnect(connection))
  
  # get sex distributions
  sql <-  
    "select 
d.CDM_SOURCE_ABBREVIATION as database_name,
cd.COHORT_TYPE,
s.RISK_WINDOW_START,
s.RISK_WINDOW_END,
s.START_ANCHOR,
s.END_ANCHOR,
coi.covariate_name,
c.sum_value,
c.average_value

from @schema.c_COVARIATES c
 inner join
(
select * from @schema.c_COVARIATE_REF 
  
  where CONCEPT_ID in (
  8507, 8532
  )
) coi

on 
c.database_id = coi.database_id and
c.run_id = coi.run_id and
c.covariate_id = coi.covariate_id

inner join
@schema.c_COHORT_DETAILS cd
on cd.COHORT_DEFINITION_ID = c.COHORT_DEFINITION_ID
and cd.database_id = c.database_id and
cd.run_id = c.run_id 

inner join
@schema.database_meta_data d
on 
c.database_id = d.database_id

inner join 
@schema.c_settings s
on s.run_id = c.run_id
and s.database_id = c.database_id

where cd.TARGET_COHORT_ID = @target_id 
and cd.OUTCOME_COHORT_ID = @outcome_id 
and cd.COHORT_TYPE in ('TnOc', 'TnO')

;
"

sql <- SqlRender::render(
  sql = sql,
  schema = schema,
  target_id = targetId,
  outcome_id = outcomeId
)

sexTable <- DatabaseConnector::querySql(
  connection = connection,  
  sql = sql, 
  snakeCaseToCamelCase = T
)
  
  
  # get age distributions
  sql <- "select 
d.CDM_SOURCE_ABBREVIATION as database_name,
cd.COHORT_TYPE,
s.RISK_WINDOW_START,
s.RISK_WINDOW_END,
s.START_ANCHOR,
s.END_ANCHOR,
coi.covariate_name,
coi.covariate_id,
c.sum_value,
c.average_value

from @schema.c_COVARIATES c
 inner join
(
select * from @schema.c_covariate_ref where analysis_id in (3)
) coi

on 
c.database_id = coi.database_id and
c.run_id = coi.run_id and
c.covariate_id = coi.covariate_id

inner join
@schema.c_COHORT_DETAILS cd
on cd.COHORT_DEFINITION_ID = c.COHORT_DEFINITION_ID
and cd.database_id = c.database_id and
cd.run_id = c.run_id 

inner join
@schema.database_meta_data d
on 
c.database_id = d.database_id

inner join 
@schema.c_settings s
on s.run_id = c.run_id
and s.database_id = c.database_id

where cd.TARGET_COHORT_ID = @target_id 
and cd.OUTCOME_COHORT_ID =@outcome_id
and cd.COHORT_TYPE in ('TnOc', 'TnO')

;
"

sql <- SqlRender::render(
  sql = sql,
  schema = schema,
  target_id = targetId,
  outcome_id = outcomeId
)

ageTable <- DatabaseConnector::querySql(
  connection = connection, 
  sql = sql, 
  snakeCaseToCamelCase = T
)
    
  return(list(
    sex = sexTable,
    age = ageTable
  ))
  
}

#' A function to extract TnO and TnOc characterization results
#'
#' @details
#' The user specifies results database and schema and the covariateIds of interest
#'
#' @param server  the server of the results
#' @param username the connection username to the result database
#' @param password the connection password to the result database
#' @param dbms the connection dbms to the result database
#' @param schema the result database schema
#' @param targetId  The cohort id for the target cohort
#' @param outcomeId The cohort id for the outcome cohort
#' @param covariateIds The vector of covariateIds to extract
#' 
#' @return
#' A data.frame with the specified characterization results
#'
#' @export
getCharacterization <- function(
    server,
    username,
    password,
    dbms,
    schema,
    targetId,
    outcomeId,
    covariateIds
    ){
  
  connectionDetails <- DatabaseConnector::createConnectionDetails(
    dbms = dbms, 
    user = username, 
    password = password, 
    server = server 
  )
  
  connection <- DatabaseConnector::connect(
    connectionDetails = connectionDetails
  )
  on.exit(DatabaseConnector::disconnect(connection))
  
  
  sql <- "select 
d.CDM_SOURCE_ABBREVIATION as database,
cd.COHORT_TYPE,
s.RISK_WINDOW_START,
s.RISK_WINDOW_END,
s.START_ANCHOR,
s.END_ANCHOR,
coi.covariate_name,
c.sum_value,
c.average_value

from @schema.c_COVARIATES c
 inner join
(
select * from @schema.c_COVARIATE_REF 
  
  where COVARIATE_ID in (
  @covariates
  )
) coi

on 
c.database_id = coi.database_id and
c.run_id = coi.run_id and
c.covariate_id = coi.covariate_id

inner join
@schema.c_COHORT_DETAILS cd

on cd.COHORT_DEFINITION_ID = c.COHORT_DEFINITION_ID
and cd.database_id = c.database_id 
and cd.run_id = c.run_id 

inner join
@schema.database_meta_data d
on 
c.database_id = d.database_id

inner join @schema.c_settings s
on s.run_id = c.run_id
and s.database_id = c.database_id

where cd.TARGET_COHORT_ID = @target_id
and cd.OUTCOME_COHORT_ID = @outcome_id
and cd.COHORT_TYPE in ('TnOc', 'TnO')
;
"

sql <- SqlRender::render(
  sql = sql,
  schema = schema,
  target_id = targetId,
  outcome_id = outcomeId,
  covariates = paste(covariateIds, sep=',', collapse = ',')
    )

  result <- DatabaseConnector::querySql(
    connection = connection, 
    sql = sql, 
    snakeCaseToCamelCase = T
  )
  
  result$tar <- paste0(
    result$riskWindowStart, result$riskWindowEnd, 
    result$startAnchor, result$endAnchor
  )
  # convert into a nicer data.frame
  #result <- tidyr::pivot_wider(
  #  data = result, 
  #  id_cols = c('covariateName'), 
  #  names_from = c('databaseName', 'cohortType', 'tar'), 
  #  values_from = 'averageValue', 
  #  values_fn = mean
  #)
  
  return(result)
}





getPredictionResults <- function(
    server,
    username,
    password,
    dbms,
    schema,
    targetId,
    outcomeId
){
  
  connectionDetails <- DatabaseConnector::createConnectionDetails(
    dbms = dbms, 
    user = username, 
    password = password, 
    server = server 
  )
  
  connection <- DatabaseConnector::connect(
    connectionDetails = connectionDetails
  )
  on.exit(DatabaseConnector::disconnect(connection))
  
  
  sql <- "SELECT 
          model_designs.model_design_id, 
          model_settings.model_type AS model_type, 
          targets.cohort_name AS target, 
          outcomes.cohort_name AS outcome,
          tars.tar_start_day, 
          tars.tar_start_anchor, 
          tars.tar_end_day, 
          tars.tar_end_anchor,
          d.CDM_SOURCE_ABBREVIATION as database,
          nResult.population_size, 
          oResult.outcome_count,
          auroc.value as AUROC,
          estat.value as estatistic,
          brier.value as brier

       FROM 
          @schema.plp_model_designs as model_designs 
          inner join
          @schema.plp_model_settings as model_settings
          on model_designs.model_setting_id = model_settings.model_setting_id
         
          inner JOIN
          @schema.plp_performances AS results
            
           on model_designs.model_design_id = results.model_design_id
           
        LEFT JOIN (select * from @schema.plp_EVALUATION_STATISTICS where EVALUATION = 'Test' and METRIC = 'AUROC') auroc
           on auroc.performance_id = results.performance_id
         
        LEFT JOIN (select * from @schema.plp_EVALUATION_STATISTICS where EVALUATION = 'Test' and METRIC = 'brier score scaled') brier
           on brier.performance_id = results.performance_id  
        
        LEFT JOIN (select * from @schema.plp_EVALUATION_STATISTICS where EVALUATION = 'Test' and METRIC = 'Eavg') estat
           on estat.performance_id = results.performance_id    
        
        LEFT JOIN (SELECT performance_id, sum(value) AS population_size FROM @schema.plp_evaluation_statistics where metric = 'populationSize' and evaluation in ('Train','Test') group by performance_id) AS nResult ON results.performance_id = nResult.performance_id
        LEFT JOIN (SELECT performance_id, sum(value) AS outcome_count FROM @schema.plp_evaluation_statistics where metric = 'outcomeCount' and evaluation in ('Train','Test') group by performance_id) AS oResult ON results.performance_id = oResult.performance_id
             
    
        inner JOIN 
        (SELECT c.cohort_id, cd.cohort_definition_id, cd.cohort_name FROM @schema.plp_cohorts c
        inner join @schema.cg_cohort_definition cd
        on c.cohort_definition_id = cd.cohort_definition_id
        ) AS targets 
        ON model_designs.target_id = targets.cohort_id
        inner JOIN 
        (SELECT c.cohort_id, cd.cohort_definition_id, cd.cohort_name FROM @schema.plp_cohorts c
        inner join @schema.cg_cohort_definition cd
        on c.cohort_definition_id = cd.cohort_definition_id
        ) AS outcomes 
        ON model_designs.outcome_id = outcomes.cohort_id
        
        inner JOIN @schema.plp_tars AS tars 
        ON model_designs.tar_id = tars.tar_id
         
        inner JOIN 
        @schema.plp_database_details AS dd
        ON results.development_database_id = dd.database_id 
        
         inner join
         @schema.database_meta_data d
         on dd.database_meta_data_id = d.database_id
  
        WHERE targets.cohort_definition_id in (@target_ids)
        AND outcomes.cohort_definition_id in (@outcome_ids)
        ;"
  
  sql <- SqlRender::render(
    sql = sql,
    schema = schema,
    target_ids = targetId,
    outcome_ids = outcomeId
  )
  
  result <- DatabaseConnector::querySql(
    connection = connection, 
    sql = sql, 
    snakeCaseToCamelCase = T
  )
  
  return(result)
}


# cohort method

computeTraditionalP <- function(
    logRr, 
    seLogRr, 
    twoSided = TRUE, 
    upper = TRUE
) 
{
  z <- logRr/seLogRr
  
  pUpperBound <- 1 - stats::pnorm(z)
  pLowerBound <- stats::pnorm(z)
  
  if (twoSided) {
    return(2 * pmin(pUpperBound, pLowerBound))
  }
  else if (upper) {
    return(pUpperBound)
  }
  else {
    return(pLowerBound)
  }
}

getCMEstimation <- function(
    server,
    username,
    password,
    dbms,
    schema,
    targetId,
    outcomeId,
    comparatorId
){
  
  connectionDetails <- DatabaseConnector::createConnectionDetails(
    dbms = dbms, 
    user = username, 
    password = password, 
    server = server 
  )
  
  connection <- DatabaseConnector::connect(
    connectionDetails = connectionDetails
  )
  on.exit(DatabaseConnector::disconnect(connection))
  
  
  sql <- "select 
  c1.cohort_name as target,
  c2.cohort_name as comparator,
  c3.cohort_name as outcome,
  r.target_id, r.comparator_id, r.outcome_id, r.analysis_id, 
  a.description,
  db.cdm_source_abbreviation as database, r.calibrated_rr, 
  r.calibrated_ci_95_lb, r.calibrated_ci_95_ub, r.calibrated_p,
  r.calibrated_log_rr, r.calibrated_se_log_rr,
  
  r.target_subjects,
  r.comparator_subjects,
  r.target_days,
  r.comparator_days,
  r.target_outcomes,
  r.comparator_outcomes
  
  from 
   @schema.@cm_table_prefixresult as r
   inner join 
   @schema.@cm_table_prefixtarget_comparator_outcome as tco
   on 
   r.target_id = tco.target_id and 
   r.comparator_id = tco.comparator_id and 
   r.outcome_id = tco.outcome_id
   
   inner join
   
   @schema.@cm_table_prefixdiagnostics_summary as unblind
   on
   r.analysis_id = unblind.analysis_id and 
   r.target_id = unblind.target_id and 
   r.comparator_id = unblind.comparator_id and 
   r.outcome_id = unblind.outcome_id and 
   r.database_id = unblind.database_id
   
   inner join
   @schema.@database_table as db
   on db.database_id = r.database_id
   
   inner join
   @schema.@cg_table_prefixcohort_definition as c1
   on c1.cohort_definition_id = r.target_id
   
   inner join
   @schema.@cg_table_prefixcohort_definition as c2
   on c2.cohort_definition_id = r.comparator_id
   
   inner join
   @schema.@cg_table_prefixcohort_definition as c3
   on c3.cohort_definition_id = r.outcome_id
   
   inner join
   @schema.@cm_table_prefixanalysis as a
   on a.analysis_id = r.analysis_id
   
   where 
   r.calibrated_rr != 0 and
   tco.outcome_of_interest = 1 and
   unblind.unblind = 1 and
   r.target_id = @target_id and
   r.outcome_id = @outcome_id and
   r.comparator_id in (@comparator_id)
  ;"
  
  sql <- SqlRender::render(
    sql = sql,
    schema = schema,
    database_table = 'database_meta_data',
    cm_table_prefix = 'cm_',
    cg_table_prefix = 'cg_',
    outcome_id = outcomeId,
    target_id = targetId,
    comparator_id = paste(comparatorId, collapse = ',')
  )
  
  result <- DatabaseConnector::querySql(
    connection = connection, 
    sql = sql, 
    snakeCaseToCamelCase = T
  )
  
  result <- result %>%
    dplyr::mutate(
      calibratedP = ifelse(
        .data$calibratedRr < 1, 
        computeTraditionalP(
          logRr = .data$calibratedLogRr, 
          seLogRr = .data$calibratedSeLogRr, 
          twoSided = FALSE, 
          upper = TRUE
        ),
        .data$calibratedP / 2)
    )
  
  return(result)
}

getCmDiagnosticsData <- function(
    server,
    username,
    password,
    dbms,
    schema,
    targetId,
    outcomeId,
    comparatorId
){
  
  connectionDetails <- DatabaseConnector::createConnectionDetails(
    dbms = dbms, 
    user = username, 
    password = password, 
    server = server 
  )
  
  connection <- DatabaseConnector::connect(
    connectionDetails = connectionDetails
  )
  on.exit(DatabaseConnector::disconnect(connection))
  
  sql <- "
    SELECT DISTINCT
      dmd.cdm_source_abbreviation database_name,
      cma.description analysis,
      cgcd1.cohort_name target,
      cgcd2.cohort_name comparator,
      cgcd3.cohort_name outcome,
      cmds.max_sdm,
      cmds.shared_max_sdm,
      cmds.equipoise,
      cmds.mdrr,
      cmds.attrition_fraction,
      cmds.ease,
      cmds.balance_diagnostic,
      cmds.shared_balance_diagnostic, -- added back
      cmds.equipoise_diagnostic,
      cmds.mdrr_diagnostic,
      cmds.attrition_diagnostic,
      cmds.ease_diagnostic,
      cmds.unblind
    FROM
      @schema.@cm_table_prefixdiagnostics_summary cmds
      INNER JOIN @schema.@cm_table_prefixanalysis cma ON cmds.analysis_id = cma.analysis_id
      INNER JOIN @schema.@database_table dmd ON dmd.database_id = cmds.database_id
      INNER JOIN @schema.@cg_table_prefixcohort_definition cgcd1 ON cmds.target_id = cgcd1.cohort_definition_id
      INNER JOIN @schema.@cg_table_prefixcohort_definition cgcd2 ON cmds.comparator_id = cgcd2.cohort_definition_id
      INNER JOIN @schema.@cg_table_prefixcohort_definition cgcd3 ON cmds.outcome_id = cgcd3.cohort_definition_id
      
      where 
      cgcd1.cohort_definition_id in (@targets)
      and cgcd2.cohort_definition_id in (@comparators)
      and cgcd3.cohort_definition_id in (@outcomes)
      {@use_analyses}?{and cma.analysis_id in (@analyses)}
      ;
  "
  
  sql <- SqlRender::render(
    sql = sql,
    schema = schema,
    cm_table_prefix = 'cm_',
    cg_table_prefix = 'cg_',
    database_table = 'database_meta_data',
    targets = paste0(targetId, collapse = ','),
    comparators = paste0(comparatorId, collapse = ','),
    outcomes = paste0(outcomeId, collapse = ','),
    use_analyses = F
  )
  
  result <- DatabaseConnector::querySql(
    connection = connection, 
    sql = sql, 
    snakeCaseToCamelCase = T
    )
  
  # adding percent fail for summary
  result$summaryValue <- apply(
    X = result[, grep('Diagnostic', colnames(result))], 
    MARGIN = 1, 
    FUN = function(x){
      
      if(sum(x %in% c('FAIL'))>0){
        return('Fail')
      } else if(sum(x %in% c('WARNING')) >0){
        return(sum(x %in% c('WARNING')))
      } else{
        return('Pass')
      }
    }
  )
  
  return(
    result
  )
}


getSccsEstimation <- function(
    server,
    username,
    password,
    dbms,
    schema,
    targetId,
    outcomeId
) {
  connectionDetails <- DatabaseConnector::createConnectionDetails(
    dbms = dbms, 
    user = username, 
    password = password, 
    server = server 
  )
  
  connection <- DatabaseConnector::connect(
    connectionDetails = connectionDetails
  )
  on.exit(DatabaseConnector::disconnect(connection))
  
  sql <- "
  SELECT
  
    ds.cdm_source_abbreviation as database_name,
    sr.exposures_outcome_set_id,
    sr.database_id,
    sc.covariate_id,
    sc.covariate_name,
    sc.era_id,
    sc.covariate_analysis_id,
    sr.analysis_id,
    a.description,
    eos.outcome_id,
    cg1.cohort_name as outcome,
    
  sr.outcome_subjects,
  sr.outcome_events,
  sr.outcome_observation_periods,
  sr.covariate_subjects, 
  sr.covariate_days,
  sr.covariate_eras,
  sr.covariate_outcomes,
  sr.observed_days,

  case when COALESCE(sds.unblind, 0) = 0 then NULL else sr.rr end rr,
  case when COALESCE(sds.unblind, 0) = 0 then NULL else sr.ci_95_lb end ci_95_lb,
  case when COALESCE(sds.unblind, 0) = 0 then NULL else sr.ci_95_ub end ci_95_ub,
  case when COALESCE(sds.unblind, 0) = 0 then NULL else sr.p end p,
  case when COALESCE(sds.unblind, 0) = 0 then NULL else sr.log_rr end log_rr,
  case when COALESCE(sds.unblind, 0) = 0 then NULL else sr.se_log_rr end se_log_rr,
  case when COALESCE(sds.unblind, 0) = 0 then NULL else sr.calibrated_rr end calibrated_rr,
  case when COALESCE(sds.unblind, 0) = 0 then NULL else sr.calibrated_ci_95_lb end calibrated_ci_95_lb,
  case when COALESCE(sds.unblind, 0) = 0 then NULL else sr.calibrated_ci_95_ub end calibrated_ci_95_ub,
  case when COALESCE(sds.unblind, 0) = 0 then NULL else sr.calibrated_p end calibrated_p,
  case when COALESCE(sds.unblind, 0) = 0 then NULL else sr.calibrated_log_rr end calibrated_log_rr,
  case when COALESCE(sds.unblind, 0) = 0 then NULL else sr.calibrated_se_log_rr end calibrated_se_log_rr,
  
  case when COALESCE(sds.unblind, 0) = 0 then NULL else sr.llr end llr,


    sds.mdrr,
  --sds.ease,
  --sds.time_trend_p,
  --sds.pre_exposure_p,
  --sds.mdrr_diagnostic,
  --sds.ease_diagnostic,
  --sds.time_trend_diagnostic,
  --sds.pre_exposure_diagnostic,
  sds.unblind
  
  FROM @schema.@sccs_table_prefixresult sr
  INNER JOIN 
  @schema.@database_table ds 
  ON sr.database_id = ds.database_id
  INNER JOIN 
  @schema.@sccs_table_prefixdiagnostics_summary sds ON (
    sds.exposures_outcome_set_id = sr.exposures_outcome_set_id AND
    sds.database_id = sr.database_id AND
    sds.analysis_id = sr.analysis_id AND
    sds.covariate_id = sr.covariate_id
  )
  INNER JOIN 
  @schema.@sccs_table_prefixcovariate sc ON (
    sc.exposures_outcome_set_id = sr.exposures_outcome_set_id AND
    sc.database_id = sr.database_id AND
    sc.analysis_id = sr.analysis_id AND
    sc.covariate_id = sr.covariate_id
  )
  INNER JOIN @schema.@sccs_table_prefixexposures_outcome_set eos
  ON 
  eos.exposures_outcome_set_id = sr.exposures_outcome_set_id
  INNER JOIN
  @schema.@sccs_table_prefixanalysis a
  on a.analysis_id = sr.analysis_id
  
  inner join 
  @schema.@cg_table_prefixcohort_definition cg1
	on cg1.cohort_definition_id = eos.outcome_id
  
  WHERE eos.outcome_id IN (@outcome_ids)
  -- sr.analysis_id IN (@analysis_ids)
  -- AND sr.database_id IN (@database_ids)
  AND sc.era_id IN (@exposure_ids)
  "
  sql <- SqlRender::render(
    sql = sql,
    schema = schema,
    database_table = 'database_meta_data',
    sccs_table_prefix = 'sccs_',
    cg_table_prefix = 'cg_',
    #database_ids = paste(quoteLiterals(databaseIds), collapse = ','),
    #analysis_ids = paste(analysisIds, collapse = ','),
    outcome_ids = paste(outcomeId, collapse = ','),
    exposure_ids = paste(targetId, collapse = ','),
  )
  
  result <- DatabaseConnector::querySql(
    connection = connection, 
    sql = sql, 
    snakeCaseToCamelCase = T
  )  

  return(result)
}

getSccsDiagnosticsData <- function(
    server,
    username,
    password,
    dbms,
    schema,
    targetId,
    outcomeId
) {

  connectionDetails <- DatabaseConnector::createConnectionDetails(
    dbms = dbms, 
    user = username, 
    password = password, 
    server = server 
  )
  
  connection <- DatabaseConnector::connect(
    connectionDetails = connectionDetails
  )
  on.exit(DatabaseConnector::disconnect(connection))
  
  sql <- "
  SELECT 
  d.cdm_source_abbreviation as database_name,
  c.cohort_name as outcome, 
  c2.cohort_name as target,
  a.description as analysis,
  cov.covariate_name,
  ds.*
  FROM @schema.@sccs_table_prefixdiagnostics_summary ds
            inner join
  @schema.@sccs_table_prefixexposures_outcome_set eos
  on ds.exposures_outcome_set_id = eos.exposures_outcome_set_id
     inner join
   @schema.@cg_table_prefixcohort_definition as c
   on c.cohort_definition_id = eos.outcome_id
   
   INNER JOIN
  @schema.@database_table d
  on d.database_id = ds.database_id
  
  INNER JOIN
  @schema.@sccs_table_prefixanalysis a
  on a.analysis_id = ds.analysis_id
  
  INNER JOIN
  @schema.@sccs_table_prefixcovariate cov
  on cov.covariate_id = ds.covariate_id and 
  cov.exposures_outcome_set_id = ds.exposures_outcome_set_id and
  cov.analysis_id = ds.analysis_id and
  cov.database_id = ds.database_id
  
   inner join
   @schema.@cg_table_prefixcohort_definition as c2
   on cov.era_id = c2.cohort_definition_id
   
   
   where
   
   c2.cohort_definition_id in (@target_ids)
   and c.cohort_definition_id in (@outcome_ids)
   {@use_analysis}?{and a.analysis_id in (@analysis_ids)}
  ;
  "
  
  sql <- SqlRender::render(
    sql = sql,
    schema = schema,
    cg_table_prefix = 'cg_',
    sccs_table_prefix = 'sccs_',
    database_table = 'database_meta_data',
    target_ids = paste0(targetId, collapse = ','),
    outcome_ids = paste0(outcomeId, collapse = ','),
    use_analysis = F
  )
  
  result <- DatabaseConnector::querySql(
    connection = connection, 
    sql = sql, 
    snakeCaseToCamelCase = T
  )
  
  result <- result %>% 
    dplyr::select(-c("analysisId","exposuresOutcomeSetId","databaseId","covariateId"))
  
  result$summaryValue <- apply(
    X = result[, grep('Diagnostic', colnames(result))], 
    MARGIN = 1, 
    FUN = function(x){
      
      if(sum(x %in% c('FAIL'))>0){
        return('Fail')
      } else if(sum(x %in% c('WARNING')) >0){
        return(sum(x %in% c('WARNING'), na.rm = T))
      } else{
        return('Pass')
      }
    }
  )
  return(result)  
  
}

getCmMetaEstimation <- function(
    server,
    username,
    password,
    dbms,
    schema,
    targetId,
    outcomeId,
    comparatorId
){
  connectionDetails <- DatabaseConnector::createConnectionDetails(
    dbms = dbms, 
    user = username, 
    password = password, 
    server = server 
  )
  
  connection <- DatabaseConnector::connect(
    connectionDetails = connectionDetails
  )
  on.exit(DatabaseConnector::disconnect(connection))
  
  sql <- "select 
  c1.cohort_name as target,
  c2.cohort_name as comparator,
  c3.cohort_name as outcome,
  r.target_id, r.comparator_id, r.outcome_id, r.analysis_id, 
  a.description,
  ev.evidence_synthesis_description as database,
  r.calibrated_rr, 
  r.calibrated_ci_95_lb, r.calibrated_ci_95_ub, r.calibrated_p,
  r.calibrated_log_rr, r.calibrated_se_log_rr
  
  from 
   @schema.@es_table_prefixcm_result as r
   inner join 
   @schema.@cm_table_prefixtarget_comparator_outcome as tco
   on 
   r.target_id = tco.target_id and 
   r.comparator_id = tco.comparator_id and 
   r.outcome_id = tco.outcome_id
   
   inner join
   
   @schema.@es_table_prefixcm_diagnostics_summary as unblind
   on
   r.analysis_id = unblind.analysis_id and 
   r.target_id = unblind.target_id and 
   r.comparator_id = unblind.comparator_id and 
   r.outcome_id = unblind.outcome_id 
   
   inner join
   @schema.@cg_table_prefixcohort_definition as c1
   on c1.cohort_definition_id = r.target_id
   
   inner join
   @schema.@cg_table_prefixcohort_definition as c2
   on c2.cohort_definition_id = r.comparator_id
   
   inner join
   @schema.@cg_table_prefixcohort_definition as c3
   on c3.cohort_definition_id = r.outcome_id
   
   inner join
   @schema.@cm_table_prefixanalysis as a
   on a.analysis_id = r.analysis_id
   
   inner join
   @schema.@es_table_prefixanalysis as ev
   on ev.evidence_synthesis_analysis_id = r.evidence_synthesis_analysis_id
   
   where 
   r.calibrated_rr != 0 and
   tco.outcome_of_interest = 1 and
   unblind.unblind = 1 and
   r.target_id = @target_id and
   r.outcome_id = @outcome_id and
   r.comparator_id in (@comparator_id)
  ;"
  
  sql <- SqlRender::render(
    sql = sql,
    schema = schema,
    cm_table_prefix = "cm_",
    cg_table_prefix = "cg_",
    es_table_prefix = "es_",
    target_id = paste0(targetId, collapse = ','),
    outcome_id = paste0(outcomeId, collapse = ','),
    comparator_id = paste0(comparatorId, collapse = ',')
  )
  
  result <- DatabaseConnector::querySql(
    connection = connection, 
    sql = sql, 
    snakeCaseToCamelCase = T
  )
  
  result <- result %>%
    dplyr::mutate(
      calibratedP = ifelse(
        .data$calibratedRr < 1, 
        computeTraditionalP(
          logRr = .data$calibratedLogRr, 
          seLogRr = .data$calibratedSeLogRr, 
          twoSided = FALSE, 
          upper = TRUE
        ),
        .data$calibratedP / 2)
    )
  
  return(unique(result))
}


getSccsMetaEstimation <- function(
    server,
    username,
    password,
    dbms,
    schema,
    targetId,
    outcomeId
) {
  connectionDetails <- DatabaseConnector::createConnectionDetails(
    dbms = dbms, 
    user = username, 
    password = password, 
    server = server 
  )
  
  connection <- DatabaseConnector::connect(
    connectionDetails = connectionDetails
  )
  on.exit(DatabaseConnector::disconnect(connection))

  sql <- "select distinct
  c1.cohort_name as target,
  c3.cohort_name as outcome,
  cov.era_id as target_id, eos.outcome_id, r.analysis_id, 
  r.exposures_outcome_set_id,
  r.outcome_subjects,
  r.observed_days,
  a.description,
  cov.covariate_name as type,
  ev.evidence_synthesis_description as database, 
  r.calibrated_rr, 
  r.calibrated_ci_95_lb, 
  r.calibrated_ci_95_ub,  
  r.calibrated_p,
  r.calibrated_log_rr, 
  r.calibrated_se_log_rr
  
  from 
   @schema.@es_table_prefixsccs_result as r
   inner join 
   @schema.@sccs_table_prefixexposures_outcome_set as eos
   on 
   r.exposures_outcome_set_id = eos.exposures_outcome_set_id
   
   inner join
   @schema.@sccs_table_prefixcovariate as cov
   on 
   r.covariate_id = cov.covariate_id and
   r.analysis_id = cov.analysis_id and
   r.exposures_outcome_set_id = cov.exposures_outcome_set_id
   
   inner join
   @schema.@sccs_table_prefixexposure as ex
   on 
   ex.era_id = cov.era_id and
   ex.exposures_outcome_set_id = cov.exposures_outcome_set_id
   
   inner join
   
   @schema.@es_table_prefixsccs_diagnostics_summary as unblind
   on
   r.analysis_id = unblind.analysis_id and 
   r.exposures_outcome_set_id = unblind.exposures_outcome_set_id and 
   r.covariate_id = unblind.covariate_id and
   r.evidence_synthesis_analysis_id = unblind.evidence_synthesis_analysis_id
   
   inner join
   @schema.@cg_table_prefixcohort_definition as c1
   on c1.cohort_definition_id = cov.era_id

   inner join
   @schema.@cg_table_prefixcohort_definition as c3
   on c3.cohort_definition_id = eos.outcome_id
   
   inner join
   @schema.@sccs_table_prefixanalysis as a
   on a.analysis_id = r.analysis_id
   
   inner join
   @schema.@es_table_prefixanalysis as ev
   on ev.evidence_synthesis_analysis_id = r.evidence_synthesis_analysis_id
   
   where 
   r.calibrated_rr != 0 and
   --ex.true_effect_size != 1 and
   cov.covariate_name in ('Main', 'Second dose') and
   unblind.unblind = 1 and
   cov.era_id = @target_id and
   eos.outcome_id = @outcome_id
  ;"  
  
  sql <- SqlRender::render(
    sql = sql,
    schema = schema,
    sccs_table_prefix = "sccs_",
    cg_table_prefix = "cg_",
    es_table_prefix = "es_",
    target_id = paste0(targetId, collapse = ','),
    outcome_id = paste0(outcomeId, collapse = ',')
  )
  
  result <- DatabaseConnector::querySql(
    connection = connection, 
    sql = sql, 
    snakeCaseToCamelCase = T
  )
  
  return(result)
}