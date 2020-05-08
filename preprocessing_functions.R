preprocess_ebovacRnaseq <- function(counts_filename = 'Counts_ebovac.csv',
                                    sampleinfo_filename = 'Descriptive_ebovac.csv',
                                    datadir = '/media/marcon/storage/dataanalysis/data/isobel/ebola/',
                                    outdir = 'ebola/intermediate/preprocessed_data/ebovac',
                                    datasetObj_name = 'ebovacObj.rds')
{
  # DATA
  counts <- data.table::fread(file.path(datadir,counts_filename),stringsAsFactors = F)
  sampleinfo <- data.table::fread(file.path(datadir,sampleinfo_filename),stringsAsFactors = F)
  
  # PREPROCESSING
  #### Counts ####
  #--- row names
  # test if all genes start w/ a letter (might have a problem when start w/ number)
  grep('^\\d+',counts$V1, value = T)
  counts <- data.frame(counts, row.names = 1)
  
  #--- column names
  # Sample names start w/ number. Substitute initial 'X' by 'sample_':
  colnames(counts) <- toupper(colnames(counts))
  colnames(counts) <- gsub('^X','sample_',colnames(counts))

  # test if there are any problematic character:
  grep(' |-|__',colnames(counts), value = T)
  
  #--- general
  # missing values
  if(anyNA(counts)) message('There are missing data in counts table...\n')
  
  #### Sample Info ####
  # clean colnames
  colnames(sampleinfo) <- tolower(colnames(sampleinfo))
  colnames(sampleinfo) <- gsub(' ','_',colnames(sampleinfo))
  
  # test if there`s any sample starting w/ character
  grep('^\\D+',sampleinfo$sample, value = T)
  sampleinfo$sample <- paste0('sample_',sampleinfo$sample)
  sampleinfo <- data.frame(sampleinfo, row.names = 'sample')
  
  #--- Cleaning
  sampleinfo$group_day <- gsub('X','',sampleinfo$group_day)
  sampleinfo$fever <- gsub(',','\\.',sampleinfo$fever)
  sampleinfo$viremia <- gsub(',','\\.',sampleinfo$viremia)
  sampleinfo$group_day <- gsub('X','',sampleinfo$group_day)
  
  #--- Variable type
  # character
  sampleinfo$volunteer_id <- as.character(sampleinfo$volunteer_id)
  # factor
  factor_vars <- c('artrithis','chills','gender','library_batch','myalgia','obj_fever','subj_fever','viremia_d1','pain')
  sampleinfo[factor_vars] <- lapply(sampleinfo[factor_vars],as.factor)
  summary(sampleinfo[factor_vars])
  
  # numeric
  num_vars <- c('fever','group_day','viremia')
  sampleinfo[num_vars] <- lapply(sampleinfo[num_vars], as.numeric)
  
  sampleinfo$class <- 'dummy_class'
  message('Creating SummarizedExperiment...\n')
  ebovacObj <- DaMiRseq::DaMiR.makeSE(x = counts, y = sampleinfo)
  ebovacObj$class <- NULL
  
  dir.create(outdir, recursive = T)
  message('\nSaving preprocessed ebovacObj...\n')
  saveRDS(ebovacObj,file = file.path(outdir, datasetObj_name))
  message('\nebovacObj saved at: ', outdir)
  
  return(ebovacObj)
}