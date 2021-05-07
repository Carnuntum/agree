lorem <- 'Lorem ipsum dolor sit amet, consectetuer adipiscing elit. Maecenas porttitor congue massa. Fusce posuere, magna sed pulvinar ultricies, purus lectus malesuada libero, sit amet commodo magna eros quis urna. Nunc viverra imperdiet enim. Fusce est. Vivamus a tellus. Pellentesque habitant morbi tristique senectus et netus et malesuada fames ac turpis egestas. Proin pharetra nonummy pede. Mauris et orci. Aenean nec lorem.
In porttitor. Donec laoreet nonummy augue. Suspendisse dui purus, scelerisque at, vulputate vitae, pretium mattis, nunc. Mauris eget neque at sem venenatis eleifend. Ut nonummy. Fusce aliquet pede non pede. Suspendisse dapibus lorem pellentesque magna. Integer nulla. Donec blandit feugiat ligula. Donec hendrerit, felis et imperdiet euismod, purus ipsum pretium metus, in lacinia nulla nisl eget sapien.
'

docum_text <- tagList('This is an application programmed R using the Shiny package for an easy access to 
several measures of agreement. You do not have to have any knowledge of programming at all in order 
to use this application. On' , a("Placeholder", href = 'https://github.com/'), 'you can find the paper 
describing each measure used in this application for a kind of primer on how and when to use different 
measures.')


file_upload_text <- 'On the right side you can upload your .csv file. Please carefully look at the
required structure of the uploaded file below.'

file_struct_text <- 'The data has to have the following structure:'

look_down <- div(icon("caret-down"), style = "font-size: 30px; color: #3c8dbc;")

#-----------------------------------------------------------------------------
#'*------------------------------- SCOTT PI ---------------------------------*
#-----------------------------------------------------------------------------

pi_output_description <- data.frame('method' = 'the choosen weighting sceme',
                                    'obs and sample' = 'number of raters and number of rated objects respectively',
                                    'est' = 'the estimated value of scott"s pi',
                                    'se' = 'the correspoinding standard error',
                                    'conf.level' = 'the confidence interval level of 0.95',
                                    'lb' = 'lower bound of the confidence interval',
                                    'ub' = 'upper bound of the confidence interval')

pi_docum_text <- "Scott's Pi was one of the first \"kappa\" type measures and... "

#-----------------------------------------------------------------------------
#'*------------------------------- COHEN KAPPA ------------------------------*
#-----------------------------------------------------------------------------

cohen_output_description <- data.frame('method' = 'the choosen weighting sceme', 
                                       'subjects and raters' = 'the dimensions of uploaded data', 
                                       'irr.name' = 'naming sceme of the underlying function', 
                                       'value' = paste0('the kappa value ranging from 0 (no agreement) to 1 (perfect agreement).',
                                                        'Negative values indicate less agreement than expected by chance.'), 
                                       'stat.name' = 'the corresponding test statistic', 
                                       'statistic' = 'the z value', 
                                       'p.value' = 'significance value for 0.05')



cohen_docum_text <-
p("Cohen's Kappa is, despite some flaws, one of the most used measures of agreement to this date. 
It ranges from 0 to 1, where 0 indicates no agreement and 1 perfect agreement. Originally it was
developed for 2 raters and N subjects and hast been generalized a couple of times (e.g. Fleiss' Kappa 
Conger's Kappa etc.)... ")

#-----------------------------------------------------------------------------
#'*----------------------------- RAND INDEX ----------------------------------*
#-----------------------------------------------------------------------------

randIndex_output_description <- data.frame('est' = 'the estimated value of the Rand Index.',
                                           'lb' = 'lower bound of the confidence interval',
                                           'ub' = 'upper bound of the confidence interval')

randIntex_docum_text <- 
"The adjusted rand index is a measure between 2 clusters or partitions 
and can be used to quantify the amount of agreement between 2 raters and N subjects...
"

#-----------------------------------------------------------------------------
#'*----------------------------- CONGER KAPPA -------------------------------*
#-----------------------------------------------------------------------------

conger_output_description <- data.frame('est.pa' = 'the raw percentage of agreement',
                                        'est.pe' = 'the expected percentage of agreement',
                                        'est.coeff.val' = 'the estimated value of Conger"s Kappa',
                                        'est.coeff.se' = 'the corresponding standard error',
                                        'est.conf.int' = 'lower and upper bound of the 95% confidence interval.',
                                        'est.p.value' = 'the corresponding p value',
                                        'est.w.name' = 'the chosen weighting sceme')

conger_docum_text <-
"Conger's Kappa is..."

#-----------------------------------------------------------------------------
#'*----------------------------- FLEISS KAPPA -------------------------------*
#-----------------------------------------------------------------------------

fleiss_output_description <- data.frame('method' = 'the choosen weighting sceme', 
                                        'subjects and raters' = 'the dimensions of uploaded data', 
                                        'irr.name' = 'naming sceme of the underlying function', 
                                        'value' = paste0('the kappa value ranging from 0 (no agreement) to 1 (perfect agreement).',
                                                         'Negative values indicate less agreement than expected by chance.'), 
                                        'stat.name' = 'the corresponding test statistic', 
                                        'statistic' = 'the z value', 
                                        'p.value' = 'significance value for 0.05')

fleiss_docum_text <-
"Similar to Conger's Kappa, Fleiss' Kappa was developed to calculate agreement
among more than 2 raters..."

#-----------------------------------------------------------------------------
#'*----------------------------- BRENNAN KAPPA ------------------------------*
#-----------------------------------------------------------------------------

brennan_output_description <- data.frame('pa' = 'the raw percentage of agreement',
                                         'pe' = 'the expected percentage of agreement',
                                         'coeff.val' = 'the estimated value of Conger"s Kappa',
                                         'coeff.se' = 'the corresponding standard error',
                                         'conf.int' = 'lower and upper bound of the 95% confidence interval.',
                                         'p.value' = 'the corresponding p value',
                                         'w.name' = 'the chosen weighting sceme')

brennan_docum_text <-
"Brennan and Prediger's Kappa was developed by ... and was rediscovered a few times
by ... "

#-----------------------------------------------------------------------------
#'*------------------------------- RWG --------------------------------------*
#-----------------------------------------------------------------------------

rwg_output_description <- data.frame('grpid' = 'list of grouping variables found in the "grpid" column of the data',
                                     'rwg.j' = 'estimated value of rwg or rwg(j) for multiple items',
                                     'gsize' = 'group size under each grouping variable')

rwg_docum_text <-
"The reliability within group index was first introduced by..."

#-----------------------------------------------------------------------------
#'*------------------------------- AWG --------------------------------------*
#-----------------------------------------------------------------------------

awg_output_description <- data.frame('grpid' = 'list of grouping variables found in the "grpid" column of the data',
                                     'a.wg' = 'estimated value of awg or awg(j) for multiple items',
                                     'nitems' = 'number of items in data',
                                     'nraters' = 'number of raters in data',
                                     'avg.grp.var' = 'average group variance')

awg_docum_text <-
  "The agreement within group index was first introduced by..."

#-----------------------------------------------------------------------------
#'*-------------------------- BANGDIWALA B ----------------------------------*
#-----------------------------------------------------------------------------

bangdiwala_output_description <- data.frame('Bangdiwala' = 'estimated value',
                                            'weights -' = 'list of weight values')

bangdiwala_docum_text <-
"Bangdiwala's B was introduced by ..."

#-----------------------------------------------------------------------------
#'*--------------------------- AICKIN ALPHA ---------------------------------*
#-----------------------------------------------------------------------------

aickin_output_description <- data.frame('lcl and ucl' = 'lower and upper level of the 95% confidence interval')

aickin_docum_text <-
"Aickin's Alpha is ye another alternative to Cohen's Kappa as it ...."

#-----------------------------------------------------------------------------
#'*---------------------------- BYRT KAPPA ----------------------------------*
#-----------------------------------------------------------------------------

byrt_output_description <- data.frame('po' = 'observed raw percentage agreement',
                                      'byrt' = 'value of byrts kappa',
                                      'lb and ub' = 'lower and upper bound of the 95% confidence interval')

byrt_docum_text <-
"Byrt's Kappa (also called PABAK for prevalence adjusted, bias adjusted kappa)
tries to overcome ..."

#-----------------------------------------------------------------------------
#'*------------------------------ IOTA --------------------------------------*
#-----------------------------------------------------------------------------

iota_output_description <- data.frame('method' = 'number of variables in dataset',
                                      'subjects and raters' = 'the dimensions of uploaded data',
                                      'value' = 'estimated iota value',
                                      'lb' = 'lower bound of the confidence interval',
                                      'ub' = 'upper bound of the confidence interval')

iota_docum_text <-
"The iota measure developed by ... is a measure for multivariate agreement ..."

#-----------------------------------------------------------------------------
#'*---------------------------- GWET AC1/2 ----------------------------------*
#-----------------------------------------------------------------------------

gwet_output_description <- data.frame('pa' = 'the raw percentage of agreement',
                                      'pe' = 'the expected percentage of agreement',
                                      'coeff.val' = 'the estimated value of Gwet"s AC',
                                      'coeff.se' = 'the corresponding standard error',
                                      'conf.int' = 'lower and upper bound of the 95% confidence interval.',
                                      'p.value' = 'the corresponding p value',
                                      'w.name' = 'the chosen weighting sceme')

gwet_docum_text <-
"Gwet's Agreement Coefficient (AC) is an extentsion to Cohen's Kappa
and tries to overcome its limitations ..."

#-----------------------------------------------------------------------------
#'*--------------------------- ENTROPY INDEX --------------------------------*
#-----------------------------------------------------------------------------

entropy_output_description <- data.frame('e.index' = 'value of the double entropy index',
                                         'lb and ub' = 'lower and upper bound of the 95% confidence interval')

entropy_docum_text <-
"The Double Entropy Index ..."

#-----------------------------------------------------------------------------
#'*----------------------------- EYE KAPPA ----------------------------------*
#-----------------------------------------------------------------------------

eye_output_description <- data.frame('est' = 'value of von eye"s kappa')

eye_docum_text <-
"Von Eye's Kappa (Ks) war developed to overcome ..."

#-----------------------------------------------------------------------------
#'*---------------------------- AD INDEX ------------------------------------*
#-----------------------------------------------------------------------------

ad_output_description <- data.frame('d' = 'value of the d coefficient',
                                    'd.crit' = 'critical value of d coefficient',
                                    'sign.' = paste0('true if d is significant, false otherwise',
                                                     ' (significant agreement exists if d is greater than d.crit)'))

ad_docum_text <-
"The Agreement Coefficient d (ad-index) uses squared Euclidean distances to ..."

#-----------------------------------------------------------------------------
#'*----------------------------- A KAPPA ------------------------------------*
#-----------------------------------------------------------------------------

akappa_output_description <- data.frame('lb and ub' = 'lower and upper bound of the 95% confidence interval')

akappa_docum_text <-
"A-Kappa was developed by ... to ..."

#-----------------------------------------------------------------------------
#'*------------------------ FREE KAPPA CARPENTIER ---------------------------*
#-----------------------------------------------------------------------------

freeKappa_output_description <- data.frame('lb and ub' = 'lower and upper bound of the 95% confidence interval')

freeKappa_docum_text <-
"Caprentier (2017) introduced a modified version of Cohen's Kappa for ..."

#-----------------------------------------------------------------------------
#'*------------------------ INFORMATION AGREEMENT ---------------------------*
#-----------------------------------------------------------------------------

infoAgree_output_description <- data.frame()

infoAgree_docum_text <- 
"The Information Agreement measure (IA) similar to the entropy index uses ..."

#-----------------------------------------------------------------------------
#'*------------------------------- OCCC -------------------------------------*
#-----------------------------------------------------------------------------

occc_output_description <- data.frame('oprec' = 'overal precision',
                                      'oaccu' = 'overal accurarcy',
                                      'lb and ub' = 'lower and upper bound of the 95% confidence interval',
                                      'second table' = 'pairwise precision, accuracy, scale shift and location shift')

occc_docum_text <- 
"The Concordance Correlation Coefficient was developed by..."

#-----------------------------------------------------------------------------
#'*------------------------- KRIPPENDORFF ALPHA -----------------------------*
#-----------------------------------------------------------------------------

kripp1_output_description <- data.frame('lb and ub' = 'lower and upper bound of the 95% confidence interval')

kripp_docum_text <- "Krippendorff's Alpha is a method for calculating agreement among several raters and 
rated objects. This measure is suited for all scale levels - nominal, ordinal, interval and ratio scale data. Below you can upload your data, choose a scale level and press calculate. For more detailed results
besides the one shown immediately in the output section, you can download a .xlsx excel spreadsheet
containing all output given by the function used in this application."

#-----------------------------------------------------------------------------
#'*------------------------ DECISION TREE TEXT ------------------------------*
#-----------------------------------------------------------------------------

nomTwoNames <- c("Cohen's Kappa",
                 "Scott's Pi",
                 "Krippendorff's Alpha",
                 "Gwet's AC1",
                 "Brennan-Prediger K",
                 "Bangdiwala's B",
                 "Carpentier's Kappa",
                 "ICC")

nomTwoVals <- c('kappa_cohen',
                'kappa_pi',
                'other_kripp',
                'kappa_gwet',
                'kappa_brennan',
                'other_bangdiwala',
                'kappa_free',
                'other_icc')

nomMoreNames <- c("Fleiss' Kappa",
                  "Conger's Kappa",
                  "Gwet's AC1",
                  "Brennan-Prediger K",
                  "Krippendorff's Alpha",
                  "Double Entropy Index",
                  "Information Agreement",
                  "Von Eye's Kappa",
                  "Ad-index",
                  "Byrt's Kappa",
                  "Iota",
                  "Sklar's Omega",
                  "A-Kappa",
                  "ICC")

nomMoreVals <- c('kappa_fleiss',
                 'kappa_conger',
                 'kappa_gwet',
                 'kappa_brennan',
                 'other_kripp',
                 'other_entropy',
                 'other_infoAgree',
                 'kappa_eye',
                 'other_ad',
                 'kappa_byrt',
                 'other_iota',
                 'other_sklar',
                 'kappa_akappa',
                 'other_icc')

ordTwoNames <- c("Cohen's Kappa",
                 "Scott's Pi",
                 "Krippendorff's Alpha",
                 "Gwet's AC2",
                 "Brennan-Prediger K",
                 "Rwg",
                 "Awg",
                 "ICC"
)

ordTwoVals <- c('kappa_cohen',
                'kappa_pi',
                'other_kripp',
                'kappa_gwet',
                'kappa_brennan',
                'other_rwg',
                'other_awg',
                'other_icc')
  
ordMoreNames <- c("Fleiss' Kappa",
                  "Conger's Kappa",
                  "Gwet's AC2",
                  "Brennan-Prediger K",
                  "Krippendorff's Alpha",
                  "Rwg(j)",
                  "Awg(j)",
                  "Double Entropy Index",
                  "Information Agreement",
                  "Ad-index",
                  "Byrt's Kappa",
                  "Iota",
                  "Sklar's Omega",
                  "A-Kappa",
                  "ICC"
)

ordMoreVals <- c('kappa_fleiss',
                 'kappa_conger',
                 'kappa_gwet',
                 'kappa_brennan',
                 'other_kripp',
                 'other_rwg',
                 'other_awg',
                 'other_entropy',
                 'other_infoAgree',
                 'other_ad',
                 'kappa_byrt',
                 'other_iota',
                 'other_sklar',
                 'kappa_akappa',
                 'other_icc')

interTwoNames <- c("Cohen's Kappa",
                   "Scott's Pi",
                   "Krippendorff's Alpha",
                   "Gwet's AC2",
                   "Brennan-Prediger K",
                   "Aickin's Alpha",
                   "Lin's CCC",
                   "Adjusted Rand Index",
                   "ICC"
)

interTwoVals <- c('kappa_cohen',
                  'kappa_pi',
                  'other_kripp',
                  'kappa_gwet',
                  'kappa_brennan',
                  'other_aickin',
                  'other_ccc',
                  'other_randIndex',
                  'other_icc')
  
interMoreNames <- c("Fleiss' Kappa",
                    "Conger's Kappa",
                    "Gwet's AC2",
                    "Brennan-Prediger K",
                    "Krippendorff's Alpha",
                    "Iota",
                    "Sklar's Omega",
                    "OCCC",
                    "ICC")


interMoreVals <- c('kappa_fleiss',
                   'kappa_conger',
                   'kappa_gwet',
                   'kappa_brennan',
                   'other_kripp',
                   'other_iota',
                   'other_sklar',
                   'other_ccc',
                   'other_icc')
  
ratioTwoNames <- c("Cohen's Kappa",
                   "Scott's Pi",
                   "Krippendorff's Alpha",
                   "Gwet's AC2",
                   "Brennan-Prediger K",
                   "Aickin's Alpha",
                   "ICC")
  
ratioTwoVals <- c('kappa_cohen',
                  'kappa_pi',
                  'other_kripp',
                  'kappa_gwet',
                  'kappa_brennan',
                  'other_aickin',
                  'other_icc')
  
ratioMoreNames <- c("Fleiss' Kappa",
                    "Conger's Kappa",
                    "Gwet's AC2",
                    "Brennan-Prediger K",
                    "Krippendorff's Alpha",
                    "Sklar's Omega",
                    "ICC")
  
ratioMoreVals <- c('kappa_fleiss',
                   'kappa_conger',
                   'kappa_gwet',
                   'kappa_brennan',
                   'other_kripp',
                   'other_sklar',
                   'other_icc')

resetDivList <- c('nRaters',
                  'nominal two', 'nominal more',
                  'ordinal two', 'ordinal more',
                  'interval two', 'interval more',
                  'ratio two', 'ratio more')

resetBoxList <- c('raterGroup',
                  'nomTwo', 'nomMore',
                  'ordTwo', 'ordMore',
                  'interTwo', 'interMore',
                  'ratoTwo', 'ratioMore')