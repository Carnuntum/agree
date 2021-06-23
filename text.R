

docum_text <- div(class = 'description_text_style',
  tagList(
    'This is the companion application to the paper',
    tags$i('Computing Agreement: A Practical User Guide and Shiny-R Web Application'),
    'Please cite this study when referring to this App.', tags$p(),
    
    'It was created using the R programming language with the Shiny package in tandem for 
    an easy access to several measures of agreement. No prior knowledge of programming 
    or the mathematical background of the various measures is needed in order to use this 
    application.', tags$p(),
    
    'As the field of agreement research consistently creates new approaches for the quantification
    of concordance between raters, coders, methods or other rating entities, it is apparent that 
    getting an overview about these developments is no easy task. The main goal of this app is to 
    be a central source of information about agreement measures and their computation. Furthermore, 
    we intend to give broader attention to more unknown and less frequently used measures and to 
    educate the user about their proper use.', tags$p(),
    
    'Getting started!', tags$p(),
    
    "If you already know which measure you want to use, you can directly jump to the 'method'
    section in the left sidebar. Measures are ordered according to their underlying structure 
    (Kappa-type, variance-type, information-theory-type, permutation-type and the gaussian-copula model). 
    However, if some uncertainty about the right choice of agreement is present, you can head to 
    the section 'Getting started' for an interactive decision tree which should help you 
    find a suited measure.", tags$p(),
    
    'The source code for this app can be found on Github: ',
    a("https://github.com/Carnuntum/agree", href = 'https://github.com/Carnuntum/agree'), tags$p(),
    
    "Below you can see a historic timeline of all included measures.",
    
    br(), br(),
    
    div(
      tags$details(
        tags$summary(a('Click here to see the image.')),
        div(id = 'timelineImage',
            tags$img(
              src = 'timelineByStruct.png'
            ),
            br(), br(),
            downloadButton(outputId = 'timelineDown', 
                           label = 'Download'),
            style = 'align: left; text-align:center;'
        )
      ),
      stlye = 'align: center; important!'
    )
  )
)

decTree_text <- div(class = 'description_text_style',
  tagList(
    "This interactive decision tree will help you to find a measure which best
    fits your data and needs.", tags$p(), br(), br(),
    
    tags$ol(
      tags$li("First, you can choose among four different scale levels for categorical 
      (nominal, ordinal) or metric (interval, ratio) data."), tags$hr(),
    
      tags$li("Second, you can choose if your dataset contains at most 2 or more than 2 raters."), tags$hr(),
    
      tags$li("Finally, a list of suited measures should open. By hovering over this 
      list, additional information about each measure is displayed. The last 
      comment on each measure is important for the type of agreement you want 
      to compute (category-wise or data-wise).")), 
    
    br(),tags$hr(),
    
    "Category-wise measures assume a uniform distribution of category 
    frequencies (raters should agree on each category approximately equally 
    often). Category-wise measures should be chosen if it is important that 
    all raters reliably agree on each category. If this is not the case and 
    although raters strongly agree on most categories but very poorly on the 
    others, the overall value of the measure will also be very low. ", tags$p(),
    
    "Data-wise measures assume no uniform distribution of the categories. 
    These measures should be chosen if it is important that raters agree on 
    most of the data in general. Hence, if raters strongly agree on most 
    categories but poorly on the rest, the overall value will still be high.", tags$p(),
    
    "By clicking on the name, you will be directed to the corresponding page 
    where you can upload your data and compute the measure. Additionally, 
    by clicking on the page's title, a box will open with further instruction 
    on the measure and its interpretation. ", tags$p(),
    
    "You can also have a quick overview of your data by uploading it as a .csv file below 
    (raters in columns and subjects in rows. For examples see the example data in one of the 
    measures in the tab 'methods'). Two histrograms visualize how your data is distributed. 
    If all categories are more or less evenly distributed all measures yield comparable values. 
    If some categories are more frequent than others, a desicion between category-wise and 
    data-wise is required.", tags$p(),
    
    "Generally raters should be in columns and subjects should be in rows for 
    correct display.", br()
    
    
  ))


file_upload_text <- 'On the right side you can upload your .csv file. Please carefully look at the
required structure of the uploaded file below.'

file_struct_text <- 'The data has to have the following structure:'

look_down <- div(icon("caret-down"), style = "font-size: 30px; color: #3c8dbc;")

interpretHelp <- function(range = 'zero_one') {
  
  switch(range,
         zero_one = 
           tags$ul(
             tags$li("The value varies in the interval: 0 to 1 (after Landis & Koch, 1977)"),
             tags$li("< 0 =          no agreement"),
             tags$li("0 - 0.20 =     slight agreement"),
             tags$li("0.21 - 0.40 =  fair agreement"),
             tags$li("0.41 - 0.60 =  moderate agreement"),
             tags$li("0.61 - 0.80 =  substantial agreement"),
             tags$li("0.81 - 1 =     almost perfect agreement")
           ),
         minus_one_to_one = 
           tags$ul(
             "The value varies in the interval: -1 to 1 (after Brown & Hauenstein, 2005)",
             tags$li("< 0.6 = bad agreement"),
             tags$li("0.6 - 0.70 = acceptable agreement"),
             tags$li("0.71 - 0.80 = moderate agreement"),
             tags$li("0.81 - 1 = good agreement")
           ),
         icc = 
           tags$ul(
             tags$li("The value varies in the interval -1 to 1 (after, Koo & Li, 2016):"),
             tags$li("< 0.5 = poor agreement"),
             tags$li("0.5 - 0.75 = moderate agreement"),
             tags$li("0.76 - 0.90 = good agreement"),
             tags$li("0.91 - 1 = excellent agreement")
           )
         )
}

weightDesc <-
  tags$ul(
    tags$li(tags$span('linear', style = 'color: #3c8dbc;'), '- these weights 
    have been proposed by various authors. They are mostly lower than 
    quadratic weights.'), hr(),
    
    tags$li(tags$span('quadratic', style = 'color: #3c8dbc;'), '- higher than 
    linear weights, thus partial agreements (e.g. 1-2) are less strictly 
    penalized a for instance with linear weights.'), hr(),
    
    tags$li(tags$span('ordinal', style = 'color: #3c8dbc;'), '- requires 
    categories to be ordinal (non-numeric categories have to be converted 
    to subsequent numerical values, e.g. A, B, C -> 1, 2, 3) and ranking 
    from lowest to highest must be possible. Are higher than linear and 
    slightly lower than quadratic weights. '), hr(),
    
    tags$li(tags$span('radical', style = 'color: #3c8dbc;'), '- weights are 
    even smaller than linear or quadratic weights. They can be selected 
    if partial disagreements (e.g. rater 1 chooses category 4 for subject 
    1 and rater 2 chooses category 2 for the same subject) should be penalized 
    more strictly. '), hr(),
    
    tags$li(tags$span('ratio', style = 'color: #3c8dbc;'), '- can be used 
            with ratio scaled data. Slightly lower than linear weights.'), hr(),
    
    tags$li(tags$span('circular', style = 'color: #3c8dbc;'), '- like the name 
    suggests the first and last weights tend to be close together. Recommended 
    if categories represent angles in degrees or radians. '), hr(),
    
    tags$li(tags$span('bipolar', style = 'color: #3c8dbc;'), '- these weights 
    are similar to ratio weights in the middle of the scale and similar to 
    quadratic weights at the end of the scale. '), hr(),
    
    "Note. There does not exist any predefined recommendation for different 
    weighting schemes. It is always up to the researcher to decide how much 
    and in which way partial agreement (amount of disagreement between raters) 
    should be penalized."
  )

#-----------------------------------------------------------------------------
#'*------------------------------- SCOTT PI ---------------------------------*
#-----------------------------------------------------------------------------

pi_output_description <- data.frame('method' = 'the choosen weighting sceme',
                                    'obs and sample' = 'number of raters and number of rated objects respectively',
                                    'est' = 'the estimated value of scott"s pi',
                                    'se' = 'the correspoinding standard error',
                                    'conf.level' = 'the confidence interval level of 0.95',
                                    'lb' = 'lower bound of the confidence interval',
                                    'ub' = 'upper bound of the confidence interval',
                                    check.names = F)

pi_docum_text <- div(class = 'description_text_style',
 tagList(
   "Scott's Pi was one of the first Kappa-type measures and is (like other measures 
   assessing category-wise agreement) sensitive to the distribution of frequencies. For more than 
   2 raters a generalization was first introduced by Fleiss (1971). Despite not developed 
   for ordinal or metric data, you may use \"linear\" or \"quadratic\" weights for adjusting the 
   measure for the ordered nature of higher order scale levels. For ordinal or metric data it is 
   nevertheless recommended to use measures which were developed for these scales. (use the 
   desiction tree in this case)", br(), br(),
   
   "Interpretative values (after Landis & Koch (1977):", br(), br(),

  interpretHelp()
 )
  )

#-----------------------------------------------------------------------------
#'*------------------------------- COHEN KAPPA ------------------------------*
#-----------------------------------------------------------------------------

cohen_output_description <- data.frame('method' = 'the choosen weighting sceme', 
                                       'subjects and raters' = 'the dimensions of uploaded data',
                                       'value' = paste0('the kappa value ranging from 0 (no agreement) to 1 (perfect agreement).',
                                                        'Negative values indicate less agreement than expected by chance.'), 
                                       'stat.name' = 'the corresponding test statistic', 
                                       'statistic' = 'the z value', 
                                       'p.value' = 'significance value for 0.05',
                                       check.names = F)



cohen_docum_text <- div(class = 'description_text_style',
  tagList(
    
    "Cohen's Kappa is probably the most used measure of agreement to this date. 
    It also started a debate on whether some Kappa type measures have a distinct 
    flaw or paradox (these measures yield low values despite high raw percent 
    agreement in the case of unevenly distributed category frequencies). As it 
    is discussed in the paper to this app, we do not think that these flaws exist 
    (outside definitional frames) and refer to frequency sensitive measures as 
    category-wise. These assess if raters agree on \"all\" categories equally well. 
    Originally it was developed for 2 raters, N subjects and a nominal scale level. 
    It has been generalized by Fleiss (1971) and Conger (1980). As Fleiss' version 
    does not reduce to Cohen's Kappa in the case of 2 raters, Conger's Kappa is 
    recommended here. Several weighting schemes are provided (after Gwet (2014)):",
    br(), br(),
    
    weightDesc, br(), br(),
    interpretHelp()
  )) 

#-----------------------------------------------------------------------------
#'*----------------------------- RAND INDEX ----------------------------------*
#-----------------------------------------------------------------------------

randIndex_output_description <- data.frame('est' = 'the estimated value of the Rand Index.',
                                           'lb' = 'lower bound of the 95% confidence interval',
                                           'ub' = 'upper bound of the 95% confidence interval',
                                           check.names = F)

randIntex_docum_text <- 
div(class = 'description_text_style',
    tagList(
"The adjusted rand index is a measure between 2 clusters 
or partitions and can be used to quantify the amount of agreement between 2 
raters and N subjects. Raters assign subjects or objects to different 
predefined classes (e.g. classes 1 to 3). Values range from 0 (no agreement) 
to 1 (perfect agreement) and assesses category-wise agreement. The confidence 
interval is computed using bootstrapping and should only be used for inference 
with > 100 rated subjects/objects (otherwise it may happen that the estimate 
lies outside the confidence interval)."

    ))
#-----------------------------------------------------------------------------
#'*----------------------------- CONGER KAPPA -------------------------------*
#-----------------------------------------------------------------------------

conger_output_description <- data.frame('est.pa' = 'the raw percentage of agreement',
                                        'est.pe' = 'the expected percentage of agreement',
                                        'est.coeff.val' = 'the estimated value of Conger"s Kappa',
                                        'est.coeff.se' = 'the corresponding standard error',
                                        'est.conf.int' = 'lower and upper bound of the 95% confidence interval.',
                                        'est.p.value' = 'the corresponding p value',
                                        'est.w.name' = 'the chosen weighting sceme',
                                        check.names = F)

conger_docum_text <-
div(class = 'description_text_style',
    tagList(
      "Conger's Kappa is a generalization for Cohen's Kappa and computes 
      category-wise agreement. Other than Fleiss' generalization, this measure 
      reduces to Cohen's K in the case of 2 raters. Several weighting schemes 
      are provided (after Gwet (2014)):", 
      br(), br(),
      weightDesc,
      interpretHelp()
    ))

#-----------------------------------------------------------------------------
#'*----------------------------- FLEISS KAPPA -------------------------------*
#-----------------------------------------------------------------------------

fleiss_output_description <- data.frame('f.est' = paste0('the kappa value ranging from 0 (no agreement) to 1 (perfect agreement).',
                                                         'Negative values indicate less agreement than expected by chance.'),
                                        'pa' = 'the raw percentage of agreement',
                                        'pe' = 'the expected percentage of agreement',
                                        check.names = F)

fleiss_docum_text <-
div(class = 'description_text_style',
    tagList(
      "Fleiss' Kappa is a generalization of Scott's Pi for more than 2 raters 
      and computes category-wise agreement. Several weighting schemes are 
      provided (after Gwet (2014)):",
      br(), br(),
      weightDesc,
      interpretHelp()
    ))

#-----------------------------------------------------------------------------
#'*----------------------------- BRENNAN KAPPA ------------------------------*
#-----------------------------------------------------------------------------

brennan_output_description <- data.frame('pa' = 'the raw percentage of agreement',
                                         'pe' = 'the expected percentage of agreement',
                                         'coeff.val' = 'the estimated value of Conger\'s Kappa',
                                         'coeff.se' = 'the corresponding standard error',
                                         'conf.int' = 'lower and upper bound of the 95% confidence interval.',
                                         'p.value' = 'the corresponding p value',
                                         'w.name' = 'the chosen weighting sceme',
                                         check.names = F)

brennan_docum_text <-
div(class = 'description_text_style',
    tagList(
      "Brennan and Prediger's Kappa was developed by Brennan & Prediger (1981) 
      and was rediscovered several times (Bennett et al., 1954; Holley & 
      Guilford, 1964; Janson & Vegelius, 1979; Maxwell, 1977; Randolph, 2005). 
      It computes data-wise agreement, meaning that if raters agree on most of 
      the dataset this measure yields also a high value. It is insensitive to 
      skewed category frequencies. Several weighting schemes are provided 
      (after Gwet (2014)):",
      br(), br(),
      weightDesc,
      interpretHelp()
    ))

#-----------------------------------------------------------------------------
#'*------------------------------- RWG --------------------------------------*
#-----------------------------------------------------------------------------

rwg_output_description <- data.frame('grpid' = 'list of grouping variables found in the "grpid" column of the data',
                                     'rwg' = 'estimated value of rwg for single, or rwg(j) for multiple items',
                                     'gsize' = 'group size under each grouping variable',
                                     check.names = F)

rwg_docum_text <-
div(class = 'description_text_style',
    tagList(
      "The reliability within group index was first introduced by Finn (1970) 
      and was further studied by James et al. (1984, 1993) given the name rwg. 
      Other than Kappa type, this measure uses expected and observed variance 
      of the data for computation. By supplying a group variable, distinct 
      group values can be computed. Thus, not every rater in the dataset must 
      have rated the same and the same number of subjects, respectively. The 
      overall measure is then an average of all groupwise values. A group 
      variable must be supplied nevertheless. Several downsides are present 
      First, the measure is scale dependent, meaning that 2 values with 
      different scales (e.g., 1-7 and 1-5) cannot be compared. Second, the 
      measure is sample size dependent, meaning the value gets bigger with a 
      growing sample size. It is very sensitive to unbalanced category frequencies.",
      br(), br(),
      interpretHelp(range = 'minus_one_to_one')
    ))

#-----------------------------------------------------------------------------
#'*------------------------------- AWG --------------------------------------*
#-----------------------------------------------------------------------------

awg_output_description <- data.frame('grpid' = 'list of grouping variables found in the "grpid" column of the data',
                                     'a.wg' = 'estimated value of awg or awg(j) for multiple items',
                                     'nitems' = 'number of items in data',
                                     'nraters' = 'number of raters in data',
                                     'avg.grp.var' = 'average group variance',
                                     'mean' = 'average rating in the data',
                                     'mean interval' = 'if mean is outside this interval, awg is not interpretable',
                                     check.names = F)

awg_docum_text <-
  div(class = 'description_text_style',
      tagList(
        "The agreement within group index was developed by Brown & Hauenstein 
        (2005) as an alternative to the rwg with fewer limitations. This measure 
        is not sensitive to category frequencies and is not dependent on scales 
        and sample size. It has nevertheless two major disadvantages. First, 
        for interpretable outcomes N - 1 raters are required (N = number of 
        categories). Second, if the data mean falls outside specific extremes, 
        interpretation of awg is not possible. ",
        br(),br(),
        interpretHelp(range = 'minus_one_to_one')
      ))

#-----------------------------------------------------------------------------
#'*-------------------------- BANGDIWALA B ----------------------------------*
#-----------------------------------------------------------------------------

bangdiwala_output_description <- data.frame('Bangdiwala' = 'estimated value',
                                            check.names = F)

bangdiwala_docum_text <-
div(class = 'description_text_style', 
           tagList(
            "Bangdiwala's B combines a data-wise statistic with the computation of a visual 
            representation of agreement in the data. The agreement plot show cell frequencies 
            as shaded rectanges. Agreement can be seen as the proportion of shaded areas comapred 
            to the overal area in the plot. Several weighting schemes are provided 
            (after Gwet (2014)):",
            br(),br(),
            weightDesc,
            interpretHelp()
           ))

#-----------------------------------------------------------------------------
#'*--------------------------- AICKIN ALPHA ---------------------------------*
#-----------------------------------------------------------------------------

aickin_output_description <- data.frame('lcl and ucl' = 'lower and upper level 
                                        of the 95% confidence interval',
                                        check.names = F)

aickin_docum_text <-
  div(class = 'description_text_style',
      tagList(
        "Being very similar to Gwet's AC (Gwet, 2008), this measure assesses 
        the portion of hard-to-rate subjects (for which ratings are given in 
        a random manner) compared to the number of overall subjects. Unlike 
        Gwet's measure, Aickin's alpha can hardly reach its maximum value of 
        1, even when agreement is substantially high. Furthermore, it relies 
        on iterative estimation, which is computationally costly for large 
        datasets.",
        br(), br(),
        interpretHelp()
      ))


#-----------------------------------------------------------------------------
#'*---------------------------- BYRT KAPPA ----------------------------------*
#-----------------------------------------------------------------------------

byrt_output_description <- data.frame('po' = 'observed raw percentage agreement',
                                      'byrt' = 'value of byrts kappa',
                                      'lb and ub' = 'lower and upper bound of the 95% confidence interval',
                                      check.names = F)

byrt_docum_text <-
  div(class = 'description_text_style',
      tagList(
        "Byrt's Kappa (also called PABAK for prevalence adjusted, bias adjusted 
        kappa) was on of the first data-wise measures. For 2 raters it is 
        identical to Brennan & Prediger's KBP (1981). For more than 2 raters 
        Byrt's Kappa value is mostly lower than that of KBP, due to its more 
        agressive correction for chance.",
        br(), br(),
        interpretHelp()
      ))


#-----------------------------------------------------------------------------
#'*------------------------------ IOTA --------------------------------------*
#-----------------------------------------------------------------------------

iota_output_description <- data.frame('method' = 'number of variables in dataset',
                                      'subjects and raters' = 'the dimensions of uploaded data',
                                      'value' = 'estimated iota value',
                                      'lb' = 'lower bound of the 95% bootstrap confidence interval',
                                      'ub' = 'upper bound of the  95% bootstrapconfidence interval',
                                      check.names = F)

iota_docum_text <-
  div(class = 'description_text_style',
      tagList(
        "Iota was developed in the case of multiple rating variables (e.g., 
        raters assign children school and behavioral grades at the same time) 
        and computes category-wise agreement. It can be used for interval or 
        nominal scaled data.",
        br(), br(),
        interpretHelp()
      ))


#-----------------------------------------------------------------------------
#'*---------------------------- GWET AC1/2 ----------------------------------*
#-----------------------------------------------------------------------------

gwet_output_description <- data.frame('pa' = 'the raw percentage of agreement',
                                      'pe' = 'the expected percentage of agreement',
                                      'coeff.val' = 'the estimated value of Gwet"s AC',
                                      'coeff.se' = 'the corresponding standard error',
                                      'conf.int' = 'lower and upper bound of the 95% confidence interval.',
                                      'p.value' = 'the corresponding p value',
                                      'w.name' = 'the chosen weighting sceme',
                                      check.names = F)

gwet_docum_text <-
  div(class = 'description_text_style',
      tagList(
        "Similar to Aickin's alpha (1990) and Brennan & Prediger's KBP (1981), 
        Gwet's AC computes data-wise agreement. Several weighting schemes are 
        provided (after Gwet, 2014):",
        br(), br(),
        weightDesc,
        interpretHelp()
      ))


#-----------------------------------------------------------------------------
#'*--------------------------- ENTROPY INDEX --------------------------------*
#-----------------------------------------------------------------------------

entropy_output_description <- data.frame('e.index' = 'value of the double entropy index',
                                         'lb and ub' = 'lower and upper bound of the 95% boostrap confidence interval',
                                         check.names = F)

entropy_docum_text <-
  div(class = 'description_text_style',
      tagList(
        "This measure uses the Shannon entropy equation for quantifying the 
        uncertainty and complexity in the data. An advantage over rwg and awg 
        is, that this measure steadily increases with increasing agreement. In 
        the case of increasing agreement rwg and awg can yield values which do 
        no reflect this fact.",
        br(), br(),
        interpretHelp()
      ))


#-----------------------------------------------------------------------------
#'*----------------------------- EYE KAPPA ----------------------------------*
#-----------------------------------------------------------------------------

eye_output_description <- data.frame('est' = 'value of von eye\'s kappa',
                                     'p.value' = 'significance for level 0.05',
                                     check.names = F)

eye_docum_text <-
  div(class = 'description_text_style',
      tagList(
        "This measure was introduced as an alternative to Cohen's Kappa. It 
        also computes category-wise agreement but is slightly higher than 
        Cohen's Kappa. ",
        br(), br(),
        interpretHelp()
      ))


#-----------------------------------------------------------------------------
#'*---------------------------- AD INDEX ------------------------------------*
#-----------------------------------------------------------------------------

ad_output_description <- data.frame('d' = 'value of the d coefficient',
                                    'd.crit' = 'critical value of d coefficient',
                                    'sign.' = paste0('true if d is significant, false otherwise',
                                                     ' (significant agreement exists if d is greater than d.crit)'),
                                    check.names = F)

ad_docum_text <-
  div(class = 'description_text_style',
      tagList(
        "The agreement coefficient d was developed as an alternative to rwg 
        and awg and computes data-wise agreement. An advantage of this measure
        is that it is interval scaled (the difference between 2 d values is 
        considered the same, same number of raters, items and scale points 
        assumed) and can be compared between different studies and samples.",
        br(), br(),
        interpretHelp()
      ))


#-----------------------------------------------------------------------------
#'*----------------------------- A KAPPA ------------------------------------*
#-----------------------------------------------------------------------------

akappa_output_description <- data.frame('lb and ub' = 'lower and upper bound of the 95% confidence interval',
                                        check.names = F)

akappa_docum_text <-
  div(class = 'description_text_style',
      tagList(
        "A-Kappa (AK) computes data-wise agreement and in the case of more 
        than 1 dataset, different AK's can be combined to describe the overall 
        data by the weighted average of all separate AK's. For example if N1 is 
        the sample size of the first dataset and N2 is the size of the second 
        then the overall AK would be ((N1 x AK1)/(N1 + N2)) + ((N2 x AK2)/(N1 + 
        N2)) and so on.",
        br(), br(),
        interpretHelp()
      ))


#-----------------------------------------------------------------------------
#'*------------------------ FREE KAPPA CARPENTIER ---------------------------*
#-----------------------------------------------------------------------------

freeKappa_output_description <- data.frame('lb and ub' = 'lower and upper bound of the 95% confidence interval',
                                           check.names = F)

freeKappa_docum_text <-
  div(class = 'description_text_style',
      tagList(
        "Introduced as a modified version of Cohen's Kappa, this measure 
        reflects the fact that a subject can contribute to more than one 
        observation (e.g., the number of lesions in the brain of a person) 
        and computes category-wise agreement. A limit of this measure is that 
        it was only introduced for binary ratings and 2 raters.",
        br(), br(),
        interpretHelp()
      ))


#-----------------------------------------------------------------------------
#'*------------------------------- OEST KAPPA -------------------------------*
#-----------------------------------------------------------------------------

oest_output_description <- data.frame('lb and ub' = 'lower and upper bound of the 95% confidence interval',
                                      check.names = F)

oest_docum_text <- 
  div(class = 'description_text_style',
      tagList(
        "This measure, again as an alternative to the original Kappa, uses a 
        Bayesian approach to correct for chance. It computes category-wise 
        agreement and yields slightly higher and in the case of highly skewed 
        category frequencies substantially higher values than Cohen's K.",
        br(), br(),
        interpretHelp()
      ))


#-----------------------------------------------------------------------------
#'*------------------------ INFORMATION AGREEMENT ---------------------------*
#-----------------------------------------------------------------------------

infoAgree_output_description <- data.frame('ia' = 'estimate of the information agreement measure',
                                           'lb and ub' = 'the lower and upper bound of the 95% boostrap confidence interval',
                                           check.names = F)

infoAgree_docum_text <- 
  div(class = 'description_text_style',
      tagList(
        "Similar to the double entropy index (DEI), this measure uses the Shannon 
        entropy equation for computing category-wise agreement for 2 raters. 
        The advantage, similar to the DEI, is that agreement increases with a 
        steadily increasing amount of agreement.",
        br(), br(),
        interpretHelp()
      ))


#-----------------------------------------------------------------------------
#'*--------------------------------- ICC ------------------------------------*
#-----------------------------------------------------------------------------

icc_output_description <- data.frame('subjects and raters' = 'the dimensions of the data',
                                     'model' = 'the chosen ICC model',
                                     'type' = 'only absolute agreement is computed',
                                     'unit' = 'the chosen characteristics of mesurements',
                                     'icc.name' = 'the computed ICC with the naming convetion after McGraw & Wong (1996)',
                                     'value' = 'the ICC estimate',
                                     'f.statistic' = 'the corresponding F value',
                                     'df.1 and df.2' = 'numerator and denominator degrees of freedom',
                                     'p.value' = 'the corresponding p-value',
                                     'lower and upper ci' = 'lower and upper 95% confidence interval',
                                     check.names = F
                                     )

icc_docum_text <- 
  div(class = 'description_text_style',
      tagList(
        "This was one of the first measures for quantifying category-wise 
        agreement dating at least back to Harris (1913). Only ICCs measuring 
        absolute agreement (in contrast consistency) are available here. 
        Uploaded data must satisfy the assumptions for an analysis of variance 
        (ANOVA).",
        br(), br(),
        
        tags$ul(
          tags$li("If the subjects in the data are sampled from a greater 
                  population and are considered replaceable, \"one-way\" 
                  should be chosen."), hr(),
          tags$li("If both subjects and raters are considered replacable, 
                  \"two-way\" should be chosen."), hr(),
          tags$li("If ratings represent single measurements (e.g., a single 
                  value for the bodyweight of a person), \"single\" should be 
                  chosen. If ratings represent an average over several values 
                  (e.g., a summary score for several testitems for each 
                  person), \"average\" should be used. "), hr()
        ),
        
        "Only if raters are random, the ICC can be generalized to a wider 
        population of raters, otherwise the ICC is only interpretable in the 
        context of the fixed rater group used for the generation of your data.",
        
        br(), br(),
        interpretHelp(range = 'icc')
      ))


#-----------------------------------------------------------------------------
#'*------------------------------- OCCC -------------------------------------*
#-----------------------------------------------------------------------------

occc_output_description <- data.frame('oprec' = 'overal precision',
                                      'oaccu' = 'overal accurarcy',
                                      'lb and ub' = 'lower and upper bound of the 95% confidence interval',
                                      'second table' = 'pairwise precision, accuracy, scale shift and location shift',
                                      check.names = F)

occc_docum_text <- 
  div(class = 'description_text_style',
      tagList(
        "This measure as an alternative to the intraclass correlation, has the 
        advantage that it does not rely on the assumptions of an analysis of 
        variance (ANOVA) and both ICC and CCC yield similar values if 
        assumptions are met. The CCC measures the degree to which measurements 
        tend to lie on the 45 degree line of equality.  In the case of 2 raters the 
        Bland Altman plot is displayed (Altman & Bland, 1983). In addition to 
        the overall statistics of the CCC, all pairwise precision (pearson 
        correlation), accuracy (how far the line of best fit deviates 
        from the line through the origin), scale- and location-shift values 
        are displayed. In addition to the overall statistics of the CCC, all 
        pairwise precision (pearson correlation), accuracy (how far the line 
        of best fit line deviation from the line through the origin), scale- 
        and location-shift values are displayed. Increasing deviation from 1 
        and 0 for the scale- and location-shift respectively, leads to 
        decreased values of the CCC.",
        br(), br(),
        interpretHelp(range = 'icc')
      ))


#-----------------------------------------------------------------------------
#'*------------------------- KRIPPENDORFF ALPHA -----------------------------*
#-----------------------------------------------------------------------------

kripp_output_description <- data.frame('lb and ub' = 'lower and upper bound of the 95% confidence interval',
                                       check.names = F)

kripp_docum_text <-
  div(class = 'description_text_style',
      tagList(
        "As a very versatile category-wise measure, krippendorff's alpha is 
        suited for all scale levels – categorical and metric. It is mostly 
        very similar to Scott's Pi for 2 and Fleiss' Kappa for more than 2 
        raters. As one of its advantages it can deal with missing values 
        without removing subjects containing them.",
        br(), br(),
        interpretHelp()
      ))



#-----------------------------------------------------------------------------
#'*------------------------------ SKLARS OMEGA ------------------------------*
#-----------------------------------------------------------------------------

omega_output_description <- data.frame('coefficients' = 'sklar\'s omega estimate',
                                       'confint' = 'the method for computing the confidence interval',
                                       'lci and uci' = 'the lower and upper 95% boostrap confidence interval',
                                       check.names = F)

omega_docum_text <-
  div(class = 'description_text_style',
      tagList(
        "Compared to Krippendorff's alpha this measure yields more robust 
        values, especially in the case of non-normal distributed datasets 
        (by choosing one of the supplied bootstrap options in the menu below). 
        Datasets have to be structured like shown below, where \"c.10.1\" 
        denotes the first score of coder/rater 10.", tags$p(),
        "Note. As the boostrap computation for Sklar's Omega is very heavy, 
        bootstrap iterations have been set to 10 (this is very low, but otherwise 
        computation would take too long).",
        br(), br(),
        interpretHelp()
      ))



#-----------------------------------------------------------------------------
#'*------------------------ DECISION TREE TEXT ------------------------------*
#-----------------------------------------------------------------------------

nomTwoNames <- c("Cohen's Kappa",
                 "Scott's Pi",
                 "Byrt's Kappa",
                 "Krippendorff's Alpha",
                 "Von Eye's Kappa",
                 "Gwet's AC1",
                 "Brennan-Prediger K",
                 "Double Entropy Index",
                 "Information Agreement",
                 "Bangdiwala's B",
                 "Carpentier's Kappa",
                 "Van Oest's Kappa",
                 "ICC")

nomTwoVals <- c('kappa_cohen',
                'kappa_pi',
                'kappa_byrt',
                'other_kripp',
                'kappa_eye',
                'kappa_gwet',
                'kappa_brennan',
                'other_entropy',
                'other_infoAgree',
                'other_bangdiwala',
                'kappa_free',
                'kappa_oest',
                'other_icc')

nomTwoDesc <- data.frame(
  "Cohen's Kappa (KC)" = "Most frequently used measure. Sensitive on marginal 
  distribution and category frequencies. Assesses category-wise agreement",
  
  "Scott's Pi" = "Same sensitivities as KC. Assesses category-wise agreement",
  
  "Byrt's Kappa" = "Developed as a data-wise alternative to Cohen's Kappa. For 2 raters
  it is identical to Brennan & Predigers Kappa. For more than 2 raters it is smaller.",
  
  "Krippendorff's Alpha" = "Very similar to Scott's Pi. Same sensitivities as 
  KC. Assesses category-wise agreement.",
  
  "Gwet's AC1" = "Not sensitive against unbalanced marginal distributions or 
  frequencies. Assesses data-wise agreement.",
  
  "Brennan-Prediger K" = "Not sensitive against unbalanced marginal 
  distributions or category frequencies. Assesses data-wise agreement.",
  
  "Double Entropy Index" = "Uses shannon's entropy equation for quantifying uncertainty. 
  Steadily increases with increasing agreement. Assesses data-wise agreement.",
  
  "Information Agreement" = "Also uses shannon's entropy equation and increases steadily
  with increasing agreement. Assesses data-wise agreement.",
  
  "Bangdiwala's B" = "Not sensitive against unbalanced marginal distributions 
  or category frequencies. Assesses data-wise agreement. Creates agreement plot 
  for visual exploration of concordance.",
  
  "Carpentier's Kappa" = "Same sensitivities as KC. Assesses category-wise agreement",
  
  "Van Oest's Kappa" = "Using Bayesian correction for chance, this measure yields 
  slightly higher values than Cohen's Kappa. Assesses category-wise agreement.",
  
  "ICC" = "Can be used for nominal data but less recommended. Has similar 
  sensitivities as KC. Assesses category-wise agreement.",
  
  check.names = F
)

nomMoreNames <- c("Fleiss' Kappa",
                  "Conger's Kappa",
                  "Gwet's AC1",
                  "Brennan-Prediger K",
                  "Krippendorff's Alpha",
                  "Double Entropy Index",
                  "Ad-index",
                  "Byrt's Kappa",
                  "Iota",
                  "Sklar's Omega",
                  "A-Kappa",
                  "Van Oest's Kappa",
                  "ICC")

nomMoreVals <- c('kappa_fleiss',
                 'kappa_conger',
                 'kappa_gwet',
                 'kappa_brennan',
                 'other_kripp',
                 'other_entropy',
                 'other_ad',
                 'kappa_byrt',
                 'other_iota',
                 'other_omega',
                 'kappa_akappa',
                 'kappa_oest',
                 'other_icc')

nomMoreDesc <- data.frame(
  "Fleiss' Kappa" = "Generalization of Cohen's Kappa for more than 2 raters. 
  Sensitive on marginal distribution and category frequencies. 
  Assesses category-wise agreement",
  
  "Conger's Kappa" = "Same sensitivities as KC. Assesses category-wise agreement",
  
  "Gwet's AC1" = "Not sensitive against unbalanced marginal distributions or 
  category frequencies. Assesses data-wise agreement.",
  
  "Brennan-Prediger K" = "Not sensitive against unbalanced marginal 
  distributions or category frequencies. Assesses data-wise agreement.",
  
  "Krippendorff's Alpha" = "Very similar to Scott's Pi. Assesses category-wise agreement.",
  
  "Double Entropy Index" = "Uses shannon's entropy equation for quantifying uncertainty. 
  Steadily increases with increasing agreement. Assesses data-wise agreement.",
  
  "Ad Index" = "Alternative to rwg and awg. Computed values are interval scaled 
  (e.g., differences of 0.5 between two \"ad\" values are the same). Assesses data-wise agreement",
  
  "Byrt's Kappa" = "Developed as a data-wise alternative to Cohen's Kappa. For 2 raters
  it is identical to Brennan & Predigers Kappa. For more than 2 raters it is smaller.",
  
  "Iota" = "Can be used for multivariate datasets (when raters assigned mora than 
  1 rating variable to the same objects). Assesses category-wise agreement.",
  
  "Sklar's Omega" = "Very versatile, model based approach for agreement. Similar to 
  Krippendorff's Alpha but performs better under extreme conditions and non-normally 
  distributed data. Assesses category-wise agreement.",
  
  "A-Kappa" = "Has the advantage that different values from different studies 
  can be combined using a weighted average, which then describes the overall agreement. 
  Assesses data-wise agreement.",
  
  "Van Oest's Kappa" = "Using Bayesian correction for chance, this measure yields 
  slightly higher values than Cohen's Kappa. Assesses category-wise agreement.",
  
  "ICC" = "Can be used for nominal data but less recommended. Has similar 
  sensitivities as KC. Assesses category-wise agreement.",
  check.names = F
)

ordTwoNames <- c("Cohen's Kappa",
                 "Scott's Pi",
                 "Krippendorff's Alpha",
                 "Gwet's AC2",
                 "Brennan-Prediger K",
                 "Information Agreement",
                 "Rwg",
                 "Awg",
                 "ICC"
)

ordTwoVals <- c('kappa_cohen',
                'kappa_pi',
                'other_kripp',
                'kappa_gwet',
                'kappa_brennan',
                'other_infoAgree',
                'other_rwg',
                'other_awg',
                'other_icc')

ordTwoDesc <- data.frame(
  "Cohen's Kappa (KC)" = "Most frequently used measure. Sensitive on marginal 
  distribution and category frequencies. Assesses category-wise agreement",
  
  "Scott's Pi" = "Same sensitivities as KC. Assesses category-wise agreement",
  
  "Krippendorff's Alpha" = "Very similar to Scott's Pi. Same sensitivities as 
  KC. Assesses category-wise agreement.",
  
  "Gwet's AC1" = "Not sensitive against unbalanced marginal distributions or 
  frequencies. Assesses data-wise agreement.",
  
  "Brennan-Prediger K" = "Not sensitive against unbalanced marginal 
  distributions or category frequencies. Assesses data-wise agreement.",
  
  "Information Agreement" = "Also uses shannon's entropy equation and increases steadily
  with increasing agreement. Assesses data-wise agreement.",
  
  "Rwg" = "Has the advantage that group-wise coefficient are calculated, given 
  a grouping variable . Not every rater must rate the same number and same 
  subjects respectively. Is sample-size sensitive. Assesses category-wise agreement",
  
  "Awg" = "Alternative to Rwg with less limitations, but needs at least N-1 raters 
  (N = number of categories) and has interpretability limits. Assesses category-wise 
  agreement",
  
  "ICC" = "Can be used for ordinal data but less recommended. Has similar 
  sensitivities as KC. Assesses category-wise agreement.",
  
  check.names = F
)
  
ordMoreNames <- c("Fleiss' Kappa",
                  "Conger's Kappa",
                  "Gwet's AC2",
                  "Brennan-Prediger K",
                  "Krippendorff's Alpha",
                  "Rwg(j)",
                  "Awg(j)",
                  "Double Entropy Index",
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
                 'other_ad',
                 'kappa_byrt',
                 'other_iota',
                 'other_omega',
                 'kappa_akappa',
                 'other_icc')

ordMoreDesc <- data.frame(
  "Fleiss' Kappa" = "Generalization of Cohen's Kappa for more than 2 raters. 
  Sensitive on marginal distribution and category frequencies. 
  Assesses category-wise agreement",
  
  "Conger's Kappa" = "Same sensitivities as KC. Assesses category-wise agreement",
  
  "Gwet's AC1" = "Not sensitive against unbalanced marginal distributions or 
  frequencies. Assesses data-wise agreement.",
  
  "Brennan-Prediger K" = "Not sensitive against unbalanced marginal 
  distributions or category frequencies. Assesses data-wise agreement.",
  
  "Krippendorff's Alpha" = "Very similar to Scott's Pi. Same sensitivities as 
  KC. Assesses category-wise agreement.",
  
  "Rwg(j)" = "Has the advantage that group-wise coefficient are calculated, given 
  a grouping variable . Not every rater must rate the same number and same 
  subjects respectively. Is sample-size sensitive. Assesses category-wise agreement",
  
  "Awg(j)" = "Alternative to Rwg with less limitations, but needs at least N-1 raters 
  (N = number of categories) and has interpretability limits. Assesses category-wise 
  agreement",
  
  "Double Entropy Index" = "Uses shannon's entropy equation for quantifying uncertainty. 
  Steadily increases with increasing agreement. Assesses data-wise agreement.",
  
  "Ad Index" = "Alternative to rwg and awg. Computed values are interval scaled 
  (e.g., differences of 0.5 between two \"ad\" values are the same). Assesses data-wise agreement",
  
  "Byrt's Kappa" = "Developed as a data-wise alternative to Cohen's Kappa. For 2 raters
  it is identical to Brennan & Predigers Kappa. For more than 2 raters it is smaller.",
  
  "Iota" = "Can be used for multivariate datasets (when raters assigned mora than 
  1 rating variable to the same objects). Assesses category-wise agreement.",
  
  "Sklar's Omega" = "Very versatile, model based approach for agreement. Similar to 
  Krippendorff's Alpha but performs better under extreme conditions and non-normally 
  distributed data. Assesses category-wise agreement.",
  
  "A-Kappa" = "Has the advantage that different values from different studies 
  can be combined using a weighted average, which then describes the overall agreement. 
  Assesses data-wise agreement.",
  
  "ICC" = "Can be used for ordinal data but less recommended. Has similar 
  sensitivities as KC. Assesses category-wise agreement.",
  
  check.names = F
)

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
                  'kappa_aickin',
                  'other_occc',
                  'other_randIndex',
                  'other_icc')

interTwoDesc <- data.frame(
  "Cohen's Kappa (KC)" = "Most frequently used measure. Sensitive on marginal 
  distribution and category frequencies. Assesses category-wise agreement",
  
  "Scott's Pi" = "Same sensitivities as KC. Assesses category-wise agreement",
  
  "Krippendorff's Alpha" = "Very similar to Scott's Pi. Same sensitivities as 
  KC. Assesses category-wise agreement.",
  
  "Gwet's AC1" = "Not sensitive against unbalanced marginal distributions or 
  frequencies. Assesses data-wise agreement.",
  
  "Brennan-Prediger K" = "Not sensitive against unbalanced marginal 
  distributions or category frequencies. Assesses data-wise agreement.",
  
  "Aickin's Alpha" = "Very similar to Gwet's AC but it can hardly reach the maximum 
  value of 1. Assesses data-wise agreement.",
  
  "Lin's CCC" = "Was developed for metric data as an alternative to the ICC. 
  In addition to an overall value, precision (pearson correlation) and accuracy 
  (how far the line of best fit deviates from the line through the origin) 
  together with scale- and location-shift values are reported. Assesses category-wise 
  agreement",
  
  "Adjusted Rand Index" = "Developed for the computation between 2 data clusters. 
  Ratings have to be given on predefined integer values. Assesses category-wise agreement.",
  
  "ICC" = "Has similar sensitivities as KC. Assesses category-wise agreement.",
  
  check.names = F
)
  
interMoreNames <- c("Fleiss' Kappa",
                    "Conger's Kappa",
                    "Gwet's AC2",
                    "Brennan-Prediger K",
                    "Krippendorff's Alpha",
                    "Iota",
                    "Sklar's Omega",
                    "Lin's CCC",
                    "ICC")


interMoreVals <- c('kappa_fleiss',
                   'kappa_conger',
                   'kappa_gwet',
                   'kappa_brennan',
                   'other_kripp',
                   'other_iota',
                   'other_omega',
                   'other_occc',
                   'other_icc')

interMoreDesc <- data.frame(
  "Fleiss' Kappa" = "Generalization of Cohen's Kappa for more than 2 raters. 
  Sensitive on marginal distribution and category frequencies. 
  Assesses category-wise agreement",
  
  "Conger's Kappa" = "Same sensitivities as KC. Assesses category-wise agreement",
  
  "Gwet's AC1" = "Not sensitive against unbalanced marginal distributions or 
  frequencies. Assesses data-wise agreement.",
  
  "Brennan-Prediger K" = "Not sensitive against unbalanced marginal 
  distributions or category frequencies. Assesses data-wise agreement.",
  
  "Krippendorff's Alpha" = "Very similar to Scott's Pi. Same sensitivities as 
  KC. Assesses category-wise agreement.",
  
  "Iota" = "Can be used for multivariate datasets (when raters assigned mora than 
  1 rating variable to the same objects). Assesses category-wise agreement.",
  
  "Sklar's Omega" = "Very versatile, model based approach for agreement. Similar to 
  Krippendorff's Alpha but performs better under extreme conditions and non-normally 
  distributed data. Assesses category-wise agreement.",
  
  "Lin's CCC" = "Was developed for metric data as an alternative to the ICC. 
  In addition to an overall value, precision (pearson correlation) and accuracy 
  (how far the line of best fit deviates from the line through the origin) 
  together with scale- and location-shift values are reported. Assesses category-wise 
  agreement",
  
  "ICC" = "Has similar sensitivities as KC. Assesses category-wise agreement.",
  
  check.names = F
)
  
ratioTwoNames <- c("Cohen's Kappa",
                   "Scott's Pi",
                   "Krippendorff's Alpha",
                   "Gwet's AC2",
                   "Brennan-Prediger K",
                   "Aickin's Alpha",
                   "Lin's CCC",
                   "ICC")
  
ratioTwoVals <- c('kappa_cohen',
                  'kappa_pi',
                  'other_kripp',
                  'kappa_gwet',
                  'kappa_brennan',
                  'kappa_aickin',
                  'other_occc',
                  'other_icc')

ratioTwoDesc <- data.frame(
  "Cohen's Kappa (KC)" = "Most frequently used measure. Sensitive on marginal 
  distribution and category frequencies. Assesses category-wise agreement",
  
  "Scott's Pi" = "Same sensitivities as KC. Assesses category-wise agreement",
  
  "Krippendorff's Alpha" = "Very similar to Scott's Pi. Same sensitivities as 
  KC. Assesses category-wise agreement.",
  
  "Gwet's AC1" = "Not sensitive against unbalanced marginal distributions or 
  frequencies. Assesses data-wise agreement.",
  
  "Brennan-Prediger K" = "Not sensitive against unbalanced marginal 
  distributions or category frequencies. Assesses data-wise agreement.",
  
  "Aickin's Alpha" = "Very similar to Gwet's AC but it can hardly reach the maximum 
  value of 1. Assesses data-wise agreement.",
  
  "Lin's CCC" = "Was developed for metric data as an alternative to the ICC. 
  In addition to an overall value, precision (pearson correlation) and accuracy 
  (how far the line of best fit deviates from the line through the origin) 
  together with scale- and location-shift values are reported. Assesses category-wise 
  agreement",
  
  "ICC" = "Has similar sensitivities as KC. Assesses category-wise agreement.",
  
  check.names = F
)
  
ratioMoreNames <- c("Fleiss' Kappa",
                    "Conger's Kappa",
                    "Gwet's AC2",
                    "Brennan-Prediger K",
                    "Krippendorff's Alpha",
                    "Sklar's Omega",
                    "Lin's CCC",
                    "ICC")
  
ratioMoreVals <- c('kappa_fleiss',
                   'kappa_conger',
                   'kappa_gwet',
                   'kappa_brennan',
                   'other_kripp',
                   'other_omega',
                   'other_occc',
                   'other_icc')

ratioMoreDesc <- data.frame(
  "Fleiss' Kappa" = "Generalization of Cohen's Kappa for more than 2 raters. 
  Sensitive on marginal distribution and category frequencies. 
  Assesses category-wise agreement",
  
  "Conger's Kappa" = "Same sensitivities as KC. Assesses category-wise agreement",
  
  "Gwet's AC1" = "Not sensitive against unbalanced marginal distributions or 
  frequencies. Assesses data-wise agreement.",
  
  "Brennan-Prediger K" = "Not sensitive against unbalanced marginal 
  distributions or category frequencies. Assesses data-wise agreement.",
  
  "Krippendorff's Alpha" = "Very similar to Scott's Pi. Same sensitivities as 
  KC. Assesses category-wise agreement.",
  
  "Sklar's Omega" = "Very versatile, model based approach for agreement. Similar to 
  Krippendorff's Alpha but performs better under extreme conditions and non-normally 
  distributed data. Assesses category-wise agreement.",
  
  "Lin's CCC" = "Was developed for metric data as an alternative to the ICC. 
  In addition to an overall value, precision (pearson correlation) and accuracy 
  (how far the line of best fit deviates from the line through the origin) 
  together with scale- and location-shift values are reported. Assesses category-wise 
  agreement",
  
  "ICC" = "Has similar sensitivities as KC. Assesses category-wise agreement.",
  
  check.names = F
)

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



refList <- c(
    "AgreeStat 360: Cloud-based inter-rater reliability analysis, Cohen's kappa, Gwet's AC1/AC2, Krippendorff's alpha, Brennan-Prediger, Fleiss generalized kappa, intraclass correlation coefficients. (n.d.). Retrieved April 8, 2021, from https://agreestat360.com/",
    "Aickin, M. (1990). Maximum Likelihood Estimation of Agreement in the Constant Predictive Probability Model, and Its Relation to Cohen's Kappa. Biometrics, 46(2), 293-302. https://doi.org/10.2307/2531434",
    "Bangdiwala, S. I., Haedo, A. S., Natal, M. L., & Villaveces, A. (2008). The agreement chart as an alternative to the receiver-operating characteristic curve for diagnostic tests. Journal of Clinical Epidemiology, 61(9), 866-874. https://doi.org/10.1016/j.jclinepi.2008.04.002",
    "Barnhart, H. X., Haber, M., & Song, J. (2002). Overall concordance correlation coefficient for evaluating agreement among multiple observers. Biometrics, 58(4), 1020-1027.",
    "Berry, K. J., Mielke Jr, P. W., & Johnston, J. E. (2016). Permutation statistical methods: An integrated approach. Springer.",
    "Bexkens, R., Claessen, F. M., Kodde, I. F., Oh, L. S., Eygendaal, D., & den Bekerom, M. P. van. (2018). The kappa paradox. Shoulder & Elbow, 10(4), 308-308. https://doi.org/10.1177/1758573218791813",
    "Brennan, R. L., & Prediger, D. J. (1981). Coefficient kappa: Some uses, misuses, and alternatives. Educational and Psychological Measurement, 41(3), 687-699.",
    "Broemeling, L. D. (2009). Bayesian methods for measures of agreement. CRC Press.",
    "Brown, R. D., & Hauenstein, N. M. (2005). Interrater agreement reconsidered: An alternative to the rwg indices. Organizational Research Methods, 8(2), 165-184.",
    "Carpentier, M., Combescure, C., Merlini, L., & Perneger, T. V. (2017). Kappa statistic to measure agreement beyond chance in free-response assessments. BMC Medical Research Methodology, 17(1), 62. https://doi.org/10.1186/s12874-017-0340-6",
    "Carrasco, J. L., King, T. S., & Chinchilli, V. M. (2009). The concordance correlation coefficient for repeated measures estimated by variance components. Journal of Biopharmaceutical Statistics, 19(1), 90-105.",
    "Carrasco, J. L., Phillips, B. R., Puig-Martinez, J., King, T. S., & Chinchilli, V. M. (2013). Estimation of the concordance correlation coefficient for repeated measures using SAS and R. Computer Methods and Programs in Biomedicine, 109(3), 293-304.",
    "Casagrande, A., Fabris, F., & Girometti, R. (2020a). Beyond kappa: An informational index for diagnostic agreement in dichotomous and multivalue ordered-categorical ratings. Medical & Biological Engineering & Computing, 58(12), 3089-3099. https://doi.org/10.1007/s11517-020-02261-2",
    "Casagrande, A., Fabris, F., & Girometti, R. (2020b). Extending Information Agreement by Continuity. 2020 IEEE International Conference on Bioinformatics and Biomedicine (BIBM), 1432-1439. https://doi.org/10.1109/BIBM49941.2020.9313173",
    "Chang, W., Cheng, J., Allaire, J. J., Xie, Y., & McPherson, J. (2020). shiny: Web Application Framework for R. https://CRAN.R-project.org/package=shiny",
    "Chen, C.-C., & Barnhart, H. X. (2008). Comparison of ICC and CCC for assessing agreement for data without and with replications. Computational Statistics & Data Analysis, 53(2), 554-564. https://doi.org/10.1016/j.csda.2008.09.026",
    "Choudhary, P. K., & Nagaraja, H. N. (2017). Measuring agreement: Models, methods, and applications. John Wiley & Sons.",
    "Cohen, J. (1960). A coefficient of agreement for nominal scales. Educational and Psychological Measurement, 20(1), 37-46.",
    "Cohen's Kappa and Other Interrater Agreement Measures. (n.d.). Retrieved April 8, 2021, from http://langtest.jp/shiny/kappa/",
    "Conger, A. J. (1980). Integration and generalization of kappas for multiple raters. Psychological Bulletin, 88(2), 322.",
    "Dettori, J. R., & Norvell, D. C. (2020). Kappa and Beyond: Is There Agreement? Global Spine Journal, 10(4), 499-501. https://doi.org/10.1177/2192568220911648",
    "Feinstein, A. R., & Cicchetti, D. V. (1990). High agreement but low Kappa: I. the problems of two paradoxes. Journal of Clinical Epidemiology, 43(6), 543-549. https://doi.org/10.1016/0895-4356(90)90158-L",
    "Finn, R. H. (1970). A note on estimating the reliability of categorical data. Educational and Psychological Measurement, 30(1), 71-76.",
    "Fleiss, J. L. (1971). Measuring nominal scale agreement among many raters. Psychological Bulletin, 76(5), 378.",
    "Gautam, S. (2014). A-kappa: A measure of agreement among multiple raters. Journal of Data Science, 12, 697-716.",
    "Gwet, K. L. (2014). Handbook of inter-rater reliability: The definitive guide to measuring the extent of agreement among raters; [a handbook for researchers, practitioners, teachers & students] (4. ed). Advanced Analytics, LLC.",
    "Harris, J. A. (1913). On the calculation of intra-class and inter-class coefficients of correlation from class moments when the number of possible combinations is large. Biometrika, 9(3/4), 446-472.",
    "Holsti, O. R. (1969). Content analysis for the social sciences and humanities. Reading. MA: Addison-Wesley (Content Analysis).",
    "Hubert, L. (1977). Kappa revisited. Psychological Bulletin, 84(2), 289.",
    "Hughes, J. (2018). Sklar's Omega: A Gaussian Copula-Based Framework for Assessing Agreement. ArXiv:1803.02734 [Stat]. http://arxiv.org/abs/1803.02734",
    "James, L. R., Demaree, R. G., & Wolf, G. (1984). Estimating within-group interrater reliability with and without response bias. Journal of Applied Psychology, 69(1), 85.",
    "James, L. R., Demaree, R. G., & Wolf, G. (1993). rwg: An assessment of within-group interrater agreement. Journal of Applied Psychology, 78(2), 306.",
    "Janson, H., & Olsson, U. (2001). A Measure of Agreement for Interval or Nominal Multivariate Observations. Educational and Psychological Measurement, 61(2), 277-289. https://doi.org/10.1177/00131640121971239",
    "Janson, H., & Olsson, U. (2004). A Measure of Agreement for Interval or Nominal Multivariate Observations by Different Sets of Judges. Educational and Psychological Measurement, 64(1), 62-70. https://doi.org/10.1177/0013164403260195",
    "Kling-Gupta efficiency Calculator. (n.d.). Retrieved April 8, 2021, from https://agrimetsoft.com/calculators/Kling-Gupta%20efficiency",
    "Kreuzpointner, L., Simon, P., & Theis, F. J. (2010). The ad coefficient as a descriptive measure of the within-group agreement of ratings. British Journal of Mathematical and Statistical Psychology, 63(2), 341-360. https://doi.org/10.1348/000711009X465647",
    "Krippendorff, K. (1970). Estimating the reliability, systematic error and random error of interval data. Educational and Psychological Measurement, 30(1), 61-70.",
    "Krippendorff, K. (2011). Agreement and Information in the Reliability of Coding. Communication Methods and Measures, 5(2), 93-112. https://doi.org/10.1080/19312458.2011.568376",
    "Krippendorff, K. (2016). Misunderstanding Reliability. Methodology, 12(4), 139-144. https://doi.org/10.1027/1614-2241/a000119",
    "Lanz, M., Sorgente, A., & Tagliabue, S. (2018). Inter-rater Agreement Indices for Multiple Informant Methodology. Marriage & Family Review, 54(2), 148-182. https://doi.org/10.1080/01494929.2017.1340919",
    "Lawrence, I., & Lin, K. (1989). A concordance correlation coefficient to evaluate reproducibility. Biometrics, 255-268.",
    "Light, R. J. (1971). Measures of response agreement for qualitative data: Some generalizations and alternatives. Psychological Bulletin, 76(5), 365.",
    "Lin, L., Hedayat, A. S., & Wu, W. (2012). Statistical Tools for Measuring Agreement. Springer New York. https://doi.org/10.1007/978-1-4614-0562-7",
    "Lin, L., Hedayat, A., & Wu, W. (2007). A unified approach for assessing agreement for continuous and categorical data. Journal of Biopharmaceutical Statistics, 17(4), 629-652.",
    "Lindell, M. K., & Brandt, C. J. (1997). Measuring interrater agreement for ratings of a single target. Applied Psychological Measurement, 21(3), 271-278.",
    "Maclure, M., & Willett, W. C. (1987). Misinterpretation and misuse of the kappa statistic. American Journal of Epidemiology, 126(2), 161-169.",
    "Marasini, D., Quatto, P., & Ripamonti, E. (2016). Assessing the inter-rater agreement for ordinal data through weighted indexes. Statistical Methods in Medical Research, 25(6), 2611-2633. https://doi.org/10.1177/0962280214529560",
    "Martin Andres, A., & Alvarez Hernandez, M. (2020). Hubert's multi-rater kappa revisited. British Journal of Mathematical and Statistical Psychology, 73(1), 1-22. https://doi.org/10.1111/bmsp.12167",
    "MATLAB. (2021). Version 9.10 (R2021a). The MathWorks Inc.",
    "Maxwell, A. E. (1970). Comparing the Classification of Subjects by Two Independent Judges. The British Journal of Psychiatry, 116(535), 651-655. https://doi.org/10.1192/bjp.116.535.651",
    "McGraw, K. O., & Wong, S. P. (1996). Forming inferences about some intraclass correlation coefficients. Psychological Methods, 1(1), 30.",
    "McNemar, Q. (1947). Note on the sampling error of the difference between correlated proportions or percentages. Psychometrika, 12(2), 153-157. https://doi.org/10.1007/BF02295996",
    "Munoz, S. R., & Bangdiwala, S. I. (1997). Interpretation of Kappa and B statistics measures of agreement. Journal of Applied Statistics, 24(1), 105-112. https://doi.org/10.1080/02664769723918",
    "Olenko, A., & Tsyganok, V. (2016). Double Entropy Inter-Rater Agreement Indices. Applied Psychological Measurement, 40(1), 37-55. https://doi.org/10.1177/0146621615592718",
    "O'Neill, T. A. (2017). An Overview of Interrater Agreement on Likert Scales for Researchers and Practitioners. Frontiers in Psychology, 8, 777. https://doi.org/10.3389/fpsyg.2017.00777",
    "Osgood, C. E. (1959). The Representational Model and Relevant Research Methods. Trends in Content Analysis, Ed. I. de S. Pool, Urbana, Ill.: The University of Illinois Press.",
    "Quarfoot, D., & Levine, R. A. (2016). How Robust Are Multirater Interrater Reliability Indices to Changes in Frequency Distribution? The American Statistician, 70(4), 373-384. https://doi.org/10.1080/00031305.2016.1141708",
    "R Core Team. (2021). R: A Language and Environment for Statistical Computing. R Foundation for Statistical Computing. https://www.R-project.org/",
    "Randolph, J. J. (2005). Free-Marginal Multirater Kappa (multirater K [free]): An Alternative to Fleiss' Fixed-Marginal Multirater Kappa. Online Submission.",
    "ReCal for Ordinal, Interval, and Ratio Data (OIR) - Deen Freelon, Ph.D. (n.d.). Retrieved April 8, 2021, from http://dfreelon.org/utils/recalfront/recal-oir/",
    "Scott, W. A. (1955). Reliability of Content Analysis: The Case of Nominal Scale Coding. The Public Opinion Quarterly, 19(3), 321-325. JSTOR.",
    "Shannon, C. E. (1948). A mathematical theory of communication. The Bell System Technical Journal, 27(3), 379-423.",
    "Shrout, P. E., & Fleiss, J. L. (1979). Intraclass correlations: Uses in assessing rater reliability. Psychological Bulletin, 86(2), 420.",
    "Stuart, A. (1955). A test for homogeneity of the marginal distributions in a two-way classification. Biometrika, 42(3/4), 412-416.",
    "Tran, D., Dolgun, A., & Demirhan, H. (2020). Weighted inter-rater agreement measures for ordinal outcomes. Communications in Statistics - Simulation and Computation, 49(4), 989-1003. https://doi.org/10.1080/03610918.2018.1490428",
    "van Oest, R. (2019). A new coefficient of interrater agreement: The challenge of highly unequal category proportions. Psychological Methods, 24(4), 439-451. https://doi.org/10.1037/met0000183",
    "von Eye, A. (2006). An alternative to Cohen's K. European Psychologist, 11(1), 12-24.",
    "Von Eye, A., & Mun, E. Y. (2014). Analyzing rater agreement: Manifest variable methods. Psychology Press.",
    "Wagner, S. M., Rau, C., & Lindemann, E. (2010). Multiple informant methodology: A critical review and recommendations. Sociological Methods & Research, 38(4), 582-618.",
    "Walter, S. R., Dunsmuir, W. T. M., & Westbrook, J. I. (2019). Inter-observer agreement and reliability assessment for observational studies of clinical work. Journal of Biomedical Informatics, 100, 103317. https://doi.org/10.1016/j.jbi.2019.103317",
    "Warrens, M. J. (2010). A Formal Proof of a Paradox Associated with Cohen's Kappa. Journal of Classification, 27(3), 322-332. https://doi.org/10.1007/s00357-010-9060-x",
    "Warrens, M. J., & de Raadt, A. (2019). Properties of Bangdiwala's B. Advances in Data Analysis and Classification, 13(2), 481-493. https://doi.org/10.1007/s11634-018-0319-0",
    "Willmott, C. J., & Wicks, D. E. (1980). An empirical method for the spatial interpolation of monthly precipitation within California. Physical Geography, 1(1), 59-73.",
    "Xue-Kun Song, P. (2000). Multivariate dispersion models generated from Gaussian copula. Scandinavian Journal of Statistics, 27(2), 305-320.")
