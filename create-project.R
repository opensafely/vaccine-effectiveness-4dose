library("tidyverse")
library("yaml")
library("here")
library("glue")
#library("rlang")
 
## import local functions and parameters ---
source(here("analysis", "design.R"))

# create action functions ----

## create comment function ----
comment <- function(...) {
  list_comments <- list(...)
  comments <- map(list_comments, ~paste0("## ", ., " ##"))
  comments
}

## create a list of actions
lapply_actions <- function(X, FUN) {
  unlist(
    lapply(
      X,
      FUN
    ),
    recursive = FALSE
  )
}



## create function to convert comment "actions" in a yaml string into proper comments
convert_comment_actions <-function(yaml.txt) {
  yaml.txt %>%
    str_replace_all("\\\n(\\s*)\\'\\'\\:(\\s*)\\'", "\n\\1")  %>%
    #str_replace_all("\\\n(\\s*)\\'", "\n\\1") %>%
    str_replace_all("([^\\'])\\\n(\\s*)\\#\\#", "\\1\n\n\\2\\#\\#") %>%
    str_replace_all("\\#\\#\\'\\\n", "\n")
}


## generic action function ----
action <- function(
  name,
  run,
  arguments=NULL,
  needs=NULL,
  highly_sensitive=NULL,
  moderately_sensitive=NULL,
  ... # other arguments / options for special action types
) {

  outputs <- list(
    highly_sensitive = highly_sensitive,
    moderately_sensitive = moderately_sensitive
  )
  outputs[sapply(outputs, is.null)] <- NULL

  action <- list(
    run = paste(c(run, arguments), collapse=" "),
    needs = needs,
    outputs = outputs,
    ... = ...
  )
  action[sapply(action, is.null)] <- NULL

  action_list <- list(name = action)
  names(action_list) <- name

  action_list
}

namelesslst <- function(...){
  unname(lst(...))
}

## actions for a single matching round ----




action_1matchround <- function(cohort, matching_round){
  
  control_extract_date <- study_dates[[cohort]][["control_extract_dates"]][matching_round]
  
  splice(
    action(
      name = glue("extract_controlpotential_{cohort}_{matching_round}"),
      run = glue(
        "cohortextractor:latest generate_cohort", 
        " --study-definition study_definition_controlpotential", 
        " --output-file output/{cohort}/matchround{matching_round}/extract/input_controlpotential.feather", 
        " --param cohort={cohort}",
        " --param matching_round={matching_round}",
        " --param index_date={control_extract_date}"
      ),
      needs = c(
        "design",
        if(matching_round>1) {glue("process_controlactual_{cohort}_{matching_round-1}")} else {NULL}
      ) %>% as.list,
      highly_sensitive = lst(
        cohort = glue("output/{cohort}/matchround{matching_round}/extract/input_controlpotential.feather")
      )
    ),
    
    action(
      name = glue("process_controlpotential_{cohort}_{matching_round}"),
      run = glue("r:latest analysis/process_data.R"),
      arguments = c("potential", cohort, matching_round),
      needs = namelesslst(
        glue("extract_controlpotential_{cohort}_{matching_round}"),
      ),
      highly_sensitive = lst(
        rds = glue("output/{cohort}/matchround{matching_round}/process/*.rds")
      ),
      moderately_sensitive = lst(
        input_controlpotential_skim = glue("output/{cohort}/matchround{matching_round}/extract/potential/*.txt"),
        data_processed_skim = glue("output/{cohort}/matchround{matching_round}/potential/*.txt"),
        data_controlpotential_skim = glue("output/{cohort}/matchround{matching_round}/process/*.txt")
      )
    ),
    
    action(
      name = glue("match_potential_{cohort}_{matching_round}"),
      run = glue("r:latest analysis/matching/match_potential.R"),
      arguments = c(cohort, matching_round),
      needs = c(
        glue("process_treated"), 
        glue("process_controlpotential_{cohort}_{matching_round}"),
        if(matching_round>1) {glue("process_controlactual_{cohort}_{matching_round-1}")} else {NULL}
      ) %>% as.list,
      highly_sensitive = lst(
        rds = glue("output/{cohort}/matchround{matching_round}/potential/*.rds"),
        csv = glue("output/{cohort}/matchround{matching_round}/potential/*.csv.gz"),
      )
    ),
    
    action(
      name = glue("extract_controlactual_{cohort}_{matching_round}"),
      run = glue(
        "cohortextractor:latest generate_cohort", 
        " --study-definition study_definition_controlactual", 
        " --output-file output/{cohort}/matchround{matching_round}/extract/input_controlactual.feather", 
        " --param cohort={cohort}",
        " --param matching_round={matching_round}",
      ),
      needs = namelesslst(
        "design",
        glue("match_potential_{cohort}_{matching_round}"), 
      ),
      highly_sensitive = lst(
        cohort = glue("output/{cohort}/matchround{matching_round}/extract/input_controlactual.feather")
      )
    ),
    
    
    action(
      name = glue("process_controlactual_{cohort}_{matching_round}"),
      run = glue("r:latest analysis/process_data.R"),
      arguments = c("actual", cohort, matching_round),
      needs = c(
        glue("process_treated"),
        glue("match_potential_{cohort}_{matching_round}"), 
        glue("extract_controlpotential_{cohort}_{matching_round}"),  # this is only necessary for the dummy data
        glue("process_controlpotential_{cohort}_{matching_round}"), # this is necessary for the vaccine data
        glue("extract_controlactual_{cohort}_{matching_round}"),
        if(matching_round>1){glue("process_controlactual_{cohort}_{matching_round-1}")} else {NULL}
      ) %>% as.list,
      highly_sensitive = lst(
        rds = glue("output/{cohort}/matchround{matching_round}/actual/*.rds"),
        csv = glue("output/{cohort}/matchround{matching_round}/actual/*.csv.gz"),
      ),
      moderately_sensitive = lst(
        input_controlactual_skim = glue("output/{cohort}/matchround{matching_round}/extract/actual/*.txt"),
        data_actual_skim = glue("output/{cohort}/matchround{matching_round}/actual/*.txt"),
      )
    )

  )
}

# test function
#action_1matchround("pfizer", 2)

# create all necessary actions for n matching rounds
action_extract_and_match <- function(cohort){
  
  n_matching_rounds <- n_matching_rounds_list[[cohort]]
  
  allrounds <- map(seq_len(n_matching_rounds), ~action_1matchround(cohort, .x)) %>% flatten
  
  splice(
    
    allrounds,
    
    
    action(
      name = glue("extract_controlfinal_{cohort}"),
      run = glue(
        "cohortextractor:latest generate_cohort", 
        " --study-definition study_definition_controlfinal", 
        " --output-file output/{cohort}/extract/input_controlfinal.feather",
        " --param cohort={cohort}",
        " --param n_matching_rounds={n_matching_rounds}",
      ),
      needs = namelesslst(
        "design",
        glue("process_controlactual_{cohort}_{n_matching_rounds}")
      ),
      highly_sensitive = lst(
        extract = glue("output/{cohort}/extract/input_controlfinal.feather")
      ),
    ),
    
    action(
      name = glue("dummydata_controlfinal_{cohort}"),
      run = glue("r:latest analysis/dummy/dummydata_controlfinal.R"),
      arguments = c(cohort),
      needs =map(
        seq_len(n_matching_rounds),
        ~glue("process_controlactual_{cohort}_",.x)
      ),
      highly_sensitive = lst(
        dummydata_controlfinal = glue("output/{cohort}/dummydata/dummy_control_final.feather")
      ),
    ),
    
    action(
      name = glue("process_controlfinal_{cohort}"),
      run = glue("r:latest analysis/process_data.R"),
      arguments = c("final", cohort),
      needs = c(
        map(
          seq_len(n_matching_rounds),
          ~glue("process_controlactual_{cohort}_",.x)
        ),
        glue("extract_controlfinal_{cohort}"),
        glue("process_treated"),
        glue("dummydata_controlfinal_{cohort}")
      ),
      highly_sensitive = lst(
        extract = glue("output/{cohort}/match/*.rds"),
        ids = glue("output/{cohort}/match/*.csv.gz")
      ),
      moderately_sensitive = lst(
        input_controlfinal_skim = glue("output/{cohort}/extract/*.txt"),
        data_matched_skim = glue("output/{cohort}/match/*.txt")
      )
    )
  )
  
}

# test action
# action_extract_and_match("pfizer", 2)

actions_model <- function(cohort, subgroup, variant_option, outcome){
  splice(
    # km
    action(
      name = glue("km_{cohort}_{subgroup}_{variant_option}_{outcome}"),
      run = glue("r:latest analysis/model/km.R"),
      arguments = c(cohort, subgroup, variant_option, outcome),
      needs = namelesslst(
        glue("process_controlfinal_{cohort}")
      ),
      moderately_sensitive= lst(
        #csv= glue("output/{cohort}/models/km/{subgroup}/{outcome}/*.csv"),
        rds = glue("output/{cohort}/models/km/{subgroup}/{variant_option}/{outcome}/*.rds"),
        png = glue("output/{cohort}/models/km/{subgroup}/{variant_option}/{outcome}/*.png")
      )
    ),
    # cox
    lapply_actions(
      c("unadj", "adj"),
      function(type)
        action(
          name = glue("cox_{type}_{cohort}_{subgroup}_{variant_option}_{outcome}"),
          run = glue("r:latest analysis/model/cox.R"),
          arguments = c(cohort, type, subgroup, variant_option, outcome),
          needs = namelesslst(
            glue("process_controlfinal_{cohort}")
          ),
          moderately_sensitive= lst(
            #csv= glue("output/{cohort}/models/cox/{subgroup}/{outcome}/*.csv"),
            rds = glue("output/{cohort}/models/cox_{type}/{subgroup}/{variant_option}/{outcome}/*.rds")
          )
        )
    )
  )
}


## model action function ----
action_combine <- function(
    cohort
){

  action(
    name = glue("combine_{cohort}"),
    run = glue("r:latest analysis/model/combine.R"),
    arguments = c(cohort),
    needs = splice(
      as.list(
        glue_data(
          .x=km_args,
          "{model}_{cohort}_{subgroup}_{variant_option}_{outcome}"
        )
      )
    ),
    moderately_sensitive = lst(
      rds = glue("output/{cohort}/models/combined/*.csv"),
      png = glue("output/{cohort}/models/combined/*.png"),
    )
  )
}

action_table1 <- function(cohort){
  action(
    name = glue("table1_{cohort}"),
    run = glue("r:latest analysis/matching/table1.R"),
    arguments = c(cohort),
    needs = namelesslst(
      glue("process_controlfinal_{cohort}"),
    ),
    moderately_sensitive= lst(
      csv= glue("output/{cohort}/table1/*.csv"),
      html= glue("output/{cohort}/table1/*.html")
    )
  )
}

action_coverage <- function(cohort){
  action(
    name = glue("coverage_{cohort}"),
    run = glue("r:latest analysis/matching/coverage.R"),
    arguments = c(cohort),
    needs = namelesslst(
      glue("process_controlfinal_{cohort}"),
    ),
    moderately_sensitive= lst(
      csv= glue("output/{cohort}/match/coverage/*.csv"),
      png= glue("output/{cohort}/match/coverage/*.png"),
    )
  )
}

action_cinc_dose4 <- function(cohort){
  action(
    name = glue("cinc_dose4_{cohort}"),
    run = glue("r:latest analysis/model/cinc_dose4.R"),
    arguments = c(cohort),
    needs = namelesslst(
      glue("process_controlfinal_{cohort}"),
    ),
    moderately_sensitive= lst(
      csv= glue("output/{cohort}/models/cinc_dose4/*.csv"),
      png= glue("output/{cohort}/models/cinc_dose4/*.png")
    )
  )
}

# specify project ----

## defaults ----
defaults_list <- lst(
  version = "3.0",
  expectations= lst(population_size=100000L)
)

## actions ----
actions_list <- splice(

  comment("# # # # # # # # # # # # # # # # # # #",
          "DO NOT EDIT project.yaml DIRECTLY",
          "This file is created by create-project.R",
          "Edit and run create-project.R to update the project.yaml",
          "# # # # # # # # # # # # # # # # # # #",
           " "
          ),
  
  comment("# # # # # # # # # # # # # # # # # # #", 
          "Preliminaries", 
          "# # # # # # # # # # # # # # # # # # #"),
  action(
    name = "design",
    run = glue("r:latest analysis/design.R"),
    moderately_sensitive = lst(
      lib = glue("lib/design/*.json")
    ),
  ),
  
  comment("# # # # # # # # # # # # # # # # # # #", 
          "Extract and process treated data", 
          "# # # # # # # # # # # # # # # # # # #"),
  # all treated people
  action(
    name = "extract_treated",
    run = glue(
      "cohortextractor:latest generate_cohort", 
      " --study-definition study_definition_treated", 
      " --output-file output/treated/extract/input_treated.feather",
    ),
    needs = namelesslst(
      "design"
    ),
    highly_sensitive = lst(
      extract = "output/treated/extract/input_treated.feather"
    ),
  ),
  
  # # all treated people
  # action(
  #   name = "process_treated",
  #   run = "r:latest analysis/process_data.R",
  #   arguments = "treated",
  #   needs = namelesslst(
  #     "extract_treated"
  #   ),
  #   highly_sensitive = lst(
  #     eligiblerds = "output/treated/eligible/*.rds",
  #     pfizer = "output/pfizer/treated/*.rds",
  #     moderna = "output/moderna/treated/*.rds"
  #   ),
  #   moderately_sensitive = lst(
  #     eligiblecsv = "output/treated/eligible/*.csv",
  #     input_treated_skim = "output/treated/extract/*.txt",
  #     data_processed_skim = "output/treated/process/*.txt",
  #     data_eligible_skim = "output/treated/eligible/*.txt"
  #   )
  # ),
  # 
  # lapply_actions(
  #   "mrna",
  #   function(x) {
  #     c(
  #       comment("# # # # # # # # # # # # # # # # # # #",
  #               glue("{x} cohort"),
  #               "# # # # # # # # # # # # # # # # # # #"),
  #       
  #       comment("# # # # # # # # # # # # # # # # # # #",
  #               "Extract and match"),
  #       
  #       action_extract_and_match(x),
  #       
  #       action_table1(x),
  #       
  #       action_coverage(x),
  #       
  #       comment("# # # # # # # # # # # # # # # # # # #",
  #               "Covid tests data",
  #               "# # # # # # # # # # # # # # # # # # #"),
  #       
  #       lapply_actions(
  #         c("treated", "control"),
  #         function(y)
  #           # covidtests data for all matched people
  #           # need to do separately for treated and controls, one patient could have two different trial dates,
  #           # and study_definition can only handle one row per patient
  #           action(
  #             name = glue("extract_covidtests_{x}_{y}"),
  #             run = glue(
  #               "cohortextractor:latest generate_cohort", 
  #               " --study-definition study_definition_covidtests", 
  #               " --output-file output/{x}/covidtests/extract/input_covidtests_{y}.feather",
  #               " --param cohort={x}",
  #               " --param arm={y}"
  #             ),
  #             needs = namelesslst(
  #               "design",
  #               "process_controlfinal_mrna"
  #             ),
  #             highly_sensitive = lst(
  #               extract = glue("output/{x}/covidtests/extract/input_covidtests_{y}.feather")
  #             )
  #           )
  #       ),
  #       
  #       action(
  #         name = glue("process_covidtests_{x}"),
  #         run = "r:latest analysis/covidtests/process_covidtests.R",
  #         arguments = "mrna",
  #         needs = namelesslst(
  #           "process_controlfinal_mrna",
  #           glue("extract_covidtests_{x}_treated"),
  #           glue("extract_covidtests_{x}_control")
  #         ),
  #         highly_sensitive = lst(
  #           extract = "output/mrna/covidtests/process/*.rds",
  #         ),
  #         moderately_sensitive = lst(
  #           skim = "output/mrna/covidtests/extract/*.txt",
  #           png = "output/mrna/covidtests/checks/*.png"
  #         )
  #       ),
  #       
  #       action(
  #         name = glue("summarise_covidtests_{x}"),
  #         run = "r:latest analysis/covidtests/summarise_covidtests.R",
  #         arguments = c("mrna", "all"), # may want to look in subgroups later, but for now just "all"
  #         needs = namelesslst(
  #           glue("process_covidtests_{x}")
  #         ),
  #         moderately_sensitive = lst(
  #           csv = "output/mrna/covidtests/summary/all/*.csv",
  #           png = "output/mrna/covidtests/summary/all/*.png"
  #         )
  #       ),
  #       
  #       comment("# # # # # # # # # # # # # # # # # # #",
  #               "Model",
  #               "# # # # # # # # # # # # # # # # # # #"),
  #       
  #       action_cinc_dose4(x),
  #       
  #       lapply_actions(
  #         subgroups,
  #         function(y) {
  #           lapply_actions(
  #             km_args %>% 
  #               filter(subgroup==y) %>%
  #               distinct(variant_option) %>%
  #               unlist() %>% unname(),
  #             function(v) {
  #               c(
  #                 comment("# # # # # # # # # # # # # # # # # # # # # # # # # # # ",
  #                         glue("cohort: {x}; subgroup: {y}; variant option: {v}"),
  #                         "# # # # # # # # # # # # # # # # # # # # # # # # # # # "),
  #                 lapply_actions(
  #                   outcomes,
  #                   function(z) actions_model(cohort=x, subgroup=y, variant_option=v, outcome=z)
  #                 )
  #               )
  #             }
  #           )
  #         }
  #       ),
  #       
  #       comment("# # # # # # # # # # # # # # # # # # #",
  #               glue("combine all outputs for {x} cohort"),
  #               "# # # # # # # # # # # # # # # # # # #"),
  #       
  #       action_combine(x)
  #       
  #     )
  #   }
  # ),
  # # 
  # # comment("# # # # # # # # # # # # # # # # # # #", 
  # #         "Move files for release", 
  # #         "# # # # # # # # # # # # # # # # # # #"),
  # # 
  # # action(
  # #   name = "release",
  # #   run = glue("r:latest analysis/release_objects.R"),
  # #   needs = namelesslst(
  # #     glue("combine_km_pfizer"),
  # #     glue("table1_pfizer"),
  # #     glue("combine_km_under12"),
  # #     glue("table1_under12"),
  # #   ),
  # #   highly_sensitive = lst(
  # #     txt = glue("output/meta-release/*.txt"),
  # #     csv = glue("output/release/*.csv"),
  # #   ),
  # # ),

  comment("#### End ####")
)

project_list <- splice(
  defaults_list,
  list(actions = actions_list)
)

## convert list to yaml, reformat comments and whitespace ----
thisproject <- as.yaml(project_list, indent=2) %>%
  # convert comment actions to comments
  convert_comment_actions() %>%
  # add one blank line before level 1 and level 2 keys
  str_replace_all("\\\n(\\w)", "\n\n\\1") %>%
  str_replace_all("\\\n\\s\\s(\\w)", "\n\n  \\1")


# if running via opensafely, check that the project on disk is the same as the project created here:
if (Sys.getenv("OPENSAFELY_BACKEND") %in% c("expectations", "tpp")){

  thisprojectsplit <- str_split(thisproject, "\n")
  currentproject <- readLines(here("project.yaml"))

  stopifnot("project.yaml is not up-to-date with create-project.R.  Run create-project.R before running further actions." = identical(thisprojectsplit, currentproject))

# if running manually, output new project as normal
} else if (Sys.getenv("OPENSAFELY_BACKEND") %in% c("")){

  ## output to file ----
  writeLines(thisproject, here("project.yaml"))
  #yaml::write_yaml(project_list, file =here("project.yaml"))
  
  ## grab all action names and send to a txt file
  
  names(actions_list) %>% tibble(action=.) %>%
    mutate(
      model = action==""  & lag(action!="", 1, TRUE),
      model_number = cumsum(model),
    ) %>%
    group_by(model_number) %>%
    summarise(
      sets = str_trim(paste(action, collapse=" "))
    ) %>% pull(sets) %>%
    paste(collapse="\n") %>%
    writeLines(here("actions.txt"))

# fail if backend not recognised
} else {
  stop("Backend not recognised by create.project.R script")
}
