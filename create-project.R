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
  
  # comment("# # # # # # # # # # # # # # # # # # #", 
  #         "Preliminaries", 
  #         "# # # # # # # # # # # # # # # # # # #"),
  # action(
  #   name = "design",
  #   run = glue("r:latest analysis/design.R"),
  #   moderately_sensitive = lst(
  #     lib = glue("lib/design/*.json")
  #   ),
  # ),
  
  comment("# # # # # # # # # # # # # # # # # # #", 
          "Extract and process data for exploratory analysis", 
          "# # # # # # # # # # # # # # # # # # #"),
  # all treated people
  action(
    name = "extract_exploratory",
    run = glue(
      "cohortextractor:latest generate_cohort", 
      " --study-definition study_definition_exploratory", 
      " --output-file output/exploratory/extract/input_exploratory.feather",
    ),
    highly_sensitive = lst(
      extract = "output/exploratory/extract/input_exploratory.feather"
    ),
  ),
  
  # all treated people
  action(
    name = "sankey",
    run = "r:latest analysis/exploratory/sankey.R",
    needs = namelesslst(
      "extract_exploratory"
    ),
    moderately_sensitive = lst(
      csv = "output/exploratory/*.csv",
      txt = "output/exploratory/*.txt"
    )
  ),

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

