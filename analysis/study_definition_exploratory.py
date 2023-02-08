# Import codelists from codelists.py
import codelists

# import json module
import json

from cohortextractor import (
  StudyDefinition,
  patients,
  codelist_from_csv,
  codelist,
  filter_codes_by_category,
  combine_codelists,
  params
)

############################################################
# vax variables
from variables_vax_exploratory import generate_vax_variables 
vax_variables = generate_vax_variables(index_date="2020-01-01", n=6)
############################################################

# Specify study definition
study = StudyDefinition(
  
  # Configure the expectations framework
  default_expectations={
    "date": {"earliest": "2020-01-01", "latest": "today"},
    "rate": "uniform",
    "incidence": 0.2,
    "int": {"distribution": "normal", "mean": 1000, "stddev": 100},
    "float": {"distribution": "normal", "mean": 25, "stddev": 5},
  },
  
  population = patients.satisfying(
    "age20210701 >= 18",
  ),

  age20210701 = patients.age_as_of("2021-07-01"),

  #################################################################
  ## Covid vaccine dates
  #################################################################
  **vax_variables,     
  
)
