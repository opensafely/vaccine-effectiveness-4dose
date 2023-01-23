
from cohortextractor import patients, combine_codelists
from codelists import *
import json
import codelists
import pandas as pd

############################################################
## functions
from variables_functions import *
############################################################

vaccine_product_names = pd.read_csv(
  filepath_or_buffer = "./lib/vaccine-product-names",
  dtype = str,
  delimiter = ";"
)


def generate_vax_variables(index_date, n):

  vax_variables = dict(
    
    # any covid vaccine
    **vaccination_date_X(
      name = "covid_vax_disease",
      index_date = index_date,
      n = n,
      target_disease_matches="SARS-2 CORONAVIRUS"
      ),

  )

  for i in vaccine_product_names.index:

    vax_variables.update(vaccination_date_X(
      name = vaccine_product_names["name"][i],
      n = n,
      index_date = index_date,
      # for some reason vaccine_product_names["product_name"][i] not working,
      # so select product_name col by index
      product_name_matches = vaccine_product_names.iloc[:,1][i]
    ))

  return vax_variables

