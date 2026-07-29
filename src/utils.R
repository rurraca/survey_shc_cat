lev_pop_type = c(
  'raw', 
  'adj_cte', 
  'adj_ln', 
  'sample',
  'subsample'
  )

lab_pop_type = c(
  'raw' = 'population, salary unadjusted', 
  'adj_cte' = 'population, salary adjusted (constant density)',
  'adj_ln' = 'population, salary adjusted (log-normal density)',
  'sample' = 'sample',
  'susbample' = 'subsample')

pal_pop_type = c(
  'raw' = '#0072B2', 
  'adj_cte' = '#56B4E9',
  'adj_ln' = '#BFE3F2',
  'sample' = '#D55E00',
  'subsample'= '#F6A57A'
)



lab_vars = c(
  "P18_genere" = 'Gender',
  "P19_edat" = 'Age',
  "P21_salari_brut_anual" = 'Gross Annual Salary',
  "P22_provincia" = 'Province',
  "P22_urba_rural"= "DEGURBA"
)
lev_gender = c("R18_dona", "R18_home")
lab_gender = c(
  "R18_dona" = "female",       
  "R18_home" = "male"
)
lev_age = c("R19_16-24anys",  "R19_25-34anys",  "R19_35-44anys",  "R19_45-54anys",  "R19_55-64anys",  "R19_65anys_mes")
lab_age = c(
  "R19_16-24anys" = "19-24",
  "R19_25-34anys" = "25-34",
  "R19_35-44anys" = "35-44",
  "R19_45-54anys" = "45-54",
  "R19_55-64anys" = "55-64",
  "R19_65anys_mes"= "65+"
)
lev_salary = c("R21_menys_11000", "R21_11000-16999", "R21_17000-24999", "R21_25000-35999","R21_36000-49999", "R21_50000_mes")
lab_salary = c(
  "R21_menys_11000" = "< 11,000",
  "R21_11000-16999"  = "[11,000 - 17,000)",
  "R21_17000-24999"  = "[17,000 - 25,000)",
  "R21_25000-35999"  = "[25,000 - 36,000)",
  "R21_36000-49999" = "[36,000 - 50,000)",
  "R21_50000_mes"  = "≥ 50,000"
)
lev_province = c("R22_barcelona", "R22_girona", "R22_lleida", "R22_tarragona")
lab_province = c(
  "R22_barcelona" = 'Barcelona',
  "R22_girona" = "Girona",
  "R22_lleida" = "Lleida",  
  "R22_tarragona" = "Tarragona"
)
lev_degurba =  c("R22_rural",  "R22_semidens",  "R22_urba")
lab_degurba = c(
  "R22_rural" = "Rural",
  "R22_semidens" = "Towns and suburbs",
  "R22_urba"= "Urban"    
)