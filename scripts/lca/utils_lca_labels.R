#-------------------------------------------------------------------------------
# COV 
lev_cov = c(
  'Q18_gender', 'Q19_age', 'Q21_annual_gross_salary', 'Q22B_degurba', 'Q22C_province', 'Q20_education'
)
lab_cov =  c(
  'Q18_gender' = 'Gender', 
  'Q19_age' = 'Age', 
  'Q21_annual_gross_salary' = 'Gross Annual Salary', 
  'Q22B_degurba' = 'DEGURBA'
)

#-------------------------------------------------------------------------------
# COV categories
lev_cov_cat = c(
  'A18_man', 'A18_woman',
  '16-34', '35-54', '55+',
  '< 17,000', '[17,000, 36,000)', '> 36,000',
  'A22B_urban', 'A22B_town_suburb', 'A22B_rural',
  'A22C_barcelona', 'A22C_girona', 'A22C_lleida', 'A22C_tarragona',
  'low', 'medium', 'high'
)
lab_cov_cat = c(
  'A18_man' = 'male', 'A18_woman' = 'female',
  '16-34' = '16-34', '35-54' = '35-54', '55+' = '55+',
  '< 17,000' = '< 17,000', '[17,000, 36,000)' = '[17,000, 36,000)', '> 36,000' = '> 36,000',
  'A22B_urban' = 'urban', 'A22B_town_suburb' = 'towns & suburbs', 'A22B_rural' = 'rural'
)
pal_cov_cat = c(
  'A18_woman' = '#66c2a5', 'A18_man' = '#fc8d62', 
  '16-34' = '#b2e2e2', '35-54' = '#66c2a4', '55+' = '#238b45',
  '< 17,000' = '#bdc9e1', '[17,000, 36,000)' = '#74a9cf', '> 36,000' = '#0570b0', 
  'A22C_barcelona' = '#ffffb2', 'A22C_tarragona' = '#fecc5c', 'A22C_lleida' = '#fd8d3c', 'A22C_girona' = '#e31a1c',
  'low' = '#d7b5d8', 'medium' = '#df65b0', 'high' = '#ce1256',
  'A22B_rural' = '#cccccc', 'A22B_town_suburb' = '#969696', 'A22B_urban' = '#525252'
)

#-------------------------------------------------------------------------------
# COV ML
lev_cov2 = c(
  'Q18_genderA18_woman', 
  'Q19_age35.54', 'Q19_age55.',
  'Q21_annual_gross_salary.17.000..36.000.', 'Q21_annual_gross_salary..36.000',
  'Q22B_degurbaA22B_town_suburb', 'Q22B_degurbaA22B_rural'           
)
lab_cov2 = c(
  'Q18_genderA18_woman' = 'Gender\n (woman)', 
  'Q19_age35.54' = 'Age\n (35-54)', 
  'Q19_age55.' = 'Age\n (≥ 55)',
  'Q21_annual_gross_salary.17.000..36.000.' = 'Salary \n [17,000, 36,000)', 
  'Q21_annual_gross_salary..36.000' = 'Salary \n ≥ 36,000', 
  'Q22B_degurbaA22B_town_suburb' = 'DEGURBA\n (town and suburb)', 
  'Q22B_degurbaA22B_rural' = 'DEGURBA\n (rural)'           
)
