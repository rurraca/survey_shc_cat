

# ------------------------------------------------------------------------------
# variables
lev_var = c('use', 'buy',  'Q03_channel', 'Q05_buying_frequency', 'Q06_buy_for_who', 'Q07_quantity_second_vs_new', 'Q15_disposal_frequency',  'Q23_environmental_concern')
lab_var = c(
  'use' = 'second-hand use', 
  'buy' = 'second-hand purchase',  
  'Q03_channel' = 'purchase channel', 
  'Q05_buying_frequency' = 'purchase frequency', 
  'Q06_buy_for_who' = 'purchase recipient', 
  'Q07_quantity_second_vs_new' = 'second-hand vs new quantity', 
  'Q15_disposal_frequency' = 'disposal frequency',  
  'Q23_environmental_concern' = 'environmental concern'
)
lev_var_cat = c(
  "non-buyers", "buyers", 
  "non-users", "users",
  "A01B_no_use_no_buy", "A01B_free", "A01B_money", "A01B_money_free",
  "less", 'same', 'more', 
  'online', "equal",  "physical",
  'low', 'medium', 'high',
  'others', 'both', 'me'
)
lab_var_cat = c(
  "non-buyers" = "Don't buy", 
  "buyers" = 'Buy', 
  "non-users" = "Don't use", 
  "users" = 'Use',
  "A01B_money" = 'Only formal', 
  "A01B_free" = 'Only informal', 
  "A01B_money_free" = 'Formal and informal',
  "less" = 'Less', 
  'same' = 'Same', 
  'more' = 'More', 
  'physical' = 'Physic ', 
  "equal" = 'Equal',  
  "online" = 'Online',
  'low' = 'Low', 
  'medium' = 'Medium', 
  'high' = 'High',
  'me' = 'Only myself', 
  'both' = 'Myself and others', 
  'others' = 'Only others'
)


# individually

lev_quantity = rev(c('A07_much_less', 'A07_less', 'A07_same', 'A07_more', 'A07_much_more', 'A07_all'))
lab_quantity = c(
  'A07_much_less' = 'Much less', 
  'A07_less' = 'Less', 
  'A07_same' = 'Same', 
  'A07_more' = 'More', 
  'A07_much_more' = 'Much more', 
  'A07_all' = 'All second-hand'
)

lev_channel = c('physic', 'physic>online', 'physic=online', 'online>physic', 'online', 'dont_buy_new')
lab_channel = c(
  'dont_buy_new' = "Don't buy clothing", 
  'physic' = 'Physic', 
  'physic>online' = 'Physic > online', 
  'physic=online' = 'Physic = online', 
  'online>physic' = 'Online < physic',
  'online' = 'Online'
)

lev_frequency = c(
  '1week', '2month', '1month', '6-11year', '3-5year', '2year', '1year', 'less_1year', 'n/a'
)
lab_frequency= c(
  '1week' = '1 per week', 
  '2month' = '2 per month', 
  '1month' = '1 per month', 
  '6-11year' = '6-11 per year', 
  '3-5year' = '3-5 per year', 
  '2year' = '2 per year', 
  '1year' = '1 per year', 
  'less_1year' = '< 1 per year', 
  'n/a' = "Don't buy clothing"
)

lab_clothes = c(
  "Q08_tshirts_shirts_blouses" = 'T-shirts, shirts and blouses',
  "Q08_pants_shorts" = 'Pants, shorts and skirts',
  "Q08_jackets_coats" = 'Jackets and coats',
  "Q08_sweaters_midlayers" = 'Sweaters and midlayers',
  "Q08_shoes" = 'Footwear',
  "Q08_dresses_skirts_jumpsuits" = 'Dresses and jumpsuits',
  "Q08_children_clothing" = 'Childwear',
  "Q08_sportswear" = 'Sportswear',
  "Q08_textile_apparel_accessories" = 'apparel accessories',
  "Q08_bags" = 'bags',
  "Q08_bags_accessories" = 'Textile accessories and bags',
  "Q08_occasional_wear_maternity" = 'oaccasional matertinity',
  "Q08_occasional_wear_suits" = 'occasional suits',
  "Q08_occasional" = 'Occasional wear',
  "Q08_household" = 'Household',
  "Q08_leggins_stockings_tights_socks_underwear_swimwear" = 'Undewear',
  "Q08_other" = 'Other'
)

lev_recipient = c('me', 'both', 'others', 'n/a')
lab_recipient = c(
  'me' = 'Only myself', 
  'both' = 'Myself & others', 
  'others' = 'Only others',
  'n/a' = "Don't buy clothing"
  )


lev_concern = c('A23_no_concern', 'A23_hardly_change_habits', 'A23_change_habits', 'A23_change_many_habits', 'A23_ecoansiety')
lab_concern = c(
  'A23_no_concern' = 'No concern',
  'A23_hardly_change_habits' = 'Hardly change habits', 
  'A23_change_habits' = 'Change habits', 
  'A23_change_many_habits' = 'Change many habits', 
  'A23_ecoansiety' = 'Ecoansiety'
)


# ------------------------------------------------------------------------------
# motivations
lev_m = c(
  "A02A_cheap",     
  "A02A_sustainable",  
  "A02A_unique_piece", 
  "A02A_consumption_patterns",           
  "A02A_quality",    
  "A02A_shopping_experience", 
  "A02A_trendy"
)

lab_m = c(
  "A02A_cheap" = 'Cheaper price',     
  "A02A_consumption_patterns" = 'Alternative purchase',
  "A02A_sustainable" = 'Environmental sustainability',            
  "A02A_unique_piece" = 'Vintage & unique items', 
  "A02A_shopping_experience" = 'Shopping experience', 
  "A02A_trendy" = 'Trendiness',
  "A02A_quality" = 'Perceived higher quality'   
)

# ------------------------------------------------------------------------------
# barriers
lev_b =  c(
  'A02B_not_accessible', 
  'A02B_unknown_use', 
  'A02B_no_trendy',
  'A02B_same_price',
  'A02B_no_size',  
  'A02B_less_quality',   
  'A02B_no_sustainable', 
  'A02B_embarrassing'
)
lab_b = c(
  'A02B_unknown_use' = 'Hygiene concerns', 
  'A02B_embarrassing' = 'Social stigma', 
  'A02B_less_quality' = 'Perceived lower quality', 
  'A02B_no_trendy' = 'Limited style availability', 
  'A02B_no_size' = 'Limited size availability', 
  'A02B_same_price' = 'Comparable price to new clothing', 
  'A02B_no_sustainable' = 'Environmental scepticism', 
  'A02B_not_accessible' = 'Limited access'
) 


# ------------------------------------------------------------------------------
# COVARIATES
lev_cov =   c(
  'Q18_gender', 
  'Q19_age', 
  'Q21_annual_gross_salary', 
  'Q22B_degurba', 
  'Q22C_province'
  )
lab_cov = c(
  'Q18_gender' = 'Gender', 
  'Q19_age' = 'Age', 
  'Q21_annual_gross_salary' = 'Salary', 
  'Q22B_degurba' = 'DEGURBA', 
  'Q22C_province' = 'Province'
)


lev_cov_cat = c(
  'A18_woman', 'A18_man', 
  '16-34', '35-54', '55+',
  '< 17,000', '[17,000, 36,000)', '> 36,000',
  'A22B_urban', 'A22B_town_suburb', 'A22B_rural',
  'A22C_barcelona', 'A22C_girona', 'A22C_lleida', 'A22C_tarragona'
)
lab_cov_cat = c(
  'A18_woman' = 'Woman',
  'A18_man' = 'Man', 
  '16-34' = '16-34',
  '35-54' = '35-54', 
  '55+' = '≥ 55',
  '< 17,000' = '< 17,000', 
  '[17,000, 36,000)' = '[17,000, 36,000)', 
  '> 36,000' = '≥ 36,000',
  'A22B_urban' = 'Urban', 
  'A22B_town_suburb' = 'Town and suburb', 
  'A22B_rural' = 'Rural',
  'A22C_barcelona' = 'Barcelona', 
  'A22C_girona' = 'Girona', 
  'A22C_lleida' = 'Lleida', 
  'A22C_tarragona' = 'Tarragona'
)

