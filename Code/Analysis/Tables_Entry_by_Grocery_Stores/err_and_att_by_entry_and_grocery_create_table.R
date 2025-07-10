# Combines empirical estimates with bootstrap estimates to estimate bootstrapped SEs for 
# average CV errors and treatment effects across multiple dollar store entries and basline grocery store counts (2005). 

# Creates latex code for Tables C.1, C.2, C.5, and C.6. 

# -------------------------------------------------------------------------------------------- #
pacman::p_load('dplyr', 'stringr', 'here', 'purrr', 'tidyr')
# -------------------------------------------------------------------------------------------- #
# Load data. 
# -------------------------------------------------------------------------------------------- #
model_dep_var = 'low_access'
model_geography = 'Urban' # Used in script below to subset by either Urban or Rural.
ds_entry_var <- 'entry_events_bins' # c('entry_events_bins', 'gross_entry_cumsum_bins')
options(scipen = 999)
print(model_dep_var); print(model_geography)
# -------------------------------------------------------------------------------------------- #
fnames <- list.files(here::here('Analysis', 
                                paste0(model_geography, '_Bootstrap'), 
                                'Low_Access', 
                                'bootstrap_errors_and_effects_by_entry_x_grocery'))

# -------------------------------------------------------------------------------------------- #
err_and_att <- fnames %>% 
  
  map(function(.x){ 
    
    readRDS(here::here('Analysis', 
                       paste0(model_geography, '_Bootstrap'), 
                       'Low_Access', 
                       'bootstrap_errors_and_effects_by_entry_x_grocery', 
                       .x))
    
    })
# -------------------------------------------------------------------------------------------- #

boot_res_att <- err_and_att %>% 
  
  map_dfr(function(.x){ 
    
    .x %>% pluck(ds_entry_var, 'att_by_entry_grocery')
    
    })
# -------------------------------------------------------------------------------------------- #
boot_sd_att <- boot_res_att %>%
  
  filter(
    (!is.na(pct_att_avg_post) & !is.infinite(pct_att_avg_post) )
     ) %>%
  
  group_by(across(.cols = c(all_of(ds_entry_var), Grocery_Count_10mile_2005_bins))) %>%
  
  summarise(across(
    .cols = c(actual_avg_post, preds_avg_post, att_avg_post, pct_att_avg_post), 
    .fns = list('sd' = \(x) sd(x, na.rm = TRUE)), 
    .names = '{.col}_{.fn}'
  ) )

sd_vars <- names(boot_sd_att) %>% str_subset(pattern = 'sd$')
# -------------------------------------------------------------------------------------------- #
emp_res <- pluck(err_and_att, 
                            1, ds_entry_var, 'att_by_entry_grocery') %>%
  
  left_join(boot_sd_att, by = c(ds_entry_var, 'Grocery_Count_10mile_2005_bins')) %>%
  
  select(matches('entry_.*bins'), Grocery_Count_10mile_2005_bins, 
         actual_avg_post, preds_avg_post, 
         att_avg_post, att_avg_post_sd, 
         pct_att_avg_post, pct_att_avg_post_sd, 
         total_per_group_post, share, unique_bg_per_group_post)
# -------------------------------------------------------------------------------------------- #
if (model_geography == 'Rural'){ 
  get_lvl_groc <- levels(emp_res[['Grocery_Count_10mile_2005_bins']])[1:4]  
  get_lvl_ds <- levels(emp_res[[ds_entry_var]])[1:4]  
  mile_id = 'ten-mile'
} else if (model_geography == 'Urban'){ 
  get_lvl_groc <- levels(emp_res[['Grocery_Count_10mile_2005_bins']])[1:5]  
  get_lvl_ds <- levels(emp_res[[ds_entry_var]])[1:4]  
  mile_id = 'two-mile'
  }
# -------------------------------------------------------------------------------------------- #
# Check share removed. 
excluded_grp_share <- emp_res[(!(emp_res[['Grocery_Count_10mile_2005_bins']] %in% get_lvl_groc) | !(emp_res[[ds_entry_var]] %in% get_lvl_ds)), ]
in_ex_grp_share <- data.frame(included_grp_share = 1-mean(excluded_grp_share$share), 
                              excluded_grp_share = mean(excluded_grp_share$share)); print(in_ex_grp_share)
# -------------------------------------------------------------------------------------------- #
emp_res <- emp_res %>%
  
  filter(
    (Grocery_Count_10mile_2005_bins %in% get_lvl_groc) & 
      (!!sym(ds_entry_var) %in% get_lvl_ds) 
    )

# Compute confidence levels. 
emp_res_format <- emp_res %>%
  # Create a function to add significance stars based on qnorm values
  mutate(
    
    # For att_avg_post, compute CIs and add significance stars
    att_avg_post = sprintf("%.3f%s", att_avg_post, 
                                     case_when(
                                       # 99% CI doesn't include 0
                                       (att_avg_post - att_avg_post_sd * qnorm(0.995) > 0) | 
                                         (att_avg_post + att_avg_post_sd * qnorm(0.995) < 0) ~ "***",
                                       # 95% CI doesn't include 0
                                       (att_avg_post - att_avg_post_sd * qnorm(0.975) > 0) | 
                                         (att_avg_post + att_avg_post_sd * qnorm(0.975) < 0) ~ "**",
                                       # 90% CI doesn't include 0
                                       (att_avg_post - att_avg_post_sd * qnorm(0.95) > 0) | 
                                         (att_avg_post + att_avg_post_sd * qnorm(0.95) < 0) ~ "*",
                                       TRUE ~ ""
                                     )),
    
    # For pct_att_avg_post, compute CIs and add significance stars
    pct_att_avg_post = sprintf("%.3f%s", pct_att_avg_post * 100, 
                                         case_when(
                                           # 99% CI doesn't include 0
                                           (pct_att_avg_post - pct_att_avg_post_sd * qnorm(0.995) > 0) | 
                                             (pct_att_avg_post + pct_att_avg_post_sd * qnorm(0.995) < 0) ~ "***",
                                           # 95% CI doesn't include 0
                                           (pct_att_avg_post - pct_att_avg_post_sd * qnorm(0.975) > 0) | 
                                             (pct_att_avg_post + pct_att_avg_post_sd * qnorm(0.975) < 0) ~ "**",
                                           # 90% CI doesn't include 0
                                           (pct_att_avg_post - pct_att_avg_post_sd * qnorm(0.95) > 0) | 
                                             (pct_att_avg_post + pct_att_avg_post_sd * qnorm(0.95) < 0) ~ "*",
                                           TRUE ~ ""
                                         ))
  ) %>% 
  
  # Round all other numeric columns to 3 digits
  
  mutate(across(
    .cols = c(actual_avg_post, preds_avg_post, att_avg_post_sd, 
              pct_att_avg_post_sd, share), 
    .fns = \(x) case_when(
      cur_column() == 'pct_att_avg_post_sd' ~ sprintf('%.3f', x * 100), 
      TRUE ~ sprintf('%.3f', x)
    )
                          
  )) %>%
  
  mutate(across( 
    .cols = c(total_per_group_post, unique_bg_per_group_post), 
    .fns = \(x) sprintf('%s', format(x, big.mark = ',', scientific = FALSE, trim = TRUE))
    )) 
# -------------------------------------------------------------------------------------------- #
emp_res_wd <- emp_res_format %>%
  
  pivot_longer(cols = actual_avg_post:unique_bg_per_group_post, 
               names_to = 'variable', 
               values_to = 'value') %>%
  
  pivot_wider(names_from = Grocery_Count_10mile_2005_bins, 
              values_from = value)

emp_res_wd$variable <- emp_res_wd$variable %>%
  str_replace_all(c("actual_avg_post" = 'Low Access (Obs.)', 
                    "preds_avg_post" = 'Low Access (Pred.)', 
                    "^att_avg_post$" = 'ATT', 
                    "^att_avg_post_sd$" = 'ATT SD', 
                    "^pct_att_avg_post$" = 'ATT (\\\\%)', 
                    "^pct_att_avg_post_sd$" = 'ATT (\\\\%) SD', 
                    "total_per_group_post" = 'Obs. per Group', 
                    "share" = 'Share of Obs.', 
                    "unique_bg_per_group_post" = 'Unique BGs per Group'))

emp_res_wd <- emp_res_wd %>%
  
  mutate(variable = case_when(str_detect(variable, 'SD$') ~ '', 
                              TRUE ~ variable))
# -------------------------------------------------------------------------------------------- #

# Create the LaTeX table
library(kableExtra)

long_note <- paste0(" We do not present estimates beyond ", get_lvl_ds[length(get_lvl_ds)], ' ', 
                    str_replace_all(ds_entry_var, c('gross_entry_cumsum' = 'gross_entries', '_' = ' ', ' bins' = '')), 
                    " and ", get_lvl_groc[length(get_lvl_groc)], " baseline grocery stores, as the remaining groups constitute a small percentage (approximately ", 
                    sprintf('%.2f', in_ex_grp_share$excluded_grp_share*100), "\\\\%) of the treated block units.")

entry_id <- str_to_title(
  str_replace_all(ds_entry_var, c('gross_entry_cumsum' = 'gross_entries', '_' = ' ', ' bins' = ''))
)

if (entry_id == 'Gross Entries'){
entry_note <- paste0("Gross dollar store entries are defined as the gross number of dollar store openings within a ", 
                     mile_id, 
                     " drive of block-group population centers from 2006 to 2020.")
} else if (entry_id == 'Entry Events'){
  entry_note <- paste0("Dollar store entry events are defined as the number of times at least one new dollar store opened within a ", 
                       mile_id, " drive of block-group population centers from 2006 to 2020.")
}



caption_title <- paste0("Average Treatment Effects by Dollar Store ", entry_id, " and Baseline Supermarket/Grocery Store Counts in 2005 ", 
                        '(', model_geography, ' Areas)')

col_names <- as.numeric(colnames(emp_res_wd))
col_names <- as.character(col_names[!is.na(col_names)])


align_length <- length(col_names) 
align_length <- paste0('l', paste0(rep('c', align_length), collapse = ''))

tbl <- kable(emp_res_wd %>% select(-matches('entry_.*bins$')), 
      format = "latex", 
      align = align_length, 
      booktabs = TRUE,
      escape = FALSE, # Note: This is the default.
      digits = 4,
      position = '!h',
      linesep = '', 
      centering = TRUE, # Note: This is the default. 
      col.names = c('Outcome/Parameter', col_names),
      caption = caption_title) %>% 
  kable_styling(font_size = 10) %>%
  add_header_above(c('', 'Supermarkets/Grocery Stores in 2005' = length(col_names))) %>%
  footnote(general = paste0(
    "Notes: Bootstrapped standard errors are in parentheses. ",
    "*$p<0.10$, **$p<0.05$, ***$p<0.01$. ",
    "$\\\\text{ATT} (\\\\%)$ gives the $\\\\text{ATT}$ as a percentage of ",
    "the estimated proportion of low access block groups conditioned on the number of entries and baseline grocery stores.", ' ',
    entry_note, ' ',
    "`Share of Obs.' denotes the proportion of the total treated data that each group's observations constitute. ",
    "`Unique BGs per Group' is the number of distinct block groups in each category. ",
    long_note
  ),
  general_title = '', 
  threeparttable = TRUE, 
  escape = FALSE, 
  footnote_as_chunk = TRUE) %>%
  pack_rows(index = setNames(rep(9, length(get_lvl_ds) ), paste0(entry_id, ': ', get_lvl_ds)) ); print(tbl)