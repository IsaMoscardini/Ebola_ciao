plot_volAdvEffect <- function(i = 1, adv_effects, data = sampleinfo) {
  adv_effect <- adv_effects[i]
  message(adv_effect)
  temp <- data[,c('volunteer_id','sampling_day',adv_effect)]
  temp$sampling_day <- factor(temp$sampling_day)
  colnames(temp)[3] <- 'adv_effect'
  
  plt <- ggplot(temp, aes(sampling_day, volunteer_id, fill = adv_effect)) + geom_tile() + 
    labs(fill = adv_effect) + theme_minimal() + theme(legend.position = 'top')
  plt
}

plot_outcomeByVar <- function(var_name = 'chills', 
                              outcome_var = 'artrithis',
                              data = sampleinfo) {
  data = data.frame(outcome = data[,outcome_var],var = data[,var_name])
  ggplot(data,aes(var, fill = outcome)) + geom_bar() + 
    geom_text(stat = 'count', 
              aes(label = paste0(round((..count../sum(..count..))*100,2))),
              vjust = -1, size = 3) +
    labs(x = var_name, y = outcome_var, 
         subtitle = paste0('% of ',outcome_var,' factors by ',var_name),
         fill = outcome_var) +
    theme_minimal() + theme(legend.position = 'top') + 
    scale_fill_manual(values = c('grey','darkred'))
}
