load('rolling_results.RData')

# Calculations ----

capm_agg = capm %>%
  group_by(Date, term) %>%
  summarize(mean = mean(estimate),
            median = median(estimate),
            lo50 = quantile(estimate, 0.25),
            hi50 = quantile(estimate, 0.75),
            lo80 = quantile(estimate, 0.1),
            hi80 = quantile(estimate, 0.9),
            lo95 = quantile(estimate, 0.025),
            hi95 = quantile(estimate, 0.975))

capm_type = capm %>%
  group_by(Date, term, Type) %>%
  summarize(mean = mean(estimate),
            median = median(estimate),
            lo50 = quantile(estimate, 0.25),
            hi50 = quantile(estimate, 0.75),
            lo80 = quantile(estimate, 0.1),
            hi80 = quantile(estimate, 0.9),
            lo95 = quantile(estimate, 0.025),
            hi95 = quantile(estimate, 0.975),
            n = n())

ff3_agg = ff3 %>%
  group_by(Date, term) %>%
  summarize(mean = mean(estimate),
            median = median(estimate),
            lo50 = quantile(estimate, 0.25),
            hi50 = quantile(estimate, 0.75),
            lo80 = quantile(estimate, 0.1),
            hi80 = quantile(estimate, 0.9),
            lo95 = quantile(estimate, 0.025),
            hi95 = quantile(estimate, 0.975))

ff3_type = ff3 %>%
  group_by(Date, term, Type) %>%
  summarize(mean = mean(estimate),
            median = median(estimate),
            lo50 = quantile(estimate, 0.25),
            hi50 = quantile(estimate, 0.75),
            lo80 = quantile(estimate, 0.1),
            hi80 = quantile(estimate, 0.9),
            lo95 = quantile(estimate, 0.025),
            hi95 = quantile(estimate, 0.975),
            n = n())

ff5_agg = ff5 %>%
  group_by(Date, term) %>%
  summarize(mean = mean(estimate),
            median = median(estimate),
            lo50 = quantile(estimate, 0.25),
            hi50 = quantile(estimate, 0.75),
            lo80 = quantile(estimate, 0.1),
            hi80 = quantile(estimate, 0.9),
            lo95 = quantile(estimate, 0.025),
            hi95 = quantile(estimate, 0.975))

ff5_type = ff5 %>%
  group_by(Date, term, Type) %>%
  summarize(mean = mean(estimate),
            median = median(estimate),
            lo50 = quantile(estimate, 0.25),
            hi50 = quantile(estimate, 0.75),
            lo80 = quantile(estimate, 0.1),
            hi80 = quantile(estimate, 0.9),
            lo95 = quantile(estimate, 0.025),
            hi95 = quantile(estimate, 0.975),
            n = n())

ff5_mom_agg = ff5_mom %>%
  group_by(Date, term) %>%
  summarize(mean = mean(estimate),
            median = median(estimate),
            lo50 = quantile(estimate, 0.25),
            hi50 = quantile(estimate, 0.75),
            lo80 = quantile(estimate, 0.1),
            hi80 = quantile(estimate, 0.9),
            lo95 = quantile(estimate, 0.025),
            hi95 = quantile(estimate, 0.975))

ff5_mom_type = ff5_mom %>%
  group_by(Date, term, Type) %>%
  summarize(mean = mean(estimate),
            median = median(estimate),
            lo50 = quantile(estimate, 0.25),
            hi50 = quantile(estimate, 0.75),
            lo80 = quantile(estimate, 0.1),
            hi80 = quantile(estimate, 0.9),
            lo95 = quantile(estimate, 0.025),
            hi95 = quantile(estimate, 0.975),
            n = n())


lbs_agg = lbs %>%
  group_by(Date, term) %>%
  summarize(mean = mean(estimate),
            median = median(estimate),
            lo50 = quantile(estimate, 0.25),
            hi50 = quantile(estimate, 0.75),
            lo80 = quantile(estimate, 0.1),
            hi80 = quantile(estimate, 0.9),
            lo95 = quantile(estimate, 0.025),
            hi95 = quantile(estimate, 0.975))

lbs_type = lbs %>%
  group_by(Date, term, Type) %>%
  summarize(mean = mean(estimate),
            median = median(estimate),
            lo50 = quantile(estimate, 0.25),
            hi50 = quantile(estimate, 0.75),
            lo80 = quantile(estimate, 0.1),
            hi80 = quantile(estimate, 0.9),
            lo95 = quantile(estimate, 0.025),
            hi95 = quantile(estimate, 0.975),
            n = n())


# CAPM graphs ----

capm_agg %>%
  filter(term == 'Mkt.RF', Date >= '2005-01-01') %>%
  ggplot(aes(x=Date)) +
  geom_line(aes(y=median), color = 'red') +
  geom_ribbon(aes(ymin=lo50, ymax=hi50), fill='red', alpha = 0.2) +
  geom_ribbon(aes(ymin=lo80, ymax=hi80), fill='red', alpha = 0.2) +
  geom_ribbon(aes(ymin=lo95, ymax=hi95), fill='red', alpha = 0.2) +
  labs(y=expression(hat(beta)))

capm_type %>%
  filter(term == '(Intercept)', Date >= '2005-01-01') %>%
  ggplot(aes(x=Date)) +
  geom_line(aes(y=median), color = 'blue') +
  geom_ribbon(aes(ymin=lo50, ymax=hi50), fill='blue', alpha = 0.2) +
  geom_ribbon(aes(ymin=lo80, ymax=hi80), fill='blue', alpha = 0.2) +
  geom_ribbon(aes(ymin=lo95, ymax=hi95), fill='blue', alpha = 0.2) +
  facet_grid(rows=vars(Type), scales = 'free_y') +
  labs(y=expression(hat(alpha))) +
  theme(strip.text.y = element_text(angle=-60))
dev.off()

capm_type %>%
  filter(term == '(Intercept)', Date >= '2005-01-01') %>%
  ggplot(aes(x=Date)) +
  geom_area(aes(y=n, fill=Type), position = 'stack', stat = 'identity')

# FF3 graphs ----

ff3_agg %>%
  filter(term == '(Intercept)') %>%
  ggplot(aes(x=Date)) +
  geom_line(aes(y=median), color = 'blue') +
  geom_ribbon(aes(ymin=lo50, ymax=hi50), fill='blue', alpha = 0.2) +
  geom_ribbon(aes(ymin=lo80, ymax=hi80), fill='blue', alpha = 0.2) +
  geom_ribbon(aes(ymin=lo95, ymax=hi95), fill='blue', alpha = 0.2)

ff3_type %>%
  filter(term == '(Intercept)') %>%
  ggplot(aes(x=Date)) +
  geom_line(aes(y=median), color = 'blue') +
  geom_ribbon(aes(ymin=lo50, ymax=hi50), fill='blue', alpha = 0.2) +
  geom_ribbon(aes(ymin=lo80, ymax=hi80), fill='blue', alpha = 0.2) +
  geom_ribbon(aes(ymin=lo95, ymax=hi95), fill='blue', alpha = 0.2) +
  facet_grid(rows=vars(Type), scales = 'free_y')

ff3_agg %>%
  filter(term == 'SMB') %>%
  ggplot(aes(x=Date)) +
  geom_line(aes(y=median), color = 'red') +
  geom_ribbon(aes(ymin=lo50, ymax=hi50), fill='red', alpha = 0.2) +
  geom_ribbon(aes(ymin=lo80, ymax=hi80), fill='red', alpha = 0.2) +
  geom_ribbon(aes(ymin=lo95, ymax=hi95), fill='red', alpha = 0.2)

ff3_type %>%
  filter(term == 'SMB') %>%
  ggplot(aes(x=Date)) +
  geom_line(aes(y=median), color = 'red') +
  geom_ribbon(aes(ymin=lo50, ymax=hi50), fill='red', alpha = 0.2) +
  geom_ribbon(aes(ymin=lo80, ymax=hi80), fill='red', alpha = 0.2) +
  geom_ribbon(aes(ymin=lo95, ymax=hi95), fill='red', alpha = 0.2) +
  facet_grid(rows=vars(Type), scales = 'free_y')

ff3_agg %>%
  filter(term == 'HML') %>%
  ggplot(aes(x=Date)) +
  geom_line(aes(y=median), color = 'red') +
  geom_ribbon(aes(ymin=lo50, ymax=hi50), fill='red', alpha = 0.2) +
  geom_ribbon(aes(ymin=lo80, ymax=hi80), fill='red', alpha = 0.2) +
  geom_ribbon(aes(ymin=lo95, ymax=hi95), fill='red', alpha = 0.2) 

ff3_type %>%
  filter(term == 'HML') %>%
  ggplot(aes(x=Date)) +
  geom_line(aes(y=median), color = 'red') +
  geom_ribbon(aes(ymin=lo50, ymax=hi50), fill='red', alpha = 0.2) +
  geom_ribbon(aes(ymin=lo80, ymax=hi80), fill='red', alpha = 0.2) +
  geom_ribbon(aes(ymin=lo95, ymax=hi95), fill='red', alpha = 0.2) +
  facet_grid(rows=vars(Type), scales = 'free_y')


# FF5 graphs


ff5_agg %>%
  filter(term == '(Intercept)') %>%
  ggplot(aes(x=Date)) +
  geom_line(aes(y=median), color = 'blue') +
  geom_ribbon(aes(ymin=lo50, ymax=hi50), fill='blue', alpha = 0.2) +
  geom_ribbon(aes(ymin=lo80, ymax=hi80), fill='blue', alpha = 0.2) +
  geom_ribbon(aes(ymin=lo95, ymax=hi95), fill='blue', alpha = 0.2)

ff5_type %>%
  filter(term == '(Intercept)') %>%
  ggplot(aes(x=Date)) +
  geom_line(aes(y=median), color = 'blue') +
  geom_ribbon(aes(ymin=lo50, ymax=hi50), fill='blue', alpha = 0.2) +
  geom_ribbon(aes(ymin=lo80, ymax=hi80), fill='blue', alpha = 0.2) +
  geom_ribbon(aes(ymin=lo95, ymax=hi95), fill='blue', alpha = 0.2) +
  facet_grid(rows=vars(Type), scales = 'free_y')

ff5_agg %>%
  filter(term == 'SMB') %>%
  ggplot(aes(x=Date)) +
  geom_line(aes(y=median), color = 'red') +
  geom_ribbon(aes(ymin=lo50, ymax=hi50), fill='red', alpha = 0.2) +
  geom_ribbon(aes(ymin=lo80, ymax=hi80), fill='red', alpha = 0.2) +
  geom_ribbon(aes(ymin=lo95, ymax=hi95), fill='red', alpha = 0.2)

ff5_type %>%
  filter(term == 'SMB') %>%
  ggplot(aes(x=Date)) +
  geom_line(aes(y=median), color = 'red') +
  geom_ribbon(aes(ymin=lo50, ymax=hi50), fill='red', alpha = 0.2) +
  geom_ribbon(aes(ymin=lo80, ymax=hi80), fill='red', alpha = 0.2) +
  geom_ribbon(aes(ymin=lo95, ymax=hi95), fill='red', alpha = 0.2) +
  facet_grid(rows=vars(Type), scales = 'free_y')

ff5_agg %>%
  filter(term == 'HML') %>%
  ggplot(aes(x=Date)) +
  geom_line(aes(y=median), color = 'red') +
  geom_ribbon(aes(ymin=lo50, ymax=hi50), fill='red', alpha = 0.2) +
  geom_ribbon(aes(ymin=lo80, ymax=hi80), fill='red', alpha = 0.2) +
  geom_ribbon(aes(ymin=lo95, ymax=hi95), fill='red', alpha = 0.2) 

ff5_type %>%
  filter(term == 'HML') %>%
  ggplot(aes(x=Date)) +
  geom_line(aes(y=median), color = 'red') +
  geom_ribbon(aes(ymin=lo50, ymax=hi50), fill='red', alpha = 0.2) +
  geom_ribbon(aes(ymin=lo80, ymax=hi80), fill='red', alpha = 0.2) +
  geom_ribbon(aes(ymin=lo95, ymax=hi95), fill='red', alpha = 0.2) +
  facet_grid(rows=vars(Type), scales = 'free_y')

ff5_agg %>%
  filter(term == 'RMW') %>%
  ggplot(aes(x=Date)) +
  geom_line(aes(y=median), color = 'red') +
  geom_ribbon(aes(ymin=lo50, ymax=hi50), fill='red', alpha = 0.2) +
  geom_ribbon(aes(ymin=lo80, ymax=hi80), fill='red', alpha = 0.2) +
  geom_ribbon(aes(ymin=lo95, ymax=hi95), fill='red', alpha = 0.2) 

ff5_type %>%
  filter(term == 'RMW') %>%
  ggplot(aes(x=Date)) +
  geom_line(aes(y=median), color = 'red') +
  geom_ribbon(aes(ymin=lo50, ymax=hi50), fill='red', alpha = 0.2) +
  geom_ribbon(aes(ymin=lo80, ymax=hi80), fill='red', alpha = 0.2) +
  geom_ribbon(aes(ymin=lo95, ymax=hi95), fill='red', alpha = 0.2) +
  facet_grid(rows=vars(Type), scales = 'free_y')

ff5_agg %>%
  filter(term == 'CMA') %>%
  ggplot(aes(x=Date)) +
  geom_line(aes(y=median), color = 'red') +
  geom_ribbon(aes(ymin=lo50, ymax=hi50), fill='red', alpha = 0.2) +
  geom_ribbon(aes(ymin=lo80, ymax=hi80), fill='red', alpha = 0.2) +
  geom_ribbon(aes(ymin=lo95, ymax=hi95), fill='red', alpha = 0.2) 

ff5_type %>%
  filter(term == 'CMA') %>%
  ggplot(aes(x=Date)) +
  geom_line(aes(y=median), color = 'red') +
  geom_ribbon(aes(ymin=lo50, ymax=hi50), fill='red', alpha = 0.2) +
  geom_ribbon(aes(ymin=lo80, ymax=hi80), fill='red', alpha = 0.2) +
  geom_ribbon(aes(ymin=lo95, ymax=hi95), fill='red', alpha = 0.2) +
  facet_grid(rows=vars(Type), scales = 'free_y')



# LBS graphs


lbs_agg %>%
  filter(term == '(Intercept)') %>%
  ggplot(aes(x=Date)) +
  geom_line(aes(y=median), color = 'blue') +
  geom_ribbon(aes(ymin=lo50, ymax=hi50), fill='blue', alpha = 0.2) +
  geom_ribbon(aes(ymin=lo80, ymax=hi80), fill='blue', alpha = 0.2) +
  geom_ribbon(aes(ymin=lo95, ymax=hi95), fill='blue', alpha = 0.2)

lbs_type %>%
  filter(term == '(Intercept)') %>%
  ggplot(aes(x=Date)) +
  geom_line(aes(y=median), color = 'blue') +
  geom_ribbon(aes(ymin=lo50, ymax=hi50), fill='blue', alpha = 0.2) +
  geom_ribbon(aes(ymin=lo80, ymax=hi80), fill='blue', alpha = 0.2) +
  geom_ribbon(aes(ymin=lo95, ymax=hi95), fill='blue', alpha = 0.2) +
  facet_grid(rows=vars(Type), scales = 'free_y')

lbs_agg %>%
  filter(term == 'PTFSBD') %>%
  ggplot(aes(x=Date)) +
  geom_line(aes(y=median), color = 'red') +
  geom_ribbon(aes(ymin=lo50, ymax=hi50), fill='red', alpha = 0.2) +
  geom_ribbon(aes(ymin=lo80, ymax=hi80), fill='red', alpha = 0.2) +
  geom_ribbon(aes(ymin=lo95, ymax=hi95), fill='red', alpha = 0.2) +
  labs(y='PTFSBD')

lbs_type %>%
  filter(term == 'PTFSBD') %>%
  ggplot(aes(x=Date)) +
  geom_line(aes(y=median), color = 'red') +
  geom_ribbon(aes(ymin=lo50, ymax=hi50), fill='red', alpha = 0.2) +
  geom_ribbon(aes(ymin=lo80, ymax=hi80), fill='red', alpha = 0.2) +
  geom_ribbon(aes(ymin=lo95, ymax=hi95), fill='red', alpha = 0.2) +
  facet_grid(rows=vars(Type), scales = 'free_y')

lbs_agg %>%
  filter(term == 'PTFSFX') %>%
  ggplot(aes(x=Date)) +
  geom_line(aes(y=median), color = 'red') +
  geom_ribbon(aes(ymin=lo50, ymax=hi50), fill='red', alpha = 0.2) +
  geom_ribbon(aes(ymin=lo80, ymax=hi80), fill='red', alpha = 0.2) +
  geom_ribbon(aes(ymin=lo95, ymax=hi95), fill='red', alpha = 0.2) 

lbs_type %>%
  filter(term == 'PTFSFX') %>%
  ggplot(aes(x=Date)) +
  geom_line(aes(y=median), color = 'red') +
  geom_ribbon(aes(ymin=lo50, ymax=hi50), fill='red', alpha = 0.2) +
  geom_ribbon(aes(ymin=lo80, ymax=hi80), fill='red', alpha = 0.2) +
  geom_ribbon(aes(ymin=lo95, ymax=hi95), fill='red', alpha = 0.2) +
  facet_grid(rows=vars(Type), scales = 'free_y')

lbs_agg %>%
  filter(term == 'PTFSCOM') %>%
  ggplot(aes(x=Date)) +
  geom_line(aes(y=median), color = 'red') +
  geom_ribbon(aes(ymin=lo50, ymax=hi50), fill='red', alpha = 0.2) +
  geom_ribbon(aes(ymin=lo80, ymax=hi80), fill='red', alpha = 0.2) +
  geom_ribbon(aes(ymin=lo95, ymax=hi95), fill='red', alpha = 0.2) 

lbs_type %>%
  filter(term == 'PTFSCOM') %>%
  ggplot(aes(x=Date)) +
  geom_line(aes(y=median), color = 'red') +
  geom_ribbon(aes(ymin=lo50, ymax=hi50), fill='red', alpha = 0.2) +
  geom_ribbon(aes(ymin=lo80, ymax=hi80), fill='red', alpha = 0.2) +
  geom_ribbon(aes(ymin=lo95, ymax=hi95), fill='red', alpha = 0.2) +
  facet_grid(rows=vars(Type), scales = 'free_y')

lbs_agg %>%
  filter(term == 'PTFSIR') %>%
  ggplot(aes(x=Date)) +
  geom_line(aes(y=median), color = 'red') +
  geom_ribbon(aes(ymin=lo50, ymax=hi50), fill='red', alpha = 0.2) +
  geom_ribbon(aes(ymin=lo80, ymax=hi80), fill='red', alpha = 0.2) +
  geom_ribbon(aes(ymin=lo95, ymax=hi95), fill='red', alpha = 0.2) 

lbs_type %>%
  filter(term == 'PTFSIR') %>%
  ggplot(aes(x=Date)) +
  geom_line(aes(y=median), color = 'red') +
  geom_ribbon(aes(ymin=lo50, ymax=hi50), fill='red', alpha = 0.2) +
  geom_ribbon(aes(ymin=lo80, ymax=hi80), fill='red', alpha = 0.2) +
  geom_ribbon(aes(ymin=lo95, ymax=hi95), fill='red', alpha = 0.2) +
  facet_grid(rows=vars(Type), scales = 'free_y')

lbs_agg %>%
  filter(term == 'PTFSSTK') %>%
  ggplot(aes(x=Date)) +
  geom_line(aes(y=median), color = 'red') +
  geom_ribbon(aes(ymin=lo50, ymax=hi50), fill='red', alpha = 0.2) +
  geom_ribbon(aes(ymin=lo80, ymax=hi80), fill='red', alpha = 0.2) +
  geom_ribbon(aes(ymin=lo95, ymax=hi95), fill='red', alpha = 0.2) 

lbs_type %>%
  filter(term == 'PTFSSTK') %>%
  ggplot(aes(x=Date)) +
  geom_line(aes(y=median), color = 'red') +
  geom_ribbon(aes(ymin=lo50, ymax=hi50), fill='red', alpha = 0.2) +
  geom_ribbon(aes(ymin=lo80, ymax=hi80), fill='red', alpha = 0.2) +
  geom_ribbon(aes(ymin=lo95, ymax=hi95), fill='red', alpha = 0.2) +
  facet_grid(rows=vars(Type), scales = 'free_y')
