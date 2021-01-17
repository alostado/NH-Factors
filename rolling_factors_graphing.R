library(tidyverse)
library(RColorBrewer)
theme_set(theme_minimal())

load('rolling_results.RData')

# Calculations ----

capm_agg = capm %>%
  filter(Type != 'AssetAllocation', Type != 'LongOnly', !is.na(Type)) %>%
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
  filter(Type != 'AssetAllocation', Type != 'LongOnly', !is.na(Type)) %>%
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
  filter(Type != 'AssetAllocation', Type != 'LongOnly', !is.na(Type)) %>%
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
  filter(Type != 'AssetAllocation', Type != 'LongOnly', !is.na(Type)) %>%
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
  filter(Type != 'AssetAllocation', Type != 'LongOnly', !is.na(Type)) %>%
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
  filter(Type != 'AssetAllocation', Type != 'LongOnly', !is.na(Type)) %>%
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
  filter(Type != 'AssetAllocation', Type != 'LongOnly', !is.na(Type)) %>%
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
  filter(Type != 'AssetAllocation', Type != 'LongOnly', !is.na(Type)) %>%
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
  filter(Type != 'AssetAllocation', Type != 'LongOnly', !is.na(Type)) %>%
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
  filter(Type != 'AssetAllocation', Type != 'LongOnly', !is.na(Type)) %>%
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

clrs_betas <- brewer.pal(8, 'Dark2')
names(clrs_betas) <- c('Mkt.RF', 'SMB', 'HML', 'RMW', 'CMA', 'MOM')

clrs_types <- brewer.pal(8, 'Dark2')
names(clrs_types) <- c('Crypto', 'CTA', 'Equity LS', 'Event Driven', 'Fixed Income',
                       'Hedge Fund', 'Market Neutral', 'RiskPremia')

# CAPM graphs ----

png('graphs_rolling/rolling_CAPM_alpha_aggregate.png', width = 8000, height = 4000, res=720)
capm_agg %>%
  filter(term == '(Intercept)', Date >= '2005-01-01') %>%
  ggplot(aes(x=Date)) +
  geom_line(aes(y=median), color = 'blue') +
  geom_ribbon(aes(ymin=lo50, ymax=hi50), fill='blue', alpha = 0.2) +
  geom_ribbon(aes(ymin=lo80, ymax=hi80), fill='blue', alpha = 0.2) +
  geom_ribbon(aes(ymin=lo95, ymax=hi95), fill='blue', alpha = 0.2) +
  labs(y=expression(hat(alpha)))
dev.off()

png('graphs_rolling/rolling_CAPM_beta_aggregate.png', width = 8000, height = 4000, res=720)
capm_agg %>%
  filter(term == 'Mkt.RF', Date >= '2005-01-01') %>%
  ggplot(aes(x=Date)) +
  geom_line(aes(y=median), color = 'red') +
  geom_ribbon(aes(ymin=lo50, ymax=hi50), fill='red', alpha = 0.2) +
  geom_ribbon(aes(ymin=lo80, ymax=hi80), fill='red', alpha = 0.2) +
  geom_ribbon(aes(ymin=lo95, ymax=hi95), fill='red', alpha = 0.2) +
  labs(y=expression(hat(beta)))
dev.off()

png('graphs_rolling/rolling_CAPM_alpha_bytype.png', width = 4000, height = 8000, res=720)
capm_type %>%
  filter(term == '(Intercept)', Date >= '2005-01-01') %>%
  ggplot(aes(x=Date)) +
  geom_line(aes(y=median), color = 'blue') +
  geom_ribbon(aes(ymin=lo50, ymax=hi50), fill='blue', alpha = 0.2) +
  geom_ribbon(aes(ymin=lo80, ymax=hi80), fill='blue', alpha = 0.2) +
  geom_ribbon(aes(ymin=lo95, ymax=hi95), fill='blue', alpha = 0.2) +
  facet_grid(rows=vars(Type), scales = 'free_y') +
  labs(y=expression(hat(alpha))) +
  theme(strip.text.y = element_text(angle=0))
dev.off()

png('graphs_rolling/rolling_CAPM_beta_bytype.png', width = 4000, height = 8000, res=720)
capm_type %>%
  filter(term == 'Mkt.RF', Date >= '2005-01-01') %>%
  ggplot(aes(x=Date)) +
  geom_line(aes(y=median), color = 'red') +
  geom_ribbon(aes(ymin=lo50, ymax=hi50), fill='red', alpha = 0.2) +
  geom_ribbon(aes(ymin=lo80, ymax=hi80), fill='red', alpha = 0.2) +
  geom_ribbon(aes(ymin=lo95, ymax=hi95), fill='red', alpha = 0.2) +
  facet_grid(rows=vars(Type), scales = 'free_y') +
  labs(y=expression(hat(alpha))) +
  theme(strip.text.y = element_text(angle=0))
dev.off()

png('graphs_rolling/rolling_CAPM_numfunuds_bytype.png', width = 8000, height = 4000, res=720)
capm_type %>%
  filter(term == '(Intercept)', Date >= '2005-01-01') %>%
  ggplot(aes(x=Date)) +
  geom_area(aes(y=n, fill=Type), position = 'stack', stat = 'identity') +
  scale_fill_manual(values=clrs_types)
dev.off()

# FF3 graphs ----

png('graphs_rolling/rolling_FF3_alpha_aggregate.png', width = 8000, height = 4000, res=720)
ff3_agg %>%
  filter(term == '(Intercept)', Date >= '2005-01-01') %>%
  ggplot(aes(x=Date)) +
  geom_line(aes(y=median), color = 'blue') +
  geom_ribbon(aes(ymin=lo50, ymax=hi50), fill='blue', alpha = 0.2) +
  geom_ribbon(aes(ymin=lo80, ymax=hi80), fill='blue', alpha = 0.2) +
  geom_ribbon(aes(ymin=lo95, ymax=hi95), fill='blue', alpha = 0.2) +
  labs(y=expression(hat(alpha)))
dev.off()

png('graphs_rolling/rolling_FF3_betas_aggregate.png', width = 8000, height = 4000, res=720)
ff3_agg %>%
  filter(term != '(Intercept)', Date >= '2005-01-01') %>%
  ggplot(aes(x=Date)) +
  geom_line(aes(y=median, color=term)) +
  geom_ribbon(aes(ymin=lo50, ymax=hi50, fill=term), alpha = 0.2) +
  geom_ribbon(aes(ymin=lo80, ymax=hi80, fill=term), alpha = 0.2) +
  geom_ribbon(aes(ymin=lo95, ymax=hi95, fill=term), alpha = 0.2) +
  facet_grid(rows = vars(term), scale = 'free_y') +
  scale_color_manual(values = clrs_betas[1:3]) +
  scale_fill_manual(values = clrs_betas[1:3]) +
  labs(y=expression(hat(beta))) +
  theme(strip.text.y = element_text(angle=0),
        legend.position = 'none')
dev.off()


png('graphs_rolling/rolling_FF3_alpha_bytype.png', width = 4000, height = 8000, res=720)
ff3_type %>%
  filter(term == '(Intercept)', Date >= '2005-01-01') %>%
  ggplot(aes(x=Date)) +
  geom_line(aes(y=median, color=Type)) +
  geom_ribbon(aes(ymin=lo50, ymax=hi50, fill=Type), alpha = 0.2) +
  geom_ribbon(aes(ymin=lo80, ymax=hi80, fill=Type), alpha = 0.2) +
  geom_ribbon(aes(ymin=lo95, ymax=hi95, fill=Type), alpha = 0.2) +
  facet_grid(rows=vars(Type), scales = 'free_y') +
  scale_color_manual(values = clrs_types) +
  scale_fill_manual(values = clrs_types) +
  labs(y=expression(hat(alpha))) +
  theme(strip.text.y = element_text(angle=0),
        legend.position = 'none')
dev.off()

png('graphs_rolling/rolling_FF3_MktRF_bytype.png', width = 4000, height = 8000, res=720)
ff3_type %>%
  filter(term == 'Mkt.RF', Date >= '2005-01-01') %>%
  ggplot(aes(x=Date)) +
  geom_line(aes(y=median, color=Type)) +
  geom_ribbon(aes(ymin=lo50, ymax=hi50, fill=Type), alpha = 0.2) +
  geom_ribbon(aes(ymin=lo80, ymax=hi80, fill=Type), alpha = 0.2) +
  geom_ribbon(aes(ymin=lo95, ymax=hi95, fill=Type), alpha = 0.2) +
  facet_grid(rows=vars(Type), scales = 'free_y') +
  scale_color_manual(values = clrs_types) +
  scale_fill_manual(values = clrs_types) +
  labs(y=expression(hat(beta))) +
  theme(strip.text.y = element_text(angle=0),
        legend.position = 'none')
dev.off()

png('graphs_rolling/rolling_FF3_SMB_bytype.png', width = 4000, height = 8000, res=720)
ff3_type %>%
  filter(term == 'SMB', Date >= '2005-01-01') %>%
  ggplot(aes(x=Date)) +
  geom_line(aes(y=median, color=Type)) +
  geom_ribbon(aes(ymin=lo50, ymax=hi50, fill=Type), alpha = 0.2) +
  geom_ribbon(aes(ymin=lo80, ymax=hi80, fill=Type), alpha = 0.2) +
  geom_ribbon(aes(ymin=lo95, ymax=hi95, fill=Type), alpha = 0.2) +
  facet_grid(rows=vars(Type), scales = 'free_y') +
  scale_color_manual(values = clrs_types) +
  scale_fill_manual(values = clrs_types) +
  labs(y=expression(hat(beta))) +
  theme(strip.text.y = element_text(angle=0),
        legend.position = 'none')
dev.off()

png('graphs_rolling/rolling_FF3_HML_bytype.png', width = 4000, height = 8000, res=720)
ff3_type %>%
  filter(term == 'HML', Date >= '2005-01-01') %>%
  ggplot(aes(x=Date)) +
  geom_line(aes(y=median, color=Type)) +
  geom_ribbon(aes(ymin=lo50, ymax=hi50, fill=Type), alpha = 0.2) +
  geom_ribbon(aes(ymin=lo80, ymax=hi80, fill=Type), alpha = 0.2) +
  geom_ribbon(aes(ymin=lo95, ymax=hi95, fill=Type), alpha = 0.2) +
  facet_grid(rows=vars(Type), scales = 'free_y') +
  scale_color_manual(values = clrs_types) +
  scale_fill_manual(values = clrs_types) +
  labs(y=expression(hat(beta))) +
  theme(strip.text.y = element_text(angle=0),
        legend.position = 'none')
dev.off()

png('graphs_rolling/rolling_FF3_numfunuds_bytype.png', width = 8000, height = 4000, res=720)
ff3_type %>%
  filter(term == '(Intercept)', Date >= '2005-01-01') %>%
  ggplot(aes(x=Date)) +
  geom_area(aes(y=n, fill=Type), position = 'stack', stat = 'identity') +
  scale_fill_manual(values=clrs_types)
dev.off()


# FF5 graphs ----

png('graphs_rolling/rolling_FF5_alpha_aggregate.png', width = 8000, height = 4000, res=720)
ff5_agg %>%
  filter(term == '(Intercept)', Date >= '2005-01-01') %>%
  ggplot(aes(x=Date)) +
  geom_line(aes(y=median), color = 'blue') +
  geom_ribbon(aes(ymin=lo50, ymax=hi50), fill='blue', alpha = 0.2) +
  geom_ribbon(aes(ymin=lo80, ymax=hi80), fill='blue', alpha = 0.2) +
  geom_ribbon(aes(ymin=lo95, ymax=hi95), fill='blue', alpha = 0.2) +
  labs(y=expression(hat(alpha)))
dev.off()

png('graphs_rolling/rolling_FF5_betas_aggregate.png', width = 8000, height = 4000, res=720)
ff5_agg %>%
  filter(term != '(Intercept)', Date >= '2005-01-01') %>%
  ggplot(aes(x=Date)) +
  geom_line(aes(y=median, color=term)) +
  geom_ribbon(aes(ymin=lo50, ymax=hi50, fill=term), alpha = 0.2) +
  geom_ribbon(aes(ymin=lo80, ymax=hi80, fill=term), alpha = 0.2) +
  geom_ribbon(aes(ymin=lo95, ymax=hi95, fill=term), alpha = 0.2) +
  facet_grid(rows = vars(term), scale = 'free_y') +
  scale_color_manual(values = clrs_betas[1:5]) +
  scale_fill_manual(values = clrs_betas[1:5]) +
  labs(y=expression(hat(beta))) +
  theme(strip.text.y = element_text(angle=0),
        legend.position = 'none')
dev.off()


png('graphs_rolling/rolling_FF5_alpha_bytype.png', width = 4000, height = 8000, res=720)
ff5_type %>%
  filter(term == '(Intercept)', Date >= '2005-01-01') %>%
  ggplot(aes(x=Date)) +
  geom_line(aes(y=median, color=Type)) +
  geom_ribbon(aes(ymin=lo50, ymax=hi50, fill=Type), alpha = 0.2) +
  geom_ribbon(aes(ymin=lo80, ymax=hi80, fill=Type), alpha = 0.2) +
  geom_ribbon(aes(ymin=lo95, ymax=hi95, fill=Type), alpha = 0.2) +
  facet_grid(rows=vars(Type), scales = 'free_y') +
  scale_color_manual(values = clrs_types) +
  scale_fill_manual(values = clrs_types) +
  labs(y=expression(hat(alpha))) +
  theme(strip.text.y = element_text(angle=0),
        legend.position = 'none')
dev.off()

png('graphs_rolling/rolling_FF5_MktRF_bytype.png', width = 4000, height = 8000, res=720)
ff5_type %>%
  filter(term == 'Mkt.RF', Date >= '2005-01-01') %>%
  ggplot(aes(x=Date)) +
  geom_line(aes(y=median, color=Type)) +
  geom_ribbon(aes(ymin=lo50, ymax=hi50, fill=Type), alpha = 0.2) +
  geom_ribbon(aes(ymin=lo80, ymax=hi80, fill=Type), alpha = 0.2) +
  geom_ribbon(aes(ymin=lo95, ymax=hi95, fill=Type), alpha = 0.2) +
  facet_grid(rows=vars(Type), scales = 'free_y') +
  scale_color_manual(values = clrs_types) +
  scale_fill_manual(values = clrs_types) +
  labs(y=expression(hat(beta))) +
  theme(strip.text.y = element_text(angle=0),
        legend.position = 'none')
dev.off()

png('graphs_rolling/rolling_FF5_SMB_bytype.png', width = 4000, height = 8000, res=720)
ff5_type %>%
  filter(term == 'SMB', Date >= '2005-01-01') %>%
  ggplot(aes(x=Date)) +
  geom_line(aes(y=median, color=Type)) +
  geom_ribbon(aes(ymin=lo50, ymax=hi50, fill=Type), alpha = 0.2) +
  geom_ribbon(aes(ymin=lo80, ymax=hi80, fill=Type), alpha = 0.2) +
  geom_ribbon(aes(ymin=lo95, ymax=hi95, fill=Type), alpha = 0.2) +
  facet_grid(rows=vars(Type), scales = 'free_y') +
  scale_color_manual(values = clrs_types) +
  scale_fill_manual(values = clrs_types) +
  labs(y=expression(hat(beta))) +
  theme(strip.text.y = element_text(angle=0),
        legend.position = 'none')
dev.off()

png('graphs_rolling/rolling_FF5_HML_bytype.png', width = 4000, height = 8000, res=720)
ff5_type %>%
  filter(term == 'HML', Date >= '2005-01-01') %>%
  ggplot(aes(x=Date)) +
  geom_line(aes(y=median, color=Type)) +
  geom_ribbon(aes(ymin=lo50, ymax=hi50, fill=Type), alpha = 0.2) +
  geom_ribbon(aes(ymin=lo80, ymax=hi80, fill=Type), alpha = 0.2) +
  geom_ribbon(aes(ymin=lo95, ymax=hi95, fill=Type), alpha = 0.2) +
  facet_grid(rows=vars(Type), scales = 'free_y') +
  scale_color_manual(values = clrs_types) +
  scale_fill_manual(values = clrs_types) +
  labs(y=expression(hat(beta))) +
  theme(strip.text.y = element_text(angle=0),
        legend.position = 'none')
dev.off()

png('graphs_rolling/rolling_FF5_RMW_bytype.png', width = 4000, height = 8000, res=720)
ff5_type %>%
  filter(term == 'RMW', Date >= '2005-01-01') %>%
  ggplot(aes(x=Date)) +
  geom_line(aes(y=median, color=Type)) +
  geom_ribbon(aes(ymin=lo50, ymax=hi50, fill=Type), alpha = 0.2) +
  geom_ribbon(aes(ymin=lo80, ymax=hi80, fill=Type), alpha = 0.2) +
  geom_ribbon(aes(ymin=lo95, ymax=hi95, fill=Type), alpha = 0.2) +
  facet_grid(rows=vars(Type), scales = 'free_y') +
  scale_color_manual(values = clrs_types) +
  scale_fill_manual(values = clrs_types) +
  labs(y=expression(hat(beta))) +
  theme(strip.text.y = element_text(angle=0),
        legend.position = 'none')
dev.off()

png('graphs_rolling/rolling_FF5_CMA_bytype.png', width = 4000, height = 8000, res=720)
ff5_type %>%
  filter(term == 'CMA', Date >= '2005-01-01') %>%
  ggplot(aes(x=Date)) +
  geom_line(aes(y=median, color=Type)) +
  geom_ribbon(aes(ymin=lo50, ymax=hi50, fill=Type), alpha = 0.2) +
  geom_ribbon(aes(ymin=lo80, ymax=hi80, fill=Type), alpha = 0.2) +
  geom_ribbon(aes(ymin=lo95, ymax=hi95, fill=Type), alpha = 0.2) +
  facet_grid(rows=vars(Type), scales = 'free_y') +
  scale_color_manual(values = clrs_types) +
  scale_fill_manual(values = clrs_types) +
  labs(y=expression(hat(beta))) +
  theme(strip.text.y = element_text(angle=0),
        legend.position = 'none')
dev.off()

png('graphs_rolling/rolling_FF5_numfunuds_bytype.png', width = 8000, height = 4000, res=720)
ff5_type %>%
  filter(term == '(Intercept)', Date >= '2005-01-01') %>%
  ggplot(aes(x=Date)) +
  geom_area(aes(y=n, fill=Type), position = 'stack', stat = 'identity') +
  scale_fill_manual(values=clrs_types)
dev.off()


# FF5+MOM graphs ----

png('graphs_rolling/rolling_FF5MOM_alpha_aggregate.png', width = 8000, height = 4000, res=720)
ff5_mom_agg %>%
  filter(term == '(Intercept)', Date >= '2005-01-01') %>%
  ggplot(aes(x=Date)) +
  geom_line(aes(y=median), color = 'blue') +
  geom_ribbon(aes(ymin=lo50, ymax=hi50), fill='blue', alpha = 0.2) +
  geom_ribbon(aes(ymin=lo80, ymax=hi80), fill='blue', alpha = 0.2) +
  geom_ribbon(aes(ymin=lo95, ymax=hi95), fill='blue', alpha = 0.2) +
  labs(y=expression(hat(alpha)))
dev.off()

png('graphs_rolling/rolling_FF5MOM_betas_aggregate.png', width = 8000, height = 4000, res=720)
ff5_mom_agg %>%
  filter(term != '(Intercept)', Date >= '2005-01-01') %>%
  ggplot(aes(x=Date)) +
  geom_line(aes(y=median, color=term)) +
  geom_ribbon(aes(ymin=lo50, ymax=hi50, fill=term), alpha = 0.2) +
  geom_ribbon(aes(ymin=lo80, ymax=hi80, fill=term), alpha = 0.2) +
  geom_ribbon(aes(ymin=lo95, ymax=hi95, fill=term), alpha = 0.2) +
  facet_grid(rows = vars(term), scale = 'free_y') +
  scale_color_manual(values = clrs_betas[1:6]) +
  scale_fill_manual(values = clrs_betas[1:6]) +
  labs(y=expression(hat(beta))) +
  theme(strip.text.y = element_text(angle=0),
        legend.position = 'none')
dev.off()


png('graphs_rolling/rolling_FF5MOM_alpha_bytype.png', width = 4000, height = 8000, res=720)
ff5_mom_type %>%
  filter(term == '(Intercept)', Date >= '2005-01-01') %>%
  ggplot(aes(x=Date)) +
  geom_line(aes(y=median, color=Type)) +
  geom_ribbon(aes(ymin=lo50, ymax=hi50, fill=Type), alpha = 0.2) +
  geom_ribbon(aes(ymin=lo80, ymax=hi80, fill=Type), alpha = 0.2) +
  geom_ribbon(aes(ymin=lo95, ymax=hi95, fill=Type), alpha = 0.2) +
  facet_grid(rows=vars(Type), scales = 'free_y') +
  scale_color_manual(values = clrs_types) +
  scale_fill_manual(values = clrs_types) +
  labs(y=expression(hat(alpha))) +
  theme(strip.text.y = element_text(angle=0),
        legend.position = 'none')
dev.off()

png('graphs_rolling/rolling_FF5MOM_MktRF_bytype.png', width = 4000, height = 8000, res=720)
ff5_mom_type %>%
  filter(term == 'Mkt.RF', Date >= '2005-01-01') %>%
  ggplot(aes(x=Date)) +
  geom_line(aes(y=median, color=Type)) +
  geom_ribbon(aes(ymin=lo50, ymax=hi50, fill=Type), alpha = 0.2) +
  geom_ribbon(aes(ymin=lo80, ymax=hi80, fill=Type), alpha = 0.2) +
  geom_ribbon(aes(ymin=lo95, ymax=hi95, fill=Type), alpha = 0.2) +
  facet_grid(rows=vars(Type), scales = 'free_y') +
  scale_color_manual(values = clrs_types) +
  scale_fill_manual(values = clrs_types) +
  labs(y=expression(hat(beta))) +
  theme(strip.text.y = element_text(angle=0),
        legend.position = 'none')
dev.off()

png('graphs_rolling/rolling_FF5MOM_SMB_bytype.png', width = 4000, height = 8000, res=720)
ff5_mom_type %>%
  filter(term == 'SMB', Date >= '2005-01-01') %>%
  ggplot(aes(x=Date)) +
  geom_line(aes(y=median, color=Type)) +
  geom_ribbon(aes(ymin=lo50, ymax=hi50, fill=Type), alpha = 0.2) +
  geom_ribbon(aes(ymin=lo80, ymax=hi80, fill=Type), alpha = 0.2) +
  geom_ribbon(aes(ymin=lo95, ymax=hi95, fill=Type), alpha = 0.2) +
  facet_grid(rows=vars(Type), scales = 'free_y') +
  scale_color_manual(values = clrs_types) +
  scale_fill_manual(values = clrs_types) +
  labs(y=expression(hat(beta))) +
  theme(strip.text.y = element_text(angle=0),
        legend.position = 'none')
dev.off()

png('graphs_rolling/rolling_FF5MOM_HML_bytype.png', width = 4000, height = 8000, res=720)
ff5_mom_type %>%
  filter(term == 'HML', Date >= '2005-01-01') %>%
  ggplot(aes(x=Date)) +
  geom_line(aes(y=median, color=Type)) +
  geom_ribbon(aes(ymin=lo50, ymax=hi50, fill=Type), alpha = 0.2) +
  geom_ribbon(aes(ymin=lo80, ymax=hi80, fill=Type), alpha = 0.2) +
  geom_ribbon(aes(ymin=lo95, ymax=hi95, fill=Type), alpha = 0.2) +
  facet_grid(rows=vars(Type), scales = 'free_y') +
  scale_color_manual(values = clrs_types) +
  scale_fill_manual(values = clrs_types) +
  labs(y=expression(hat(beta))) +
  theme(strip.text.y = element_text(angle=0),
        legend.position = 'none')
dev.off()

png('graphs_rolling/rolling_FF5MOM_RMW_bytype.png', width = 4000, height = 8000, res=720)
ff5_mom_type %>%
  filter(term == 'RMW', Date >= '2005-01-01') %>%
  ggplot(aes(x=Date)) +
  geom_line(aes(y=median, color=Type)) +
  geom_ribbon(aes(ymin=lo50, ymax=hi50, fill=Type), alpha = 0.2) +
  geom_ribbon(aes(ymin=lo80, ymax=hi80, fill=Type), alpha = 0.2) +
  geom_ribbon(aes(ymin=lo95, ymax=hi95, fill=Type), alpha = 0.2) +
  facet_grid(rows=vars(Type), scales = 'free_y') +
  scale_color_manual(values = clrs_types) +
  scale_fill_manual(values = clrs_types) +
  labs(y=expression(hat(beta))) +
  theme(strip.text.y = element_text(angle=0),
        legend.position = 'none')
dev.off()

png('graphs_rolling/rolling_FF5MOM_CMA_bytype.png', width = 4000, height = 8000, res=720)
ff5_mom_type %>%
  filter(term == 'CMA', Date >= '2005-01-01') %>%
  ggplot(aes(x=Date)) +
  geom_line(aes(y=median, color=Type)) +
  geom_ribbon(aes(ymin=lo50, ymax=hi50, fill=Type), alpha = 0.2) +
  geom_ribbon(aes(ymin=lo80, ymax=hi80, fill=Type), alpha = 0.2) +
  geom_ribbon(aes(ymin=lo95, ymax=hi95, fill=Type), alpha = 0.2) +
  facet_grid(rows=vars(Type), scales = 'free_y') +
  scale_color_manual(values = clrs_types) +
  scale_fill_manual(values = clrs_types) +
  labs(y=expression(hat(beta))) +
  theme(strip.text.y = element_text(angle=0),
        legend.position = 'none')
dev.off()

png('graphs_rolling/rolling_FF5MOM_MOM_bytype.png', width = 4000, height = 8000, res=720)
ff5_mom_type %>%
  filter(term == 'MOM', Date >= '2005-01-01') %>%
  ggplot(aes(x=Date)) +
  geom_line(aes(y=median, color=Type)) +
  geom_ribbon(aes(ymin=lo50, ymax=hi50, fill=Type), alpha = 0.2) +
  geom_ribbon(aes(ymin=lo80, ymax=hi80, fill=Type), alpha = 0.2) +
  geom_ribbon(aes(ymin=lo95, ymax=hi95, fill=Type), alpha = 0.2) +
  facet_grid(rows=vars(Type), scales = 'free_y') +
  scale_color_manual(values = clrs_types) +
  scale_fill_manual(values = clrs_types) +
  labs(y=expression(hat(beta))) +
  theme(strip.text.y = element_text(angle=0),
        legend.position = 'none')
dev.off()

png('graphs_rolling/rolling_FF5MOM_numfunuds_bytype.png', width = 8000, height = 4000, res=720)
ff5_mom_type %>%
  filter(term == '(Intercept)', Date >= '2005-01-01') %>%
  ggplot(aes(x=Date)) +
  geom_area(aes(y=n, fill=Type), position = 'stack', stat = 'identity') +
  scale_fill_manual(values=clrs_types)
dev.off()


# LBS graphs ----

names(clrs_betas) <- c('PTFSBD', 'PTFSCOM', 'PTFSFX', 'PTFSIR', 'PTFSSTK')

png('graphs_rolling/rolling_LBS_alpha_aggregate.png', width = 8000, height = 4000, res=720)
lbs_agg %>%
  filter(term == '(Intercept)', Date >= '2005-01-01') %>%
  ggplot(aes(x=Date)) +
  geom_line(aes(y=median), color = 'blue') +
  geom_ribbon(aes(ymin=lo50, ymax=hi50), fill='blue', alpha = 0.2) +
  geom_ribbon(aes(ymin=lo80, ymax=hi80), fill='blue', alpha = 0.2) +
  geom_ribbon(aes(ymin=lo95, ymax=hi95), fill='blue', alpha = 0.2) +
  labs(y=expression(hat(alpha)))
dev.off()

png('graphs_rolling/rolling_LBS_betas_aggregate.png', width = 8000, height = 4000, res=720)
lbs_agg %>%
  filter(term != '(Intercept)', Date >= '2005-01-01') %>%
  ggplot(aes(x=Date)) +
  geom_line(aes(y=median, color=term)) +
  geom_ribbon(aes(ymin=lo50, ymax=hi50, fill=term), alpha = 0.2) +
  geom_ribbon(aes(ymin=lo80, ymax=hi80, fill=term), alpha = 0.2) +
  geom_ribbon(aes(ymin=lo95, ymax=hi95, fill=term), alpha = 0.2) +
  facet_grid(rows = vars(term), scale = 'free_y') +
  scale_color_manual(values = clrs_betas[1:5]) +
  scale_fill_manual(values = clrs_betas[1:5]) +
  labs(y=expression(hat(beta))) +
  theme(strip.text.y = element_text(angle=0),
        legend.position = 'none')
dev.off()


png('graphs_rolling/rolling_LBS_alpha_bytype.png', width = 4000, height = 8000, res=720)
lbs_type %>%
  filter(term == '(Intercept)', Date >= '2005-01-01') %>%
  ggplot(aes(x=Date)) +
  geom_line(aes(y=median, color=Type)) +
  geom_ribbon(aes(ymin=lo50, ymax=hi50, fill=Type), alpha = 0.2) +
  geom_ribbon(aes(ymin=lo80, ymax=hi80, fill=Type), alpha = 0.2) +
  geom_ribbon(aes(ymin=lo95, ymax=hi95, fill=Type), alpha = 0.2) +
  facet_grid(rows=vars(Type), scales = 'free_y') +
  scale_color_manual(values = clrs_types) +
  scale_fill_manual(values = clrs_types) +
  labs(y=expression(hat(alpha))) +
  theme(strip.text.y = element_text(angle=0),
        legend.position = 'none')
dev.off()

png('graphs_rolling/rolling_LBS_PTFSBD_bytype.png', width = 4000, height = 8000, res=720)
lbs_type %>%
  filter(term == 'PTFSBD', Date >= '2005-01-01') %>%
  ggplot(aes(x=Date)) +
  geom_line(aes(y=median, color=Type)) +
  geom_ribbon(aes(ymin=lo50, ymax=hi50, fill=Type), alpha = 0.2) +
  geom_ribbon(aes(ymin=lo80, ymax=hi80, fill=Type), alpha = 0.2) +
  geom_ribbon(aes(ymin=lo95, ymax=hi95, fill=Type), alpha = 0.2) +
  facet_grid(rows=vars(Type), scales = 'free_y') +
  scale_color_manual(values = clrs_types) +
  scale_fill_manual(values = clrs_types) +
  labs(y=expression(hat(beta))) +
  theme(strip.text.y = element_text(angle=0),
        legend.position = 'none')
dev.off()

png('graphs_rolling/rolling_LBS_PTFSCOM_bytype.png', width = 4000, height = 8000, res=720)
lbs_type %>%
  filter(term == 'PTFSCOM', Date >= '2005-01-01') %>%
  ggplot(aes(x=Date)) +
  geom_line(aes(y=median, color=Type)) +
  geom_ribbon(aes(ymin=lo50, ymax=hi50, fill=Type), alpha = 0.2) +
  geom_ribbon(aes(ymin=lo80, ymax=hi80, fill=Type), alpha = 0.2) +
  geom_ribbon(aes(ymin=lo95, ymax=hi95, fill=Type), alpha = 0.2) +
  facet_grid(rows=vars(Type), scales = 'free_y') +
  scale_color_manual(values = clrs_types) +
  scale_fill_manual(values = clrs_types) +
  labs(y=expression(hat(beta))) +
  theme(strip.text.y = element_text(angle=0),
        legend.position = 'none')
dev.off()

png('graphs_rolling/rolling_LBS_PTFSFX_bytype.png', width = 4000, height = 8000, res=720)
lbs_type %>%
  filter(term == 'PTFSFX', Date >= '2005-01-01') %>%
  ggplot(aes(x=Date)) +
  geom_line(aes(y=median, color=Type)) +
  geom_ribbon(aes(ymin=lo50, ymax=hi50, fill=Type), alpha = 0.2) +
  geom_ribbon(aes(ymin=lo80, ymax=hi80, fill=Type), alpha = 0.2) +
  geom_ribbon(aes(ymin=lo95, ymax=hi95, fill=Type), alpha = 0.2) +
  facet_grid(rows=vars(Type), scales = 'free_y') +
  scale_color_manual(values = clrs_types) +
  scale_fill_manual(values = clrs_types) +
  labs(y=expression(hat(beta))) +
  theme(strip.text.y = element_text(angle=0),
        legend.position = 'none')
dev.off()

png('graphs_rolling/rolling_LBS_PTFSIR_bytype.png', width = 4000, height = 8000, res=720)
lbs_type %>%
  filter(term == 'PTFSIR', Date >= '2005-01-01') %>%
  ggplot(aes(x=Date)) +
  geom_line(aes(y=median, color=Type)) +
  geom_ribbon(aes(ymin=lo50, ymax=hi50, fill=Type), alpha = 0.2) +
  geom_ribbon(aes(ymin=lo80, ymax=hi80, fill=Type), alpha = 0.2) +
  geom_ribbon(aes(ymin=lo95, ymax=hi95, fill=Type), alpha = 0.2) +
  facet_grid(rows=vars(Type), scales = 'free_y') +
  scale_color_manual(values = clrs_types) +
  scale_fill_manual(values = clrs_types) +
  labs(y=expression(hat(beta))) +
  theme(strip.text.y = element_text(angle=0),
        legend.position = 'none')
dev.off()

png('graphs_rolling/rolling_LBS_PTFSSTK_bytype.png', width = 4000, height = 8000, res=720)
lbs_type %>%
  filter(term == 'PTFSSTK', Date >= '2005-01-01') %>%
  ggplot(aes(x=Date)) +
  geom_line(aes(y=median, color=Type)) +
  geom_ribbon(aes(ymin=lo50, ymax=hi50, fill=Type), alpha = 0.2) +
  geom_ribbon(aes(ymin=lo80, ymax=hi80, fill=Type), alpha = 0.2) +
  geom_ribbon(aes(ymin=lo95, ymax=hi95, fill=Type), alpha = 0.2) +
  facet_grid(rows=vars(Type), scales = 'free_y') +
  scale_color_manual(values = clrs_types) +
  scale_fill_manual(values = clrs_types) +
  labs(y=expression(hat(beta))) +
  theme(strip.text.y = element_text(angle=0),
        legend.position = 'none')
dev.off()

png('graphs_rolling/rolling_LBS_numfunuds_bytype.png', width = 8000, height = 4000, res=720)
lbs_type %>%
  filter(term == '(Intercept)', Date >= '2005-01-01') %>%
  ggplot(aes(x=Date)) +
  geom_area(aes(y=n, fill=Type), position = 'stack', stat = 'identity') +
  scale_fill_manual(values=clrs_types)
dev.off()





