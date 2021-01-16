library(tidyverse)
library(broom)
library(lubridate)
library(RColorBrewer)
theme_set(theme_minimal())

load('CTAdb.RData')

capm = df %>% 
  filter(between(Date, capm_mindate, capm_maxdate)) %>% # last capm observation
  group_by(ID, Program, Manager, Type, Style, Strategy, Sector) %>%
  filter(sum(!is.na(Return)) > 1) %>% # at least 2 observations
  nest() %>%
  mutate(
    model = 'CAPM',
    fit = map(data, ~ lm(Return - RF ~ Mkt.RF, data=.x)),
    tidied = map(fit, tidy),
    glanced = map(fit, glance)
  ) %>%
  select(-c(data, fit))

ff3 = df %>% 
  filter(between(Date, ff3_mindate, ff3_maxdate)) %>% # last ff3 observation
  group_by(ID, Program, Manager, Type, Style, Strategy, Sector) %>%
  filter(sum(!is.na(Return)) > 3) %>% # at least 4 observations
  nest() %>%
  mutate(
    model = 'FF3',
    fit = map(data, ~ lm(Return -RF ~ Mkt.RF+SMB+HML, data=.x)),
    tidied = map(fit, tidy),
    glanced = map(fit, glance)
  ) %>%
  select(-c(data, fit))

ff5 = df %>% 
  filter(between(Date, ff5_mindate, ff5_maxdate)) %>% # last ff5 observation
  group_by(ID, Program, Manager, Type, Style, Strategy, Sector) %>%
  filter(sum(!is.na(Return)) > 5) %>% # at least 6 observations
  nest() %>%
  mutate(
    model = 'FF5',
    fit = map(data, ~ lm(Return - RF ~ Mkt.RF+SMB+HML+RMW+CMA, data=.x)),
    tidied = map(fit, tidy),
    glanced = map(fit, glance)
  ) %>%
  select(-c(data, fit))

ff5_mom = df %>% 
  filter(between(Date, ff5_mom_mindate, ff5_mom_maxdate)) %>% # last MOM observation
  group_by(ID, Program, Manager, Type, Style, Strategy, Sector) %>%
  filter(sum(!is.na(Return)) > 6) %>% # at least 7 observations
  nest() %>%
  mutate(
    model = 'FF5+MOM',
    fit = map(data, ~ lm(Return - RF ~ Mkt.RF+SMB+HML+RMW+CMA+MOM, 
                         data=.x)),
    tidied = map(fit, tidy),
    glanced = map(fit, glance)
  ) %>%
  select(-c(data, fit))

lbs = df %>% 
  filter(between(Date, lbs_mindate, lbs_maxdate)) %>% # last lbs observation
  group_by(ID, Program, Manager, Type, Style, Strategy, Sector) %>%
  filter(sum(!is.na(Return)) > 5) %>% # at least 6 observations
  nest() %>%
  mutate(
    model = 'LBS',
    fit = map(data, ~ lm(Return - RF ~ PTFSBD+PTFSFX+PTFSCOM+PTFSIR+PTFSSTK, data=.x)),
    tidied = map(fit, tidy),
    glanced = map(fit, glance)
  ) %>%
  select(-c(data, fit))


factor_models = rbind(capm, ff3, ff5, ff5_mom, lbs) %>% filter(!is.na(Type))

clrs_betas <- brewer.pal(6, 'Dark2')
names(clrs_betas) <- c('Mkt.RF', 'SMB', 'HML', 'RMW', 'CMA', 'MOM')

png('graphs/CAPM_beta.png', width = 8000, height = 4000, res=720)
factor_models %>% unnest(tidied) %>%
  filter(model == 'CAPM', term != '(Intercept)') %>%
  ggplot(aes(x = estimate, color=term)) +
  geom_density(key_glyph = draw_key_abline) +
  facet_wrap(~ Type, scales = 'free_y') + 
  xlim(c(-1.5,1.5)) +
  labs(x=expression(hat(beta)),
       y='Density') +
  scale_color_manual(name = 'Factor', values = clrs_betas) +
  theme(axis.text.y = element_blank(),
        plot.title = element_text(hjust = 0.5))
dev.off()

png('graphs/FF3_betas.png', width = 8000, height = 4000, res=720)
factor_models %>% unnest(tidied) %>%
  filter(model == 'FF3', term != '(Intercept)') %>%
  ggplot(aes(x = estimate, color = term)) +
  geom_density(key_glyph = draw_key_abline) +
  facet_wrap(~ Type, scales = 'free_y') + 
  xlim(c(-1.5,1.5)) +
  labs(x=expression(hat(beta)),
       y='Density') +
  scale_color_manual(name = 'Factor', 
                     values = clrs_betas) +
  theme(axis.text.y = element_blank(),
        plot.title = element_text(hjust = 0.5))
dev.off()

png('graphs/FF5_betas.png', width = 8000, height = 4000, res=720)
factor_models %>% unnest(tidied) %>%
  filter(model == 'FF5', term != '(Intercept)') %>%
  ggplot(aes(x = estimate, color = term)) +
  geom_density(key_glyph = draw_key_abline) +
  facet_wrap(~ Type, scales = 'free_y') + 
  xlim(c(-1.5,1.5)) +
  labs(x=expression(hat(beta)),
       y='Density') +
  scale_color_manual(name = 'Factor', 
                     values = clrs_betas) +
  theme(axis.text.y = element_blank(),
        plot.title = element_text(hjust = 0.5))
dev.off()

png('graphs/FF5_MOM_betas.png', width = 8000, height = 4000, res=720)
factor_models %>% unnest(tidied) %>%
  filter(model == 'FF5+MOM', term != '(Intercept)') %>%
  ggplot(aes(x = estimate, color = term)) +
  geom_density(key_glyph = draw_key_abline) +
  facet_wrap(~ Type, scales = 'free_y') + 
  xlim(c(-1.5,1.5)) +
  labs(x=expression(hat(beta)),
       y='Density') +
  scale_color_manual(name = 'Factor', 
                     values = clrs_betas) +
  theme(axis.text.y = element_blank(),
        plot.title = element_text(hjust = 0.5))
dev.off()

png('graphs/LBS_betas_zoomout.png', width = 8000, height = 4000, res=720)
factor_models %>% unnest(tidied) %>%
  filter(model == 'LBS', term != '(Intercept)') %>%
  ggplot(aes(x = estimate, color = term)) +
  geom_density(key_glyph = draw_key_abline) +
  facet_wrap(~ Type, scales = 'free_y') + 
  xlim(c(-1, 1)) +
  labs(x=expression(hat(beta)),
       y='Density') +
  scale_color_manual(name = 'Factor', 
                     values = unname(clrs_betas)) +
  theme(axis.text.y = element_blank(),
        plot.title = element_text(hjust = 0.5))
dev.off()

png('graphs/LBS_betas_zoomin.png', width = 8000, height = 4000, res=720)
factor_models %>% unnest(tidied) %>%
  filter(model == 'LBS', term != '(Intercept)') %>%
  ggplot(aes(x = estimate, color = term)) +
  geom_density(key_glyph = draw_key_abline) +
  facet_wrap(~ Type, scales = 'free_y') + 
  xlim(c(-0.1, 0.1)) +
  labs(x=expression(hat(beta)),
       y='Density') +
  scale_color_manual(name = 'Factor', 
                     values = unname(clrs_betas)) +
  theme(axis.text.y = element_blank(),
        plot.title = element_text(hjust = 0.5))
dev.off()

clrs_models <- brewer.pal(5, 'Set1')
names(clrs_models) <- c('CAPM', 'FF3', 'FF5', 'FF5+MOM', 'LBS')

png('graphs/alphas_fixed_x.png', width = 8000, height = 4000, res=720)
factor_models %>% unnest(tidied) %>%
  filter(term == '(Intercept)') %>%
  ggplot(aes(x = estimate, color = model)) +
  geom_density(key_glyph = draw_key_abline) +
  xlim(c(-0.03,0.03)) +
  facet_wrap(~ Type, scales = 'free_y') + 
  labs(x=expression(hat(alpha)),
       y='Density') +
  scale_color_manual(name = 'Factor Model', 
                     values = clrs_models) +
  theme(axis.text.y = element_blank(),
        plot.title = element_text(hjust = 0.5))
dev.off()

png('graphs/alphas_free_x.png', width = 8000, height = 4000, res=720)
factor_models %>% unnest(tidied) %>%
  filter(term == '(Intercept)') %>%
  ggplot(aes(x = estimate, color = model)) +
  geom_density(key_glyph = draw_key_abline) +
  facet_wrap(~ Type, scales = 'free') + 
  labs(x=expression(hat(alpha)),
       y='Density') +
  scale_color_manual(name = 'Factor Model', 
                     values = clrs_models) +
  theme(axis.text.y = element_blank(),
        plot.title = element_text(hjust = 0.5))
dev.off()

png('graphs/R2.png', width = 8000, height = 4000, res=720)
factor_models %>% unnest(glanced) %>%
  ggplot(aes(x = r.squared, color = model)) +
  geom_density(key_glyph = draw_key_abline) +
  facet_wrap(~ Type, scales = 'fixed') + 
  labs(x=expression(R^2),
       y='Density') +
  scale_color_manual(name = 'Factor Model', 
                     values = clrs_models) +
  theme(axis.text.y = element_blank(),
        plot.title = element_text(hjust = 0.5))
dev.off()





