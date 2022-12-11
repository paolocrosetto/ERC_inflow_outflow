library(tidyverse)
library(hrbrthemes)
library(ggflags)
library(ggforce)
library(ggtext)


# getting the data 
df <- read_csv("Data/data.csv")

## share outflow, share inflow
inflow <- df %>% 
  filter(nationality %in% c(host, "Other")) %>%
  mutate(self = host == nationality) %>% 
  group_by(date, host, self) %>% 
  summarise(N = sum(N)) %>% 
  mutate(inflow_sh = N/sum(N)) %>% 
  select(-N) %>% 
  pivot_wider(names_from = self, values_from = inflow_sh) %>% 
  rename(ERC_incoming = `FALSE`, ERC_staying = `TRUE`, nation = host)

outflow <- df %>% 
  filter(nationality %in% c(host, "Other")) %>%
  mutate(self = host == nationality) %>% 
  group_by(date, nationality, self) %>% 
  summarise(N = sum(N)) %>% 
  mutate(outflow_sh = N/sum(N)) %>% 
  select(-N) %>% 
  pivot_wider(names_from = self, values_from = outflow_sh) %>% 
  rename(ERC_abroad = `FALSE`, ERC_home = `TRUE`, nation = nationality)

# raw data
data <- inflow %>% 
  left_join(outflow, by = c("date", "nation"))


# formatting data and labels for plotting
plot_data <- data %>% 
  select(-ERC_staying, - ERC_home) %>% 
  pivot_wider(names_from = "date", values_from = c(ERC_incoming, ERC_abroad)) %>% 
  mutate(nation = if_else(nation == "UK", "GB", nation)) %>% 
  mutate(country = str_to_lower(nation)) %>% 
  mutate(group = case_when(nation %in% c("AT", "CH") ~ "ATCH",
                           nation %in% c("IT") ~ "IT",
                           nation %in% c("IL") ~ "IL",
                           nation %in% c("NL", "SE", "GB", "DK") ~ "nordic",
                           TRUE ~ "core")) %>% 
  mutate(label = case_when(group == "ATCH" ~ "Small countries\nwith large inflows",
                           group == "core" ~ "The core of Europe\n~25% in, ~25% out",
                           group == "nordic" ~ "More inflow than outflow",
                           group == "IL" ~ "Rather closed",
                           group == "IT" ~ "Large outflow, very little inflow"))


#### plot 1: scatter of inflow and outflow, with flags ####
ggplot(plot_data)+
  ## main plot: flags + annotation
  aes(x = `ERC_incoming_2014-20`, y = `ERC_abroad_2014-20`, country = country, image = country)+
  geom_abline(slope = 1, intercept = 0, color = "#bebada", linetype = "dashed")+
  geom_flag(size = 12)+
  geom_mark_hull(aes(group = group, description = label), 
                          expand = unit(6, "mm"),
                          label.fill = NULL, label.fontsize = c(10,9), label.buffer = unit(2, "mm"),
                          con.colour = "grey60", con.cap = unit(1.3, "mm"))+
  coord_cartesian(xlim = c(0,0.7), ylim = c(-0.05,0.7))+
  ## scales, labels and theme
  scale_x_continuous(labels = scales::percent)+
  scale_y_continuous(labels = scales::percent)+
  labs(y= "<span style = 'font-size:18pt'>**Outflow**</span><br><span style = 'font-size:9pt'> % of national ERC grantees abroad</span>",
       x = "<span style = 'font-size:18pt'>**Inflow**</span><br><span style = 'font-size:9pt'> % of foreign ERC grantees in nation</span>",
       caption = "analysis @paolocrosetto",
       title = "<span style = 'font-size:24pt'>Does your country _attract_ or _lose_ ERC grantees?</span>",
       subtitle = "ERC grantees **inflow** and **outflow** for the 12 largest ERC host countries, _2014-20_")+
  theme_ipsum_rc()+
  theme(legend.position = "bottom",
        panel.grid.minor = element_blank(),
        plot.title.position = "plot",
        axis.title.x = element_markdown(),
        axis.title.y = element_markdown(),
        plot.title = element_markdown(),
        plot.subtitle = element_markdown(hjust = 1),
        plot.background = element_rect(colour = "white", fill = "white")
        ) 

## saving the plot
ggsave("Plots/ERC.png", width = 16/1.6, height = 9/1.6, units = "in", dpi = 320)


#### plot 2: scatter of inflow and outflow, with flags scaled by number of ERC hosted projects ####
grants <- df %>% 
  filter(date == "2014-20") %>% 
  mutate(host = if_else(host == "UK", "GB", host)) %>% 
  group_by(host) %>% 
  summarise(N = sum(N))

plot_data %>% 
  left_join(grants, by = c("nation" = "host")) %>% 
  ggplot()+
  aes(x = `ERC_incoming_2014-20`, y = `ERC_abroad_2014-20`, country = country, image = country)+
  geom_abline(slope = 1, intercept = 0, color = "#bebada", linetype = "dashed")+
  geom_flag(aes(size = N))+
  geom_mark_hull(aes(group = group, description = label), 
                 expand = unit(6, "mm"),
                 label.fill = NULL, label.fontsize = c(10,9), label.buffer = unit(2, "mm"),
                 con.colour = "grey60", con.cap = unit(1.3, "mm"))+
  coord_cartesian(xlim = c(0,0.7), ylim = c(-0.05,0.7))+
  ## scales, labels and theme
  scale_x_continuous(labels = scales::percent)+
  scale_y_continuous(labels = scales::percent)+
  scale_size(range = c(1,14))+
  labs(y= "<span style = 'font-size:18pt'>**Outflow**</span><br><span style = 'font-size:9pt'> % of national ERC grantees abroad</span>",
       x = "<span style = 'font-size:18pt'>**Inflow**</span><br><span style = 'font-size:9pt'> % of foreign ERC grantees in nation</span>",
       caption = "analysis @paolocrosetto",
       title = "<span style = 'font-size:24pt'>Does your country _attract_ or _lose_ top-class researchers?</span>",
       subtitle = "ERC grantees **inflow** and **outflow** for the 12 largest ERC host countries, _2014-20_. Flag size indicates # of ERC grants received.")+
  theme_ipsum_rc()+
  theme(legend.position = "none",
        panel.grid.minor = element_blank(),
        plot.title.position = "plot",
        axis.title.x = element_markdown(),
        axis.title.y = element_markdown(),
        plot.title = element_markdown(),
        plot.subtitle = element_markdown(hjust = 1),
        plot.background = element_rect(colour = "white", fill = "white")
  ) 

ggsave("Plots/ERC_size.png", width = 16/1.6, height = 9/1.6, units = "in", dpi = 320)


#### plot 3: dumbbell of evolution of inflow and outflow over the two time periods ####


inflow_plot <- plot_data %>%
  mutate(sign = `ERC_incoming_2014-20` - `ERC_incoming_2007-13` > 0) %>% 
  ggplot() +
  aes(y = reorder(nation, `ERC_incoming_2014-20`), x = `ERC_incoming_2007-13`) +
  #geom_point()+
  #geom_point(aes(x = `ERC_incoming_2014-20`))+
  geom_segment(aes(y = reorder(nation, `ERC_incoming_2014-20`), yend = reorder(nation, `ERC_incoming_2014-20`),
                   x = `ERC_incoming_2007-13`, xend = `ERC_incoming_2014-20`, color = sign), 
               arrow = arrow(angle = 30, length = unit(0.10, "inches"),
                             ends = "last", type = "closed"), 
               lineend = "round",
               size = 2)+
  geom_flag(aes(x = -0.1, country = country), size = 10)+
  scale_x_continuous(labels = scales::percent)+
  scale_color_manual(name = "", values = c("#E73F12", "#018241"))+
  labs(x = "<span style = 'font-size:18pt'>**changes in inflow**</span><br><span style = 'font-size:9pt'> % of foreign ERC grantees in nation</span>",
       y = "",
       title = "<span style = 'font-size:24pt'> Inflow </span>")+
  hrbrthemes::theme_ipsum_rc()+
  theme(legend.position = "none",
        panel.grid.minor = element_blank(),
        plot.title.position = "plot",
        axis.title.x = element_markdown(),
        axis.text.y = element_blank(),
        plot.title = element_markdown(),
        plot.subtitle = element_markdown(hjust = 1),
        plot.background = element_rect(colour = "white", fill = "white"))



outflow_plot <- plot_data %>%
  mutate(sign = `ERC_abroad_2014-20` - `ERC_abroad_2007-13` < 0) %>% 
  ggplot() +
  aes(y = reorder(nation, `ERC_abroad_2014-20`), x = `ERC_abroad_2007-13`) +
  #geom_point()+
  #geom_point(aes(x = `ERC_abroad_2014-20`))+
  geom_segment(aes(y = reorder(nation, `ERC_abroad_2014-20`), yend = reorder(nation, `ERC_abroad_2014-20`),
                   x = `ERC_abroad_2007-13`, xend = `ERC_abroad_2014-20`, color = sign), 
               arrow = arrow(angle = 30, length = unit(0.10, "inches"),
                             ends = "last", type = "closed"), 
               lineend = "round",
               size = 2)+
  geom_flag(aes(x = -0.1, country = country), size = 10)+
  scale_x_continuous(labels = scales::percent)+
  scale_color_manual(name = "", values = c("#E73F12", "#018241"))+
  labs(x = "<span style = 'font-size:18pt'>**changes in outflow**</span><br><span style = 'font-size:9pt'> % of national ERC grantees abroad</span>",
       y = "",
       caption = "no data for Sweden in 2014-20",
       title = "<span style = 'font-size:24pt'>Outflow</span>")+
  hrbrthemes::theme_ipsum_rc()+
  theme(legend.position = "none",
        panel.grid.minor = element_blank(),
        plot.title.position = "plot",
        axis.title.x = element_markdown(),
        axis.text.y = element_blank(),
        plot.title = element_markdown(),
        plot.subtitle = element_markdown(hjust = 1),
        plot.background = element_rect(colour = "white", fill = "white"))

library(patchwork)
inflow_plot+outflow_plot + 
  plot_annotation(title = "<span style = 'font-size:24pt'>How did ERC grantees inflow and outflow change over time?</span>", 
                  subtitle = "Change in _inflow_ & _outflow_, _2007-13_ to _2014-20_, 12 largest ERC host countries", 
                  caption = "analysis @paolocrosetto",
                  theme = theme(plot.title = element_markdown(face = "bold", family = "Roboto Condensed"),
                                plot.subtitle = element_markdown(family = "Roboto condensed", 
                                                                 hjust = 1, size = 14, face = "plain")))
ggsave("Plots/inflow_outflow.png", width = 16/1.1, height = 9/1.1, units = "in", dpi = 320)

#### plot 4: a bit chaotic but this is a chord diagram ####
library(ggalluvial)

df %>% 
  filter(date == "2014-20") %>% 
  filter(nationality %in% c(host, "Other")) %>%
  ggplot(aes(y = N, axis1 = nationality, axis2 = host)) +
  geom_alluvium(aes(fill = host), width = 0.5/12, color = "black", size = .3) +
  geom_stratum(aes(fill = host), width = 1/12)+
  geom_label(stat = "stratum", aes(label = after_stat(stratum)))+
  scale_x_discrete(expand = c(.05, .05))+
  labs(y = "", 
       title = "Researchers of nation (left) running an ERC in country (right)",
       subtitle = "ERC data, _2014-20_, top 12 ERC host countries",
       caption = "analysis @paolocrosetto")+
  #paletteer::scale_fill_paletteer_d("ggthemes::Classic_Cyclic")+
  theme_ipsum_rc()+
  theme(legend.position = "none",
        axis.title = element_blank(),
        axis.text.y = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        plot.title = element_markdown(),
        plot.subtitle = element_markdown(hjust = 1),
        plot.background = element_rect(fill = "white", color = "white"))
ggsave("Plots/alluvial.png", height = 16/1.2, width = 9/1.2, units = "in", dpi = 320)



