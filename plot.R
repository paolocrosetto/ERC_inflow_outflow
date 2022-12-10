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
  mutate(nation = if_else(nation == "UK", "GB", nation),
         nation = if_else(nation == "IL", "IE", nation)) %>% 
  mutate(country = str_to_lower(nation)) %>% 
  mutate(group = case_when(nation %in% c("AT", "CH") ~ "ATCH",
                           nation %in% c("IT") ~ "IT",
                           nation %in% c("IE") ~ "IE",
                           nation %in% c("NL", "SE", "GB", "DK") ~ "nordic",
                           TRUE ~ "core")) %>% 
  mutate(label = case_when(group == "ATCH" ~ "Small countries\nwith large inflows",
                           group == "core" ~ "The core of Europe\n~25% in, ~25% out",
                           group == "nordic" ~ "More inflow than outflow",
                           group == "IE" ~ "Rather closed",
                           group == "IT" ~ "Large outflow, very little inflow"))

# plot
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
       title = "<span style = 'font-size:24pt'>Does your country _attract_ or _lose_ top-class researchers?</span>",
       subtitle = "ERC grantees **inflow** and **outflow**, _2014-20_")+
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
ggsave("ERC.png", width = 16/1.6, height = 9/1.6, units = "in", dpi = 320)

