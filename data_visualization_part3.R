

#--------------------------------------------------------------------------------
#                Bubble Chart
# Tham khảo trang 38: https://www.amazon.com/Great-Escape-Health-Origins-Inequality/dp/0691165629
#--------------------------------------------------------------------------------

# Load các gói và lấy dữ liệu  từ World Bank: 
library(WDI)
mydf <- WDI(country = "all", 
            start = 2016, 
            end = 2017, 
            indicator = c("SP.POP.TOTL", 
                          # Tuổi thọ bình quân: 
                          "SP.DYN.LE00.IN", 
                          # GDP đầu người: 
                          "NY.GDP.PCAP.PP.CD"))

# Xem qua dữ liệu: 
head(mydf)

# Chỉ lấy dữ liệu năm 2016  đồng thời đổi tên cho một số cột biến: 
library(tidyverse)
library(magrittr)

mydf_small <- mydf %>% 
  filter(year == 2016) %>% 
  rename(pop = SP.POP.TOTL, 
         life = SP.DYN.LE00.IN, 
         gdp = NY.GDP.PCAP.PP.CD)

sapply(mydf_small, class)

# Lấy dữ liệu về phân loại nhóm quốc gia (mất chừng 1 phút tùy chất lượng mạng): 
d <- WDIcache() 

# Chú ý rằng d là một list: 
d %>% str()

# Khai thác DF thứ 2: 
d2 <- d[[2]] %>% as.data.frame()
names(d2)

# Xem qua: 
d2$income %>% unique()

# Lấy ra các cột biến cần thiết: 
income_group <- d2 %>% 
  mutate_if(is.factor, as.character) %>% 
  filter(income != "Aggregates") %>% 
  select(iso2c, region, income)

# Ghép hai  DF và loại NA: 

total_df <- right_join(mydf_small, income_group, by = "iso2c") %>% na.omit()

# Nhóm thu nhập: 
total_df$income %>% table()
total_df %>% 
  group_by(income) %>% 
  count()

# Vẽ phác thảo: 
theme_set(theme_minimal())
total_df %>% 
  ggplot(aes(gdp, life)) + 
  geom_point()

# Xem qua: 
total_df$gdp %>% range()

# Nếu thế thì: 
library(scales)

total_df %>% 
  filter(gdp < 75000) %>% 
  ggplot(aes(gdp, life)) + 
  geom_point() + 
  scale_x_continuous(breaks = seq(0, 75000, 10000), labels = dollar) + 
  scale_y_continuous(breaks = seq(50, 85, 5))

# Biểu diễn thêm quy mô dân số: 

total_df %>% 
  filter(gdp < 75000) %>% 
  ggplot(aes(gdp, life, size = pop, color = income)) + 
  geom_point() + 
  scale_x_continuous(breaks = seq(0, 75000, 10000), labels = dollar) + 
  scale_y_continuous(breaks = seq(50, 85, 5)) 


# Cải tiến hơn: 

total_df %>% 
  filter(gdp < 75000) %>% 
  ggplot(aes(gdp, life, size = pop, color = income)) + 
  geom_point(alpha = 0.3) + 
  scale_x_continuous(breaks = seq(0, 75000, 10000), labels = dollar) + 
  scale_y_continuous(breaks = seq(50, 85, 5)) + 
  scale_size(range = c(1, 30)) + 
  scale_color_discrete(name = "Income Group:") + 
  guides(size = FALSE) + 
  theme(legend.position = c(0.859, 0.26)) + 
  theme(legend.title = element_text(size = 12, face = "bold")) + 
  theme(legend.background = element_rect(fill = "#fff7ec", size = 0.2, color = "grey70")) + 
  labs(x = "GDP per capital", 
       y = "Life expectancy", 
       title = "The relationship between Life Expectancy and GDP per capital in 2016", 
       subtitle = "According to WHO definitions, Life Expectancy at reflects the overall mortality level of\na population and it is defined as the average number of years that a newborn is\nexpected to live if current mortality rates continue to apply. ", 
       caption =  "Data Source: The World Bank")


# Hoặc như trang 38 sách của A. Deaton: 

total_df %>% 
  filter(gdp < 75000) %>% 
  ggplot(aes(gdp, life, size = pop, color = income)) + 
  geom_smooth(aes(group = 1), method = "lm", formula = y ~ log(x), 
              color = "grey40", alpha = 0.1, se = FALSE) + 
  geom_point(alpha = 0.3) + 
  scale_x_continuous(breaks = seq(0, 75000, 10000), labels = dollar) + 
  scale_y_continuous(breaks = seq(50, 85, 5)) + 
  scale_size(range = c(1, 30)) + 
  scale_color_discrete(name = "Income Group:") + 
  guides(size = FALSE) + 
  theme(legend.position = c(0.859, 0.26)) + 
  theme(legend.title = element_text(size = 12, face = "bold")) + 
  theme(legend.background = element_rect(fill = "#fff7ec", size = 0.2, color = "grey70")) + 
  labs(x = "GDP per capital", 
       y = "Life expectancy", 
       title = "The relationship between Life Expectancy and GDP per capital in 2016", 
       subtitle = "According to WHO definitions, Life Expectancy at reflects the overall mortality level of\na population and it is defined as the average number of years that a newborn is\nexpected to live if current mortality rates continue to apply. ", 
       caption =  "Data Source: The World Bank")


# Một phiên bản khác: 


total_df %>% 
  filter(gdp < 75000) %>% 
  ggplot(aes(gdp, life, size = pop, color = income)) + 
  geom_smooth(aes(group = 1), method = "lm", formula = y ~ log(x), 
              color = "grey40", alpha = 0.1, se = FALSE) + 
  geom_point(alpha = 0.3) + 
  scale_x_log10(labels = dollar) + 
  scale_y_continuous(breaks = seq(50, 85, 5)) + 
  scale_size(range = c(1, 30)) + 
  scale_color_discrete(name = "Income Group:") + 
  guides(size = FALSE) + 
  theme(legend.position = c(0.15, 0.819)) + 
  theme(legend.title = element_text(size = 12, face = "bold")) + 
  theme(legend.background = element_rect(fill = "#fff7ec", size = 0.2, color = "grey70")) + 
  labs(x = "GDP per capital", 
       y = "Life expectancy", 
       title = "The relationship between Life Expectancy and GDP per capital in 2016", 
       subtitle = "According to WHO definitions, Life Expectancy at reflects the overall mortality level of\na population and it is defined as the average number of years that a newborn is\nexpected to live if current mortality rates continue to apply. ", 
       caption =  "Data Source: The World Bank")
  

# Hoặc kiểu khác nữa: 
total_df %>% 
  filter(gdp < 75000) %>% 
  ggplot(aes(gdp, life, size = pop, color = income)) + 
  geom_smooth(aes(group = 1), method = "lm", formula = y ~ log(x), 
              color = "grey40", alpha = 0.1, se = FALSE) + 
  geom_point(alpha = 0.3) + 
  scale_x_log10(labels = dollar, breaks = seq(0, 80000, 20000)) + 
  scale_y_continuous(breaks = seq(50, 85, 5)) + 
  scale_size(range = c(1, 30)) + 
  scale_color_discrete(name = "Income Group:") + 
  guides(size = FALSE) + 
  theme(legend.position = c(0.15, 0.819)) + 
  theme(legend.title = element_text(size = 12, face = "bold")) + 
  theme(legend.background = element_rect(fill = "#fff7ec", size = 0.2, color = "grey70")) + 
  labs(x = "GDP per capital", 
       y = "Life expectancy", 
       title = "The relationship between Life Expectancy and GDP per capital in 2016", 
       subtitle = "According to WHO definitions, Life Expectancy at reflects the overall mortality level of\na population and it is defined as the average number of years that a newborn is\nexpected to live if current mortality rates continue to apply. ", 
       caption =  "Data Source: The World Bank")

# Muốn nhấn mạnh đến một số quốc gia: 

my_country <- c("Vietnam", "China", "India", "Thailand", "Malaysia", 
                "Germany", "Japan", "Nigeria", "Indonesia")

library(ggrepel)

total_df %>% 
  filter(gdp < 75000) %>% 
  ggplot(aes(gdp, life, size = pop, color = income)) + 
  geom_smooth(aes(group = 1), method = "lm", formula = y ~ log(x), 
              color = "grey40", alpha = 0.1, se = FALSE) + 
  geom_point(alpha = 0.3) + 
  geom_text_repel(data = total_df %>% filter(country %in% my_country), 
            aes(label = country), color = "gray20", size = 3.5, force = 19) + 
  scale_x_continuous(breaks = seq(0, 75000, 10000), labels = dollar) + 
  scale_y_continuous(breaks = seq(50, 85, 5)) + 
  scale_size(range = c(1, 30)) + 
  scale_color_discrete(name = "Income Group:") + 
  guides(size = FALSE) + 
  theme(legend.position = c(0.83, 0.22)) + 
  theme(legend.title = element_text(size = 12, face = "bold")) + 
  theme(legend.background = element_rect(fill = "#fff7ec", size = 0.2, color = "grey70")) + 
  labs(x = "GDP per capital", 
       y = "Life expectancy", 
       title = "The relationship between Life Expectancy and GDP per capital in 2016", 
       subtitle = "According to WHO definitions, Life Expectancy at reflects the overall mortality level of\na population and it is defined as the average number of years that a newborn is\nexpected to live if current mortality rates continue to apply. ", 
       caption =  "Data Source: The World Bank") 

# Thay đổi một số theme chẳng hạn: 
library(ggthemes)

total_df %>% 
  filter(gdp < 75000) %>% 
  ggplot(aes(gdp, life, size = pop, color = income)) + 
  geom_smooth(aes(group = 1), method = "lm", formula = y ~ log(x), 
              color = "grey40", alpha = 0.1, se = FALSE) + 
  geom_point(alpha = 0.3) + 
  geom_text_repel(data = total_df %>% filter(country %in% my_country), 
                  aes(label = country), color = "gray20", size = 3.5, force = 19) + 
  scale_x_continuous(breaks = seq(0, 75000, 10000), labels = dollar) + 
  scale_y_continuous(breaks = seq(50, 85, 5)) + 
  scale_size(range = c(1, 30)) + 
  guides(size = FALSE) + 
  labs(x = "GDP per capital", 
       y = "Life expectancy", 
       title = "The relationship between Life Expectancy and GDP per capital in 2016", 
       subtitle = "According to WHO definitions, Life Expectancy at reflects the overall mortality level of\na population and it is defined as the average number of years that a newborn is\nexpected to live if current mortality rates continue to apply. ", 
       caption =  "Data Source: The World Bank") + 
  theme_fivethirtyeight() + 
  scale_color_discrete(name = "Income Group:") + 
  theme(legend.title = element_text(size = 12, face = "bold")) + 
  theme(legend.position = c(0.882, 0.27)) + 
  theme(legend.direction = "vertical") + 
  theme(legend.background = element_rect(fill = "#fff7ec", size = 0.2, color = "grey70")) 
  
#-------------------------------------
#            Line Plot
#-------------------------------------

# Vẽ đơn giản: 
economics %>% 
  ggplot(aes(date, psavert)) + 
  geom_line() 

# Nhấn mạnh một thời điểm: 

economics %>% 
  ggplot(aes(date, psavert)) + 
  geom_line() + 
  geom_point(data = economics %>% slice(which.max(psavert)), 
             aes(date, psavert), color = "red", size = 3) + 
  annotate("text", 
           label = "The highest saving rate was 17% in 2000.", 
           x = as.Date("1978-01-01"), y = 17.3, 
           size = 4, hjust = 0, vjust = 1)

# Cải tiến hơn: 

economics %>% 
  mutate(psavert = 0.01*psavert) ->> my_economics


my_economics %>% 
  ggplot(aes(date, psavert)) + 
  geom_line() + 
  geom_point(data = my_economics %>% slice(which.max(psavert)), 
             aes(date, psavert), color = "red", size = 3) + 
  annotate("text", 
           label = "The highest saving rate was 17% in 2000.", 
           x = as.Date("1978-01-01"), y = 0.173, 
           size = 4, hjust = 0, vjust = 1, color = "grey40") ->> p

p

# Lại cải tiến: 
my_economics$psavert %>% range()

p + 
  scale_y_continuous(labels = percent, breaks = seq(0.015, 0.20, 0.025))


# Cải tiến nữa: 
my_economics %>% 
  ggplot(aes(date, psavert)) + 
  geom_line() + 
  scale_y_continuous(labels = percent, breaks = seq(0.015, 0.18, 0.025)) + 
  geom_point(data = my_economics %>% slice(which.max(psavert)), 
             aes(date, psavert), color = "red", size = 3) + 
  annotate("text", 
           label = "The highest saving rate was 17% in 2000.", 
           x = as.Date("1978-01-01"), y = 0.173, 
           size = 4, hjust = 0, vjust = 1, color = "grey40") + 
  annotate("curve", 
           curvature = 0,
           x = as.Date("1978-01-01"), 
           xend = as.Date("1976-01-01"),
           y = 0.17, 
           yend = 0.17,
           arrow = arrow(angle = 20, length = unit(.2, "cm")), size = .6) 

# Một cách khác có hiệu ứng tương tự nhưng sẽ rất hữu ích: 

p <- my_economics %>% 
  ggplot() + 
  geom_line(aes(date, psavert)) + 
  scale_y_continuous(labels = percent, breaks = seq(0.015, 0.18, 0.025)) + 
  geom_point(data = my_economics %>% slice(which.max(psavert)), 
             aes(date, psavert), color = "red", size = 3) + 
  annotate("text", 
           label = "The highest saving rate was 17% in 2000.", 
           x = as.Date("1978-01-01"), y = 0.173, 
           size = 4, hjust = 0, vjust = 1, color = "grey40") + 
  annotate("curve", 
           curvature = 0,
           x = as.Date("1978-01-01"), 
           xend = as.Date("1976-01-01"),
           y = 0.17, 
           yend = 0.17,
           arrow = arrow(angle = 20, length = unit(.2, "cm")), size = .6) 

p

# Thất nghiệp của Hoa Kì: 
p1 <- economics %>% ggplot() + 
  geom_line(aes(date, unemploy)) 

p1

# Bộ dữ liệu  nhiệm kì tổng thống: 

data("presidential")
presidential %>% head()

 

# Tô màu theo từng khoảng cầm quyền của các đảng: 
p1 + geom_rect(aes(xmin = start, xmax = end, fill = party),
                   ymin = -Inf, ymax = Inf, alpha = 0.2,
                   data = presidential)

# Cải tiến: 

p1 + geom_rect(aes(xmin = start, xmax = end, fill = party),
               ymin = -Inf, ymax = Inf, alpha = 0.2,
               data = presidential %>% filter(start >= min(economics$date), 
                                              start <= max(economics$date)))


# Hoặc cách khác như sau: 
small_pre <-  presidential %>% filter(start >= min(economics$date), 
                                      start <= max(economics$date))


  
economics %>% 
  filter(date >= min(small_pre$start)) %>% 
  ggplot() + 
  geom_line(aes(date, unemploy)) + 
  geom_rect(aes(xmin = start, xmax = end, fill = party),
            ymin = -Inf, ymax = Inf, alpha = 0.2,
            data = small_pre) ->> p2


# Tô màu theo ý muốn: 
p2 + 
  scale_fill_manual(values = c("red", "blue"), name = "Party") + 
  scale_y_continuous(breaks = seq(2000, 16000, 2000)) + 
  theme(panel.grid.minor = element_blank()) + 
  theme(panel.grid.major.x = element_blank())


p2 + 
  scale_fill_manual(values = c("red", "blue"), name = "Party") + 
  theme(panel.grid.minor = element_blank()) + 
  theme(panel.grid.major.x = element_blank()) + 
  geom_vline(aes(xintercept = as.numeric(start)),
             data = small_pre, colour = "blue")


p2 + 
  scale_fill_manual(values = c("red", "blue"), name = "Party") + 
  theme(panel.grid.minor = element_blank()) + 
  theme(panel.grid.major.x = element_blank()) + 
  theme(legend.position = "top") + 
  geom_vline(aes(xintercept = as.numeric(start)),
             data = small_pre, colour = "gray40") + 
  geom_text(aes(x = start, y = 1000, label = name),
            data = small_pre, size = 3, 
            vjust = -1, hjust = 0, nudge_x = 50) + 
  labs(x = "Date", y = "Number of unemployed in thousands", 
       title = "Number of unemployed in the United States by Political Party from 1967 to 2015", 
       subtitle = "A person is defined as unemployed in the United States if they are jobless, but have looked for work\nin the last four weeks and are available for work. People who are neither employed nor defined as\nunemployed are not included in the labor force calcualation.", 
       caption = "Source: Bureau of Labor Statistics")
  
# Hoặc thiết kế theo một kiểu khác: 
economics %>% 
  ggplot(aes(date, uempmed / 100)) + 
  geom_line(color = "cyan") + 
  scale_y_continuous(labels = percent) + 
  geom_area(fill = "cyan", alpha = 0.1) + 
  labs(x = "Date", y = "Unemployment Rate", 
       title = "Official Unemployment Rate in the United States from 1967 to 2015", 
       subtitle = "The official unemployment rate is known as U3. It defines unemployed people as those who\nare willing and available to work, and who have actively sought work within the past four weeks.\nThose with temporary, part-time or full-time jobs are considered employed, as are those who\nperform at least 15 hours of unpaid family work.", 
       caption = "Source: Bureau of Labor Statistics")

# Sử dụng font chữ ưa thích: 

library(extrafont)
font_import() # CHỉ thực hiện một lần duy nhất. m
extrafont::loadfonts(device = "win")

economics %>% 
  ggplot(aes(date, uempmed / 100)) + 
  geom_line(color = "cyan") + 
  scale_y_continuous(labels = percent) + 
  geom_area(fill = "cyan", alpha = 0.1) + 
  labs(x = "Date", y = "Unemployment Rate", 
       title = "Official Unemployment Rate in the United States from 1967 to 2015", 
       subtitle = "The official unemployment rate is known as U3. It defines unemployed people as those who\nare willing and available to work, and who have actively sought work within the past four weeks.\nThose with temporary, part-time or full-time jobs are considered employed, as are those who\nperform at least 15 hours of unpaid family work.", 
       caption = "Source: Bureau of Labor Statistics") + 
  # Chọn màu và kiểu chữ cho title: 
  theme(text = element_text(family = "Georgia", color = "grey10", size = 15)) + 
  # Hiệu chỉnh kiểu chữ cho caption: 
  theme(plot.caption = element_text(face = "italic")) + 
  # Hiệu chỉnh cho subtitle: 
  theme(plot.subtitle = element_text(color = "gray40", size = 12)) + 
  # Hiệu chỉnh vị trí của chữ Date: 
  theme(axis.title.x = element_text(hjust = 0, face = "bold", size = 11, color = "grey20")) + 
  # Tương tự: 
  theme(axis.title.y = element_text(hjust = 1, face = "bold", size = 11, color = "grey20")) + 
  theme(panel.grid.minor = element_blank()) + 
  theme(panel.grid.major.x = element_blank())


economics %>% 
  ggplot(aes(date, uempmed / 100)) + 
  geom_line(color = "cyan") + 
  scale_y_continuous(labels = percent) + 
  geom_area(fill = "cyan", alpha = 0.1) + 
  labs(x = "Date", y = "Unemployment Rate", 
       title = "Official Unemployment Rate in the United States from 1967 to 2015", 
       subtitle = "The official unemployment rate is known as U3. It defines unemployed people as those who\nare willing and available to work, and who have actively sought work within the past four weeks.\nThose with temporary, part-time or full-time jobs are considered employed, as are those who\nperform at least 15 hours of unpaid family work.", 
       caption = "Source: Bureau of Labor Statistics") + 
  # Chọn màu và kiểu chữ cho title: 
  theme(text = element_text(family = "Georgia", color = "grey10", size = 15)) + 
  # Hiệu chỉnh kiểu chữ cho caption: 
  theme(plot.caption = element_text(face = "italic")) + 
  # Hiệu chỉnh cho subtitle: 
  theme(plot.subtitle = element_text(color = "gray40", size = 12)) + 
  # Hiệu chỉnh vị trí của chữ Date: 
  theme(axis.title.x = element_text(hjust = 0, face = "bold", size = 11, color = "grey20")) + 
  # Tương tự: 
  theme(axis.title.y = element_text(hjust = 1, face = "bold", size = 11, color = "grey20")) + 
  theme(panel.grid.major = element_line(color = "#4d5566")) + 
  theme(panel.grid.minor.y = element_blank()) + 
  theme(panel.grid.minor.x = element_blank()) + 
  theme(panel.background = element_rect(fill = "#444B5A")) 

# Tạo thành một hàm riêng: 

my_theme <- function(...) {
  theme(text = element_text(family = "Georgia", color = "grey10", size = 15)) + 
    theme(plot.caption = element_text(face = "italic")) + 
    theme(plot.subtitle = element_text(color = "gray40", size = 12)) + 
    theme(axis.title.x = element_text(hjust = 0, face = "bold", size = 11, color = "grey20")) + 
    theme(axis.title.y = element_text(hjust = 1, face = "bold", size = 11, color = "grey20")) + 
    theme(panel.grid.major = element_line(color = "#4d5566")) + 
    theme(panel.grid.minor.y = element_blank()) + 
    theme(panel.grid.minor.x = element_blank()) + 
    theme(panel.background = element_rect(fill = "#444B5A")) 
  
}

# ĐIều đó có nghĩa là:   
  

economics %>% 
  ggplot(aes(date, uempmed / 100)) + 
  geom_line(color = "cyan") + 
  scale_y_continuous(labels = percent) + 
  geom_area(fill = "cyan", alpha = 0.1) + 
  labs(x = "Date", y = "Unemployment Rate", 
       title = "Official Unemployment Rate in the United States from 1967 to 2015", 
       subtitle = "The official unemployment rate is known as U3. It defines unemployed people as those who\nare willing and available to work, and who have actively sought work within the past four weeks.\nThose with temporary, part-time or full-time jobs are considered employed, as are those who\nperform at least 15 hours of unpaid family work.", 
       caption = "Source: Bureau of Labor Statistics") + 
  my_theme()

# Thể hiện hai time series: 

economics %>%
  select(date, psavert, uempmed) %>% 
  gather(a, b, -date)

economics %>%
  select(date, psavert, uempmed) %>% 
  gather(a, b, -date) %>% 
  ggplot(aes(x = date, y = b)) +
  geom_line(aes(color = a), size = 1) +
  scale_color_manual(values = c("#00AFBB", "#E7B800"))

economics %>%
  select(date, psavert, uempmed) %>% 
  gather(a, b, -date) %>% 
  ggplot(aes(x = date, y = b)) +
  geom_line(aes(color = a), size = 1) +
  scale_color_manual(values = c("#00AFBB", "#E7B800"))

economics %>%
  select(date, psavert, uempmed) %>% 
  gather(a, b, -date) %>% 
  ggplot(aes(x = date, y = b)) +
  geom_line(aes(color = a), size = 1) + 
  facet_wrap(~ a) + 
  my_theme()


economics %>%
  select(date, psavert, uempmed) %>% 
  gather(a, b, -date) %>% 
  ggplot(aes(x = date, y = b)) +
  geom_line(aes(color = a), size = 1) + 
  facet_wrap(~ a, scales = "free") + 
  my_theme()

#---------------------------------
#         Area Graph
#---------------------------------


economics %>%
  select(date, psavert, uempmed) %>% 
  gather(a, b, -date) %>% 
  ggplot(aes(date, y = b)) +
  geom_area(aes(color = a, fill = a), alpha = 0.5)


economics %>%
  select(date, psavert, uempmed) %>% 
  gather(a, b, -date) %>% 
  ggplot(aes(date, y = b)) +
  geom_area(aes(color = a, fill = a), alpha = 0.5, position = position_dodge(0.8))
  
economics %>%
  select(date, psavert, uempmed) %>% 
  gather(a, b, -date) %>% 
  ggplot(aes(date, y = b)) +
  geom_area(aes(color = a, fill = a), alpha = 0.5, position = position_dodge(0.8)) +
  scale_color_manual(values = c("#00AFBB", "#E7B800")) +
  scale_fill_manual(values = c("#00AFBB", "#E7B800"))


economics %>%
  select(date, psavert, uempmed) %>% 
  gather(a, b, -date) %>% 
  ggplot(aes(date, b)) + 
  geom_line(color = "cyan") + 
  geom_area(fill = "cyan", alpha = 0.1) + 
  facet_wrap(~ a, scales = "free") + 
  my_theme()


# Cải tiến cho đẹp hơn: 

economics %>%
  select(date, psavert, uempmed) %>% 
  gather(a, b, -date) %>% 
  mutate(a = case_when(a == "psavert" ~ "Saving", a != "psavert" ~ "Unemployment")) %>% 
  mutate(b = b / 100) %>% 
  ggplot(aes(date, y = b)) +
  geom_area(aes(color = a, fill = a), alpha = 0.5, position = position_dodge(0.8)) +
  scale_color_manual(name = "", values = c("#00AFBB", "#E7B800")) +
  scale_fill_manual(name = "", values = c("#00AFBB", "#E7B800")) + 
  scale_y_continuous(labels = percent) + 
  theme(panel.grid.minor.y = element_blank()) + 
  theme(panel.grid.minor.x = element_blank()) + 
  theme(legend.position = "top") + 
  theme(text = element_text(family = "Garamond", color = "grey10", size = 13, face = "bold")) + 
  labs(x = NULL, y = NULL, 
       title = "Official Unemployment and Saving Rate in the United States from 1967 to 2015",
       caption = "Source: Bureau of Labor Statistics")


# Case Study (tham khảo http://datatopics.worldbank.org/sdgatlas/SDG-13-climate-action.html): 

data <- WDI(country = "VN", 
            indicator = c("NE.IMP.GNFS.CD", "TM.VAL.MRCH.CD.WT", "TM.VAL.MRCH.WL.CD"), 
            start = 2005, end = 2015)


# Đổi tên: và chuyển về long form: 

data %<>% rename(IMP = NE.IMP.GNFS.CD, 
                 MRW = TM.VAL.MRCH.CD.WT, 
                 MAC = TM.VAL.MRCH.WL.CD)


#-----------------------------------------------------------------------------
#        Sử dụng font
#  1. Trước hết download một font yêu thích, ví dụ, từ
#     http://www.megafonts.net/view/officinasanitc-extrabold_96211.
#  2. Cài đặt font mới này (đuôi là ttf)
#  3. Di chuyển file vừa cài đặt đó vào: 
#     (1) thư mục font của Win, (2) thư mục hiện thời mà R đang hoạt động.
#-----------------------------------------------------------------------------

setwd("C:/Users/HP/Documents")
font_import(pattern = "OfficinaSansITCMedium.ttf", prompt = FALSE)

# Vẽ: 

data %>%  
  select(year, IMP, MRW, MAC) %>% 
  gather(Item, b, -year) %>% 
  mutate(b = round(b / 1000000000)) %>% 
  ggplot(aes(x = year, y = b, fill = Item)) + 
  geom_area(stat = "identity", show.legend = FALSE) + 
  labs(x = NULL, 
       y = NULL, 
       title = "Imports of Goods and Services in Vietnam: 2005 - 2015", 
       subtitle = "Note: Unit in billion dollar and numbers adjusted for inflation rate and price of 2015", 
       caption = "Data Source: The World Bank") + 
  scale_x_continuous(breaks = seq(2005, 2015, 1), 
                     limits = c(2005, 2015.1), 
                     expand = c(0.01, 0)) + 
  scale_fill_manual(values = c("#9C9C9C", "#FF82AB", "#FF1493")) + 
  theme(plot.background = element_rect(fill = "white")) + 
  theme(panel.grid.minor = element_blank()) + 
  theme(panel.grid.major.x = element_blank()) + 
  theme(panel.grid.major.y = element_line(size = 0.8)) + 
  theme(axis.ticks = element_blank()) + 
  theme(panel.border = element_blank()) + 
  theme(text = element_text(family = "OfficinaSansITC", size = 18, color = "black")) + 
  theme(plot.subtitle = element_text(color = "gray40", size = 15)) + 
  # Dịch vị trí của caption sang phía trái: 
  theme(plot.caption = element_text(color = "gray40", size = 13, family = "OfficinaSansITC", hjust = -0.02)) + 
  theme(legend.position = "top") + 
  # Kích cỡ + màu sắc hiển thị trên trục X và Y: 
  theme(axis.text.x = element_text(size = 13, color = "gray40")) + 
  theme(axis.text.y = element_text(size = 13, color = "gray40")) +  
  annotate("text", x = 2011, y = 80, label = "Industrial machines\nfrom US and EU", 
           hjust = 0, vjust = 1, color = "white", 
           size = 5, family = "OfficinaSansITC") + 
  annotate("text", x = 2011, y = 190, label = "Raw industrial materials\n from China", 
           hjust = 0, vjust = 1, color = "white", 
           size = 5, family = "OfficinaSansITC") + 
  annotate("text", x = 2009.2, y = 370, label = "Consumer goods from\nall countries", 
           hjust = 0, vjust = 1, color = "gray30", 
           size = 5, family = "OfficinaSansITC")  
  


  

