

#-------------------------------------
#   Scatter Plot + Regression Line
#-------------------------------------

# Load dữ liệu: 
library(gapminder)
data("gapminder")

# Tìm hiểu về ý nghĩa của các biến số của bộ dữ liệu: 
?gapminder

# Scatter Plot cơ bản: 

gapminder %>% 
  ggplot(aes(gdpPercap, lifeExp)) + 
  geom_point() 

# Làm mờ các điểm + tô màu theo ý muốn: 
library(tidyverse)

gapminder %>% 
  ggplot(aes(gdpPercap, lifeExp)) + 
  geom_point(alpha = 0.2, color = "purple") 

# Nhấn mạnh đến một số Outliers" 

gapminder %>% 
  ggplot(aes(gdpPercap, lifeExp)) + 
  geom_point(alpha = 0.2, color = "purple") + 
  geom_point(data = gapminder %>% filter(gdpPercap > 60000), 
             aes(gdpPercap, lifeExp), color = "red", size = 2) ->> p1

p1

# Có thể đổi theme nếu muốn: 
p1 + theme_linedraw()
p1 + theme_light()

# Hoặc cố định theme: 
theme_set(theme_minimal())
p1

# Hiệu chỉnh trục X: 
p2 <- p1 + 
  scale_x_log10()

# Hiệu chỉnh tên trục + tiêu đề: 

gapminder %>% 
  ggplot(aes(gdpPercap, lifeExp)) + 
  geom_point(alpha = 0.2, color = "purple") + 
  scale_x_log10() + 
  scale_y_continuous(breaks = seq(20, 90, 10)) + 
  labs(x = "GDP per Capita", 
       y = "Life Expectancy", 
       title = "The Relationship Between GDP and Life Expectancy", 
       subtitle = "Note: Life Expectancy at birth, in years", 
       caption = "Data Source: http://www.gapminder.org/data/")

# Hiệu chỉnh tiếp cho trục x: 
library(scales)

gapminder %>% 
  ggplot(aes(gdpPercap, lifeExp)) + 
  geom_point(alpha = 0.2, color = "purple") + 
  scale_x_log10(labels = scales::dollar) + 
  scale_y_continuous(breaks = seq(20, 90, 10)) + 
  labs(x = "GDP per Capita", 
       y = "Life Expectancy", 
       title = "The Relationship Between GDP and Life Expectancy", 
       subtitle = "Note: Life Expectancy at birth, in years", 
       caption = "Data Source: http://www.gapminder.org/data/") ->> g3 

g3

# Thêm đường hồi quy: 
g3 + geom_smooth(method = "lm")
g3 + geom_smooth(method = "lm", color = "blue", fill = "blue", alpha = 0.1)
g3 + geom_smooth(method = "lm", color = "orange", se = FALSE)


# Nhấn mạnh đến Việt Nam năm 2007 chẳng hạn: 

g3 + 
  geom_smooth(method = "lm", color = "orange", se = FALSE) + 
  geom_point(data = gapminder %>% filter(year == 2007 & country == "Vietnam"), 
             aes(gdpPercap, lifeExp), color = "red", size = 3) + 
  geom_text(data = gapminder %>% filter(year == 2007 & country == "Vietnam"), 
            aes(label = country))

# Hiệu chỉnh thêm nữa: 
library(ggrepel)

g3 + 
  geom_smooth(method = "lm", color = "orange", se = FALSE) + 
  geom_point(data = gapminder %>% filter(year == 2007 & country == "Vietnam"), 
             aes(gdpPercap, lifeExp), color = "red", size = 3) + 
  geom_text_repel(data = gapminder %>% filter(year == 2007 & country == "Vietnam"), 
            aes(label = country), force = 19)

# Câu hỏi mở rộng: Hiển thị thêm Ấn Độ, Trung Quốc, Thái Lan và Malaysia trong
# năm 2007 nhằm so sánh các quốc gia này với Việt Nam về tuổi thọ binh quân bằng
# cách nào? 


# Biểu đồ phân tán với màu sắc riêng biệt ứng với từng châu lục: 

gapminder %>% 
  ggplot(aes(gdpPercap, lifeExp, color = continent)) + 
  geom_point(alpha = 0.4)

# Hoặc bỏ hiển thị legend: 
gapminder %>% 
  ggplot(aes(gdpPercap, lifeExp, color = continent)) + 
  geom_point(alpha = 0.4, show.legend = FALSE)

# Hoặc điều chỉnh vị trí của legend (kiểu 1): 
gapminder %>% 
  ggplot(aes(gdpPercap, lifeExp, color = continent)) + 
  geom_point(alpha = 0.4) + 
  theme(legend.position = c(0.9, 0.3))

# Điều chỉnh vị trí legend (kiểu 2): 
gapminder %>% 
  ggplot(aes(gdpPercap, lifeExp, color = continent)) + 
  geom_point(alpha = 0.4) + 
  theme(legend.position = "top")
  
# Biểu diễn kiểu khác: 

gapminder %>% 
  ggplot(aes(gdpPercap, lifeExp, color = continent)) + 
  geom_point(alpha = 0.4) + 
  scale_x_log10(labels = scales::dollar) + 
  labs(x = "GDP per Capita", 
       y = "Life Expectancy", 
       title = "The Relationship Between GDP and Life Expectancy", 
       subtitle = "Note: Life Expectancy at birth, in years", 
       caption = "Data Source: http://www.gapminder.org/data/")

gapminder %>% 
  rename(Continent = continent) %>% 
  ggplot(aes(gdpPercap, lifeExp, color = Continent)) + 
  geom_point(alpha = 0.4) + 
  scale_x_log10(labels = scales::dollar) + 
  labs(x = "GDP per Capita", 
       y = "Life Expectancy", 
       title = "The Relationship Between GDP and Life Expectancy", 
       subtitle = "Note: Life Expectancy at birth, in years", 
       caption = "Data Source: http://www.gapminder.org/data/")


# Thay đổi vị trí của legend (kiểu 1): 

gapminder %>% 
  rename(Continent = continent) %>% 
  ggplot(aes(gdpPercap, lifeExp, color = Continent)) + 
  geom_point(alpha = 0.4) + 
  scale_x_log10(labels = scales::dollar) + 
  labs(x = "GDP per Capita", 
       y = "Life Expectancy", 
       title = "The Relationship Between GDP and Life Expectancy", 
       subtitle = "Note: Life Expectancy at birth, in years", 
       caption = "Data Source: http://www.gapminder.org/data/") + 
  theme(legend.position = "top")

# Thay đổi vị trí củalLegend (kiểu 2): 

gapminder %>% 
  rename(Continent = continent) %>% 
  ggplot(aes(gdpPercap, lifeExp, color = Continent)) + 
  geom_point(alpha = 0.4) + 
  scale_x_log10(labels = scales::dollar) + 
  labs(x = "GDP per Capita", 
       y = "Life Expectancy", 
       title = "The Relationship Between GDP and Life Expectancy", 
       subtitle = "Note: Life Expectancy at birth, in years", 
       caption = "Data Source: http://www.gapminder.org/data/") + 
  theme(legend.position = c(0.85, 0.25))


# Scatter Plot trên 5 panel riêng biệt: 
gapminder %>% 
  rename(Continent = continent) %>% 
  ggplot(aes(gdpPercap, lifeExp, color = Continent)) + 
  geom_point(alpha = 0.4, show.legend = FALSE) + 
  scale_x_log10(labels = scales::dollar) + 
  labs(x = "GDP per Capita", 
       y = "Life Expectancy", 
       title = "The Relationship Between GDP and Life Expectancy", 
       subtitle = "Note: Life Expectancy at birth, in years", 
       caption = "Data Source: http://www.gapminder.org/data/") + 
  facet_wrap(~ Continent)

  
gapminder %>% 
  rename(Continent = continent) %>% 
  ggplot(aes(gdpPercap, lifeExp, color = Continent)) + 
  geom_point(alpha = 0.4, show.legend = FALSE) + 
  scale_x_log10(labels = scales::dollar) + 
  labs(x = "GDP per Capita", 
       y = "Life Expectancy", 
       title = "The Relationship Between GDP and Life Expectancy", 
       subtitle = "Note: Life Expectancy at birth, in years", 
       caption = "Data Source: http://www.gapminder.org/data/") + 
  facet_wrap(~ Continent, scales = "free") 


gapminder %>% 
  rename(Continent = continent) %>% 
  ggplot(aes(gdpPercap, lifeExp, color = Continent)) + 
  geom_point(alpha = 0.4, show.legend = FALSE) + 
  scale_x_log10(labels = scales::dollar) + 
  labs(x = "GDP per Capita", 
       y = "Life Expectancy", 
       title = "The Relationship Between GDP and Life Expectancy", 
       subtitle = "Note: Life Expectancy at birth, in years", 
       caption = "Data Source: http://www.gapminder.org/data/") + 
  facet_wrap(~ Continent, nrow = 1, ncol = 5)


gapminder %>% 
  rename(Continent = continent) %>% 
  ggplot(aes(gdpPercap, lifeExp, color = Continent)) + 
  geom_point(alpha = 0.3, show.legend = FALSE) + 
  geom_smooth(method = "lm", color = "orange", fill = "orange", alpha = 0.2) + 
  scale_x_log10(labels = scales::dollar) + 
  labs(x = "GDP per Capita", 
       y = "Life Expectancy", 
       title = "The Relationship Between GDP and Life Expectancy", 
       subtitle = "Note: Life Expectancy at birth, in years", 
       caption = "Data Source: http://www.gapminder.org/data/") + 
  facet_wrap(~ Continent, scales = "free") 


gapminder %>% 
  rename(Continent = continent) %>% 
  ggplot(aes(gdpPercap, lifeExp, color = Continent)) + 
  geom_point(alpha = 0.3, show.legend = FALSE) + 
  geom_smooth(method = "lm", color = "orange", fill = "orange", alpha = 0.2) + 
  scale_x_log10(labels = scales::dollar) + 
  labs(x = "GDP per Capita", 
       y = "Life Expectancy", 
       title = "The Relationship Between GDP and Life Expectancy", 
       subtitle = "Note: Life Expectancy at birth, in years", 
       caption = "Data Source: http://www.gapminder.org/data/") + 
  facet_wrap(~ Continent)

# Tô màu theo ý muốn: 

gapminder %>% 
  rename(Continent = continent) %>% 
  ggplot(aes(gdpPercap, lifeExp, color = Continent)) + 
  geom_point(alpha = 0.3, show.legend = FALSE) + 
  geom_smooth(method = "lm", color = "green", fill = "green", alpha = 0.2) + 
  scale_x_log10(labels = scales::dollar) + 
  labs(x = "GDP per Capita", 
       y = "Life Expectancy", 
       title = "The Relationship Between GDP and Life Expectancy", 
       subtitle = "Note: Life Expectancy at birth, in years", 
       caption = "Data Source: http://www.gapminder.org/data/") + 
  facet_wrap(~ Continent) + 
  scale_color_manual(values = c("red", "blue", "purple", "#E69F00", "black"))


gapminder %>% 
  rename(Continent = continent) %>% 
  ggplot(aes(gdpPercap, lifeExp, color = Continent)) + 
  geom_point(alpha = 0.3, show.legend = FALSE) + 
  geom_smooth(method = "lm") + 
  scale_x_log10(labels = scales::dollar) + 
  labs(x = "GDP per Capita", 
       y = "Life Expectancy", 
       title = "The Relationship Between GDP and Life Expectancy", 
       subtitle = "Note: Life Expectancy at birth, in years", 
       caption = "Data Source: http://www.gapminder.org/data/") + 
  theme(legend.position = c(0.85, 0.25))


# Đường hồi quy cho Americas nhưng vẫn để  các thằng kia (không phải Americas) làm nền: 
gapminder %>% 
  ggplot(aes(gdpPercap, lifeExp)) + 
  scale_x_log10(labels = scales::dollar) + 
  geom_point(data = gapminder %>% filter(continent != "Americas"), alpha =  0.1) + 
  geom_point(data = gapminder %>% filter(continent == "Americas"), color = "purple") + 
  geom_smooth(data = gapminder %>% filter(continent == "Americas"), 
              method = "lm", color = "orange", fill = "orange", alpha = 0.1) + 
  labs(x = "GDP per Capita", 
       y = "Life Expectancy", 
       title = "The Relationship Between GDP and Life Expectancy", 
       subtitle = "Note: Life Expectancy at birth, in years", 
       caption = "Data Source: http://www.gapminder.org/data/") 

#-------------------------------------------------------------------------------
#  Histogram / Density Plot
#  Tham khảo: http://www.pewsocialtrends.org/2016/05/11/americas-shrinking-middle-class-a-close-look-at-changes-within-metropolitan-areas/
#-------------------------------------------------------------------------------

# Đọc dữ liệu: 
library(readxl)
income <- read_excel("D:/GSO_R_Course/data_for_visualization/america_metro.xlsx", 
                     sheet = 3, skip = 7)

# Bỏ đi cột và dòng không cần thiết: 
income <- income %>% 
  select(-X__2) %>% 
  slice(-1)

# Đổi lại tên cho cột biến: 

names(income) <- c("Metro", "All_99", "Lower_99", "Middle_99", "Upper_99",
                   "All_14", "Lower_14", "Middle_14", "Upper_14")

# Xem qua dữ liệu: 
sapply(income, class)

# Ba kiểu Histogram: 
p1 <- income %>% 
  ggplot(aes(x = All_14)) +
  geom_histogram(binwidth = 1000) +
  labs(title = "Bin Width = 1000")

p2 <- income %>% 
  ggplot(aes(x = All_14)) +
  geom_histogram(binwidth = 5000, color = "red", fill = "blue", alpha = 0.3) +
  labs(title = "Bin Width = 5000")

p3 <- income %>% 
  ggplot(aes(x = All_14)) +
  geom_histogram(binwidth = 10000, color = "grey30", fill = "white") +
  labs(title = "Bin Width = 10000")

library(gridExtra)
grid.arrange(p1, p2, p3, nrow = 1, ncol = 3)

# Density Plot: 

income %>% 
  ggplot(aes(x = All_14)) +
  geom_density()

income %>% 
  ggplot(aes(x = All_14)) +
  geom_density(color = "grey40", fill = "grey80", size = 1.2)

# Biểu diễn đồng thời cả Histogram và Density: 

income %>% 
  ggplot(aes(x = All_14)) + 
  geom_density(color = "red", fill = "red", alpha = 0.2) + 
  geom_histogram(aes(y = ..density..), binwidth = 2000, 
                 fill = "blue", color = "blue", alpha = 0.2)

# So sánh các nhóm. Trước hết chuyển dữ liệu về long form: 
compare <- income %>%
  select(Metro, All_99, All_14) %>%
  gather(Year, Income, -Metro)

# Tham khảo thêm cách chọn màu ở http://colorbrewer2.org/: 
compare %>% 
  ggplot(aes(x = Income, fill = Year)) +
  geom_density(alpha = 0.4) + 
  scale_fill_manual(values = c("#1b9e77", "#7570b3"))

compare %>% 
  ggplot(aes(x = Income)) +
  geom_histogram(binwidth = 2000, color = "grey30", fill = "white") +
  facet_grid(Year ~ .)


class_comparison <- income %>%
  select(Metro, Lower_99:Upper_99, Lower_14:Upper_14) %>%
  gather(Class, Income, -Metro) %>%
  separate(Class, into = c("Class", "Year")) %>%
  mutate(Year = case_when(Year == "99" ~ "1999", 
                          Year != "99" ~ "2014"))


class_comparison %>% 
  ggplot(aes(x = Income, fill = Class, color = Class)) +
  geom_histogram(alpha = 0.2) +
  facet_wrap(~ Class, scales = "free_x")


class_comparison %>% 
  ggplot(aes(x = Income, fill = Class, color = Class)) +
  geom_histogram(alpha = 0.2) +
  facet_wrap(Year ~ Class, scales = "free_x")


class_comparison %>% 
  ggplot(aes(x = Income, fill = Class, color = Class)) +
  geom_histogram(alpha = 0.2) +
  facet_wrap(Year ~ Class, scales = "free")


class_comparison %>% 
  ggplot(aes(x = Income, fill = Year, color = Year)) +
  geom_histogram(alpha = 0.2) +
  facet_wrap(Year ~ Class, scales = "free")

# Thu nhập trung bình của từng nhóm theo năm: 
class_mean <- class_comparison %>%
  group_by(Class, Year) %>%
  summarise(Mean = mean(Income)) %>% 
  ungroup()

class_comparison %>% 
  ggplot(aes(x = Income, fill = Class, color = Class)) +
  geom_histogram(alpha = 0.2, show.legend = FALSE) +
  facet_wrap(Year ~ Class, scales = "free") + 
  geom_vline(data = class_mean, aes(xintercept = Mean), linetype = "dashed") + 
  ylab("Frequency") +
  xlab("Median Household Income (thousands)") +
  labs(title = "Median Household Income by Income Tier Across U.S. Metropolitan Areas",
       subtitle = "Average median income across 229 metros decreased from $67,863 in 1999 to $62,662 in 2014, representing an 8% loss in \nincome. The lower income class experienced the largest impact with a 11% decrease while the middle and upper class median \nhousehold income decreased by 6% and 8% respectively.",
       caption = "Source: Pew Research Center analysis of the \n2000 decennial census and 2014 American \nCommunity Survey (IPUMS)")


# Cải tiến: 

p <- class_comparison %>% 
  ggplot(aes(x = Income / 1000, fill = Class, color = Class)) +
  geom_histogram(alpha = 0.2, show.legend = FALSE) +
  facet_wrap(Year ~ Class, scales = "free") + 
  geom_vline(data = class_mean, aes(xintercept = Mean / 1000), linetype = "dashed") + 
  scale_x_continuous(labels = scales::dollar) +
  scale_y_continuous(limits = c(0, 58), expand = c(0, 0)) + 
  labs(x = "Median Household Income (thousands)", y = "Frequency", 
       title = "Median Household Income by Income Tier Across U.S. Metropolitan Areas", 
       subtitle = "Average median income across 229 metros decreased from $67,863 in 1999 to $62,662 in 2014, representing an 8% loss in \nincome. The lower income class experienced the largest impact with a 11% decrease while the middle and upper class median \nhousehold income decreased by 6% and 8% respectively.",
       caption = "Source: Pew Research Center analysis of the \n2000 decennial census and 2014 American \nCommunity Survey (IPUMS)")


p

# Cải tiến hơn nữa: 

class_mean <- class_mean %>%
  mutate(Label = paste0("$", prettyNum(round(Mean, 0), big.mark = ",")))

p + 
  geom_text(data = class_mean, 
            aes(x = Mean / 1000, y = 52, id = Class, label = Label),
            size = 3, hjust = -.1, show.legend = FALSE, color = "black")

#------------------------
#      Box Plot
#------------------------

# Vẽ đơn giản: 
iris %>% 
  ggplot(aes(Species, Sepal.Length)) + 
  geom_boxplot()

# Tô màu đỏ, chẳng hạn, nếu  muốn  nhấn mạnh đến outliers: 
iris %>% 
  ggplot(aes(Species, Sepal.Length)) + 
  geom_boxplot(outlier.color = "red")

# Hiển thị thêm mean nếu muốn: 
iris %>% 
  ggplot(aes(Species, Sepal.Length)) + 
  geom_boxplot(outlier.color = "red") + 
  stat_summary(fun.y = mean, colour = "blue", geom = "point")

# Boxplot của cả 4 biến số cho 3 loài hoa diên vĩ + đầy đủ chỉ dẫn: 
iris %>% 
  gather(Variable, Value, -Species) %>% 
  ggplot(aes(Species, Value, fill = Species, color = Species)) + 
  geom_boxplot(show.legend = FALSE, alpha = 0.4) + 
  facet_wrap(~ Variable, scales = "free") + 
  labs(x = NULL, 
       y = NULL, 
       title = "An Example of Boxplot", 
       caption = "Data Source: Iris data set by R. Fisher (1936)")

# Hoặc một kiểu khác:  
iris %>% 
  gather(Variable, Value, -Species) %>% 
  ggplot(aes(Species, Value, fill = Species, color = Species)) + 
  geom_boxplot(show.legend = FALSE, alpha = 0.4) + 
  facet_wrap(~ Variable, scales = "free") + 
  theme_bw() + 
  labs(x = NULL, 
       y = NULL, 
       title = "An Example of Boxplot", 
       caption = "Data Source: Iris data set by R. Fisher (1936)")

# So sánh với Histogram và Density: 
iris %>% 
  gather(Variable, Value, -Species) %>% 
  ggplot(aes(Value, fill = Species, color = Species)) + 
  geom_histogram(alpha = 0.3) + 
  facet_wrap(~ Variable) + 
  theme_bw() + 
  theme(legend.position = "top") + 
  labs(x = NULL, 
       y = NULL, 
       title = "An Example of Histogram", 
       caption = "Data Source: Iris data set by R. Fisher (1936)")

iris %>% 
  gather(Variable, Value, -Species) %>% 
  ggplot(aes(Value, fill = Species, color = Species)) + 
  geom_density(alpha = 0.3, show.legend = FALSE) + 
  facet_wrap(~ Variable, scales = "free") + 
  theme_bw() + 
  labs(x = NULL, 
       y = NULL, 
       title = "An Example of Density Plot", 
       caption = "Data Source: Iris data set by R. Fisher (1936)")


iris %>% 
  gather(Variable, Value, -Species) %>% 
  ggplot(aes(Value, fill = Species, color = Species)) + 
  geom_density(alpha = 0.3, show.legend = FALSE) + 
  facet_wrap(~ Variable) + 
  theme_bw() + 
  labs(x = NULL, 
       y = NULL, 
       title = "An Example of Density Plot", 
       caption = "Data Source: Iris data set by R. Fisher (1936)")




 