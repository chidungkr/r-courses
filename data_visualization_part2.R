

#---------------------------
#        Bar Graph
#---------------------------
                
library(tidyverse)       
library(magrittr)

flights <- read_csv("D:/GSO_R_Course/data_for_visualization/flights_revised.csv")

# Số chuyến bay của các hãng: 
firm_number_flights <- flights %>% 
  group_by(carrier) %>% 
  count()

firm_number_flights <- flights %>% 
  group_by(carrier) %>% 
  count()


# Đặt một theme cố định: 
theme_set(theme_minimal())

# Vẽ đơn giản: 
firm_number_flights %>% 
  ggplot(aes(carrier, n)) + 
  geom_col()

# Hoặc sử dụng geom_bar(): 
firm_number_flights %>% 
  ggplot(aes(carrier, n)) + 
  geom_bar(stat = "identity")

# sắp xếp lại (cách 1): 

firm_number_flights %>% 
  ggplot(aes(reorder(carrier, n), n)) + 
  geom_col()


# Cách 2: 
firm_number_flights_ordered <- firm_number_flights %>% 
  ungroup() %>% 
  arrange(n) %>% 
  mutate(carrier = factor(carrier, levels = carrier))


firm_number_flights_ordered %>% 
  ggplot(aes(carrier, n)) + 
  geom_col()

# Hoặc một kiểu khác: 
firm_number_flights %>% 
  ggplot(aes(reorder(carrier, -n), n)) + 
  geom_col()

# Sử dụng chuỗi toán tử pipe: 

flights %>% 
  group_by(carrier) %>% 
  count() %>% 
  ggplot(aes(reorder(carrier, -n), n)) + 
  geom_col()

# 7 hãng hàng không lớn nhất: 
flights %>% 
  filter(carrier %in% c("UA", "B6", "EV", "DL", "AA", "MQ", "US")) %>% 
  group_by(carrier) %>% 
  count() %>% 
  ggplot(aes(reorder(carrier, -n), n)) + 
  geom_col()
  
# Hiển thị các con số: 

flights %>% 
  filter(carrier %in% c("UA", "B6", "EV", "DL", "AA", "MQ", "US")) %>% 
  group_by(carrier) %>% 
  count() %>% 
  ggplot(aes(reorder(carrier, -n), n)) + 
  geom_col(width = 0.7) + 
  geom_text(aes(label = n), vjust = 1.4, color = "white", size = 4) + 
  labs(x = NULL, y = "Number of Flights", 
       title = "Number of Flights by some U.S Largest Airlines in 2013", 
       caption = "Data Source: https://www.faa.gov/") ->> p1

p1

# Xoay nghiêng: 
p1 + coord_flip()

# Hiệu chỉnh: 

flights %>% 
  filter(carrier %in% c("UA", "B6", "EV", "DL", "AA", "MQ", "US")) %>% 
  group_by(carrier) %>% 
  count() %>% 
  ggplot(aes(reorder(carrier, n), n)) + 
  geom_col(width = 0.7) + 
  geom_text(aes(label = n), hjust = 1.2, color = "white", size = 5) + 
  labs(x = NULL, y = "Number of Flights", 
       title = "Number of Flights by some U.S Largest Airlines in 2013", 
       caption = "Data Source: https://www.faa.gov/") + 
  coord_flip() + 
  scale_y_continuous(breaks = seq(0, 60000, 10000))

# Hoặc kiểu khác: 

flights %>% 
  filter(carrier %in% c("UA", "B6", "EV", "DL", "AA", "MQ", "US")) %>% 
  group_by(carrier) %>% 
  count() %>% 
  ggplot(aes(reorder(carrier, n), n, fill = carrier)) + 
  geom_col(width = 0.7, show.legend = FALSE) + 
  geom_text(aes(label = n), hjust = 1.2, color = "white", size = 5) + 
  labs(x = NULL, y = "Number of Flights", 
       title = "Number of Flights by some U.S Largest Airlines in 2013", 
       caption = "Data Source: https://www.faa.gov/") + 
  coord_flip() + 
  scale_y_continuous(breaks = seq(0, 60000, 10000)) ->> p2

p2

# Sử dụng theme của một số tạp chí nổi tiếng: 
library(ggthemes)

p2 + 
  theme_economist(horizontal = FALSE)

p2 + 
  theme_economist(horizontal = FALSE) + 
  scale_fill_economist()

p2 + 
  theme_fivethirtyeight(base_size = 10)


# Làm nổi bật một hãng được chọn là AA: 

flights %>% 
  filter(carrier %in% c("UA", "B6", "EV", "DL", "AA", "MQ", "US")) %>% 
  group_by(carrier) %>% 
  count() %>% 
  ungroup() %>% 
  arrange(n) %>% 
  mutate(carrier = factor(carrier, levels = carrier)) ->> df1


df1 %>% 
  ggplot() + 
  geom_bar(aes(carrier, n), fill = "#377eb8", stat = "identity", width = 0.7) + 
  geom_bar(data = df1 %>% filter(carrier == "AA"), aes(carrier, n), 
           fill = "#e41a1c", stat = "identity", width = 0.7) + 
  geom_text(data = df1 %>% filter(carrier == "AA"), aes(carrier, n, label = n), 
            hjust = 1.2, color = "white", size = 5) + 
  coord_flip() + 
  labs(x = NULL, y = "Number of Flights", 
       title = "Number of Flights by some U.S Largest Airlines in 2013", 
       caption = "Data Source: https://www.faa.gov/") + 
  coord_flip() + 
  scale_y_continuous(breaks = seq(0, 60000, 10000))


  
flights %>% 
  filter(carrier %in% c("UA", "B6", "EV", "DL", "AA", "MQ", "US")) %>% 
  group_by(carrier, origin) %>% 
  count() %>% 
  ungroup() ->> df2

df2 %>% 
  ggplot(aes(carrier, n, fill = origin)) + 
  geom_col()
  
# Vậy thì cải tiến như sau: 

df2 %>% 
  rename(Airport = origin) %>% 
  ggplot(aes(carrier, n, fill = Airport)) + 
  geom_col() 
  
df2 %>% 
  rename(Airport = origin) %>% 
  ggplot(aes(carrier, n, fill = Airport)) + 
  geom_col(position = "dodge")  

df2 %>% 
  rename(Airport = origin) %>% 
  ggplot(aes(carrier, n, fill = Airport)) + 
  geom_col(position = "fill") +
  coord_flip()


df2 %>% 
  mutate(Airport = case_when(origin != "JFK" ~ "Others", 
                             TRUE ~ origin)) %>% 
  ggplot(aes(carrier, n, fill = Airport)) + 
  geom_col(position = "fill") + 
  coord_flip()

# Chú ý rằng: 

df2 %>% spread(origin, n)

# Tạo ra các cột biến mới: 
df2 %>% 
  spread(origin, n) %>% 
  mutate(others = EWR + LGA, 
         total = others + JFK) %>% 
  mutate(jfk_per = JFK / total, 
         others_per = 1 - jfk_per) %>% 
  select(carrier, jfk_per, others_per) %>% 
  ungroup() %>% 
  arrange(jfk_per) %>% 
  mutate(carrier = factor(carrier, levels = carrier)) ->> df3

df3 %>% 
  ggplot() + 
  geom_segment(aes(x = 0, xend = jfk_per, y = carrier, yend = carrier), 
               size = 16, color = "#e41a1c") + 
  geom_segment(aes(x = jfk_per, xend = 1, y = carrier, yend = carrier), 
               size = 16, color = "#377eb8") ->> g1

g1  

library(scales)
g1 + scale_x_continuous(labels = percent) 

# Cải tiến đẹp hơn như sau: 
df3 %>% 
  ggplot() + 
  geom_segment(aes(x = 0, xend = jfk_per, y = carrier, yend = carrier, color = "JFK"), 
               size = 16) + 
  geom_segment(aes(x = jfk_per, xend = 1, y = carrier, yend = carrier, color = "Others"), 
               size = 16) 

df3 %>% 
  ggplot() + 
  geom_segment(aes(x = 0, xend = jfk_per, y = carrier, yend = carrier, color = "JFK"), 
               size = 16) + 
  geom_segment(aes(x = jfk_per, xend = 1, y = carrier, yend = carrier, color = "Others"), 
               size = 16) + 
  scale_color_manual(values = c('#e41a1c', '#377eb8'), name = "Airport")

# Hiệu chỉnh kích thước (tham khảo: https://stat.ethz.ch/pipermail/r-help/2014-March/367232.html): 

df3 %>% 
  ggplot() + 
  geom_segment(aes(x = 0, xend = jfk_per, y = carrier, yend = carrier, color = "JFK"), 
               size = 12) + 
  geom_segment(aes(x = jfk_per, xend = 1, y = carrier, yend = carrier, color = "Others"), 
               size = 12) + 
  scale_color_manual(values = c('#e41a1c', '#377eb8'), name = "Airport") + 
  theme(legend.title = element_text(face = "bold")) + 
  scale_x_continuous(labels = percent) + 
  labs(x = NULL, y = NULL, 
       title = "Rate of Use by Some U.S Largest Airlines in 2013 for JFK Airport", 
       caption = "Data Source: https://www.faa.gov/")

#---------------------------------------
#         Bổ sung Error Bar 
#---------------------------------------
  
# Trước hết tính mean, stadard deviation:   

iris_summary <- iris %>% 
  group_by(Species) %>%   
  summarise(mean_PL = mean(Petal.Length),
            sd_PL = sd(Petal.Length))


# Rồi vẽ: 
iris_summary %>% 
  ggplot(aes(Species, mean_PL)) + 
  geom_col(width = 0.7) + 
  geom_errorbar(aes(ymin = mean_PL - sd_PL, ymax = mean_PL + sd_PL), width = 0.15) 

# Vẽ kiểu khác: 

iris_summary %>% 
  ggplot(aes(Species, mean_PL)) + 
  geom_col(width = 0.6, color = "blue", fill = "blue", alpha = 0.3, size = 1) + 
  geom_errorbar(aes(ymin = mean_PL - sd_PL, ymax = mean_PL + sd_PL), width = 0.15)


# Dưới đây chúng ta sẽ hình ảnh hóa mức lương trung bình của 
# từng khu vực địa lí theo hai nhóm chủng tộc chính ở Hoa Kì
# đồng thời thể hiện error bar. Trước hết ta sử dụng bộ 
# dữ liệu CPS1988 từ gói AER. Chú ý rằng không như tình huống
# mà chúng ta vừa nghiên cứu, ở đây chúng ta đang muốn biểu
# diễn mức lương trung bình theo đồng thời cả hai yếu tố 
# định tính là: (1) khu vực, và (2) chủng tộc: 

library("AER")
data("CPS1988")

# Tính toán mức lương trung bình cùng độ lệch chuẩn đồng 
# thời tính error bar cộng trừ độ lệch chuẩn so với 
# giá trị trung bình: 

CPS1988 %>% 
  group_by(region, ethnicity) %>% 
  summarise(tb = mean(wage), 
            lc = sd(wage)) %>% 
  mutate(up = tb + lc, lo = tb - lc) ->> my_df


# Vẽ phác thảo và thấy cái hình chưa được như ý muốn: 
my_df %>% 
  ggplot(aes(region, tb, fill = ethnicity)) + 
  geom_col(position = "dodge") + 
  geom_errorbar(aes(ymin = lo, ymax = up), position = "dodge", width = 0.25)

# Hiệu chỉnh lại như sau: 
dodge <- position_dodge(width = 0.9)

my_df %>% 
  ggplot(aes(region, tb, fill = ethnicity)) + 
  geom_col(position = dodge, alpha = 0.6) +
  geom_errorbar(aes(ymin = lo, ymax = up), position = dodge, 
                width = 0.25, color = "gray20") ->> p

p

# Hoặc một kiểu khác: 

p + 
  scale_fill_manual(values = c("#999999", "#E69F00")) + 
  labs(x = NULL, y = NULL, 
       title = "Mean Wage by US Region and Race with Error Bar", 
       caption = "Data Source: Determinants of Wages Data (CPS 1988)")

# Hoặc một kiểu khác nữa: 

my_df %>% 
  ggplot(aes(region, tb, fill = ethnicity, color = ethnicity)) + 
  geom_col(position = dodge, alpha = 0.4, color = "grey50", size = 1) +
  geom_errorbar(aes(ymin = lo, ymax = up), position = dodge, width = 0.15) ->> g

g

g + 
  scale_fill_manual(values = c("#999999", "#E69F00")) + 
  scale_color_manual(values = c("#999999", "#E69F00")) + 
  labs(x = NULL, y = NULL, 
       title = "Mean Wage by US Region and Race with Error Bar", 
       caption = "Data Source: Determinants of Wages Data (CPS 1988)")
  
  
g +   
  scale_color_brewer(type = "qual", palette = "Dark2") +
  scale_fill_brewer(type = "qual", palette = "Dark2") + 
  labs(x = NULL, y = NULL, 
       title = "Mean Wage by US Region and Race with Error Bar", 
       caption = "Data Source: Determinants of Wages Data (CPS 1988)")







