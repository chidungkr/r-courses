

#=======================================
#      Mapping / Choropleth Maps 
#=======================================


#---------------------------------------
#     Mapping (bản đồ hành chính)
#---------------------------------------

# Lấy dữ liệu địa lí cho VN đến cấp tỉnh: 
library(raster)

# Và dữ liệu địa lí của VN đến cấp tỉnh: 
vietnam_province <- getData("GADM", country = "Vietnam", level = 1)

# Gỡ bỏ sử dụng raster: 
detach(package:raster)

# Lưu ý về kiểu liệu: 
library(tidyverse)
vietnam_province %>% class()

# Chuyển hóa về data frame quen thuộc: 
vietnam_df <- vietnam_province %>% fortify(region = "NAME_1")

# Xem qua dữ liệu: 
vietnam_df %>% head()

# Các tỉnh thành: 
province <- vietnam_df$id %>% unique()
province

# Bản đồ Việt Nam:  
theme_set(theme_minimal())

ggplot() + 
  geom_polygon(data = vietnam_df, aes(long, lat, group = group)) -> m1

m1

# Hiệu chỉnh:
m1 + coord_fixed(1)
m1 + coord_equal()

# Bản đồ hành chính đến cấp tỉnh (Kiểu 1): 

ggplot() +
  geom_polygon(data = vietnam_df, aes(long, lat, group = group), fill = "gray80", color = "blue") +
  coord_equal()

# Giả sử chúng ta muốn biểu diễn vị trí của Hà Nội ( https://www.google.com):

ha_noi <- data.frame(lat = 21.040002, long = 105.834388)

ggplot() +
  geom_polygon(data = vietnam_df,
               aes(long, lat, group = group),
               fill = "gray80", color = "blue") +
  coord_equal() + 
  geom_point(data = ha_noi, aes(long, lat), color = "red", size = 3)

# Một kiểu khác: 
ggplot() +
  geom_polygon(data = vietnam_df, aes(long, lat, group = group), fill = NA, color = "red") + 
  coord_equal()


# Tô màu để phân biệt các tỉnh: 
ggplot() +
  geom_polygon(data = vietnam_df, aes(long, lat, group = group, fill = id), show.legend = FALSE) +
  coord_equal()

# Thêm đường ranh giới giữa các tỉnh (cách 1): 
ggplot() +
  geom_polygon(data = vietnam_df, 
               aes(long, lat, group = group, fill = id), 
               show.legend = FALSE, color = "grey50") +
  coord_equal()

# Cách 2: 
ggplot() +
geom_polygon(data = vietnam_df,
             aes(long, lat, group = group, fill = id),
             show.legend = FALSE) +
  geom_path(data = vietnam_df, aes(long, lat, group = group),
            color = "grey50", size = 0.1) +
  coord_equal()


#---------------------------
#     Choropleth Maps 
#---------------------------
# https://rpubs.com/chidungkt/388218
# https://rpubs.com/chidungkt/388184
# https://rpubs.com/chidungkt/388254 

# Dữ liệu về đói nghèo (lấy từ http://www.gso.gov.vn/default_en.aspx?tabid=783): 

poverty <- read.csv("D:\\GSO_R_Course\\data_for_visualization\\E11.35.csv", sep = ";")

# Xem qua: 
poverty %>% head()

# Lấy rowname và chuyển hóa về Latin-ASCII: 
library(stringi)
library(magrittr)

poverty %<>% mutate(id = rownames(.), 
                    id =  stri_trans_general(id, "Latin-ASCII"))
poverty %>% head()

# Đổi tên cho cột biến: 
names(poverty) <- c("poverty", "id")

# Bỏ ba dòng đầu: 
poverty %<>% slice(-c(1:3))
poverty %>% str()

# So sánh: 
province %>% head()

# Chuyển hóa về Latin-ASCII: 
vietnam_df %<>% mutate(id = stri_trans_general(id, "Latin-ASCII"))

# Các tỉnh thành của Việt Nam lúc này: 
province <- vietnam_df$id %>% unique()
province %>% head()

# So sánh: 
setdiff(poverty$id, province)

# Vậy thì cần đổi tên cho một số: 

poverty %<>% mutate(id = case_when(id == "Quang  Nam" ~ "Quang Nam", 
                                   id == "Thua Thien-Hue" ~ "Thua Thien - Hue", 
                                   id == "Quang  Ngai" ~ "Quang Ngai", 
                                   id == "Khanh  Hoa" ~ "Khanh Hoa", 
                                   id == "Ninh  Thuan" ~ "Ninh Thuan", 
                                   id == "Kien  Giang" ~ "Kien Giang", 
                                   TRUE ~ id))

# Giờ kiểm tra lại: 
setdiff(poverty$id, province) ->> khac_biet
khac_biet

# Lọc dữ liệu: 
poverty %<>% filter(!id %in% khac_biet)
poverty %>% dim()

# Chuyển hóa cột biến poverty về dạng số: 
poverty %<>% mutate(rate = poverty %>% as.character() %>% as.numeric())

# Nối dữ liệu: 
vietnam_df_poverty <- right_join(vietnam_df, poverty, by = "id")

# Bản đồ tỉ lệ nghèo (vẽ phác thảo): 
ggplot() + 
  geom_polygon(data = vietnam_df_poverty, 
               aes(long, lat, group = group, fill = rate), color = "white") +
  coord_equal() + 
  labs(title = "Poverty Rate in Vietnam by Province",
       subtitle = "Note: Data Is Not Available for\nVietnam's Paracel and Spratly Islands",
       caption = "Data Source: General Statistics Office Of Vietnam") ->> m1

m1

# Cải tiến cho cái theme:

m1 + 
  theme(text = element_text(color = "#22211d", face = "bold")) + 
  theme(axis.line = element_blank()) + 
  theme(axis.text.x = element_blank()) + 
  theme(axis.text.y = element_blank()) + 
  theme(axis.ticks = element_blank()) + 
  theme(axis.title.x = element_blank()) + 
  theme(axis.title.y = element_blank()) + 
  theme(panel.grid.minor = element_blank()) + 
  theme(panel.grid.major = element_line(color = "#ebebe5", size = 0.2)) + 
  theme(plot.background = element_rect(fill = "#f5f5f2", color = NA)) + 
  theme(panel.background = element_rect(fill = "#f5f5f2", color = NA)) + 
  theme(legend.background = element_rect(fill = "#f5f5f2", color = NA)) + 
  theme(panel.border = element_blank())



# Nếu thé nên viết thành hàm để sử dụng nhiều lần: 

my_theme_for_map <- function(...) {
  theme(text = element_text(color = "#22211d", face = "bold")) + 
    theme(axis.line = element_blank()) + 
    theme(axis.text.x = element_blank()) + 
    theme(axis.text.y = element_blank()) + 
    theme(axis.ticks = element_blank()) + 
    theme(axis.title.x = element_blank()) + 
    theme(axis.title.y = element_blank()) + 
    theme(panel.grid.minor = element_blank()) + 
    theme(panel.grid.major = element_line(color = "#ebebe5", size = 0.2)) + 
    theme(plot.background = element_rect(fill = "#f5f5f2", color = NA)) + 
    theme(panel.background = element_rect(fill = "#f5f5f2", color = NA)) + 
    theme(legend.background = element_rect(fill = "#f5f5f2", color = NA)) + 
    theme(panel.border = element_blank())
  
}

# Do vậy: 
m1 + my_theme_for_map() ->> m2
m2

# Cải tiến nữa: 

library(viridis)
m2 +  
  scale_fill_viridis(direction = -1, option = "A", "Poverty Rate") + 
  theme(legend.position = c(0.2, 0.5))

# Hoặc một kiểu khác: 
m2 +  
  scale_fill_viridis(direction = -1, 
                     option = "B", 
                     name = "Poverty Rate", 
                     guide = guide_colourbar(direction = "horizontal",
                                             barheight = unit(2, units = "mm"),
                                             barwidth = unit(50, units = "mm"),
                                             draw.ulim = F,
                                             title.hjust = 0.5,
                                             label.hjust = 0.5, 
                                             title.position = "top")) + 
  theme(legend.position = "top")


m2 +  
  scale_fill_viridis(direction = -1, 
                     option = "D", 
                     name = "Poverty Rate", 
                     guide = guide_colourbar(direction = "horizontal",
                                             barheight = unit(3, units = "mm"),
                                             barwidth = unit(50, units = "mm"),
                                             draw.ulim = F,
                                             title.hjust = 0.5,
                                             label.hjust = 0.5, 
                                             title.position = "top")) + 
  theme(legend.position = c(0.3, 0.45))

# Nếu cần tham khảo thêm về: 
# 1. cách sử dụng màu sắc tại https://rpubs.com/chidungkt/388254
# 2. Bản đồ cấp xã - hoặc huyện tại https://rpubs.com/chidungkt/388184


#==============================================
#    Lollipop Chart / Cleveland’s Dot
#==============================================

#----------------------------
#      Cleveland’s Dot
#----------------------------

# Load dữ liệu (nguồn từ http://www.gso.gov.vn/default_en.aspx?tabid=783): 

library(readxl)
income <- read_excel("D:\\GSO_R_Course\\data_for_visualization\\E11.24.xlsx")

# Dòng thứ 3 chính là các tên cột biến. Căn cứ vào đó: 
col_names <- c("province", 
               paste(c("gen", "q1", "q2", "q3", "q4", "q5"), "2010", sep = "_"), 
               paste(c("gen", "q1", "q2", "q3", "q4", "q5"), "2012", sep = "_"), 
               paste(c("gen", "q1", "q2", "q3", "q4", "q5"), "2014", sep = "_"), 
               paste(c("gen", "q1", "q2", "q3", "q4", "q5"), "2016", sep = "_"))

# Đổi tên cột biến: 
names(income) <- col_names

# Loại NA: 
income %<>% na.omit()
# Loại tiếp các dòng không cần thiết (cách này thủ công và dễ hiểu): 
income$province
income %<>% slice(-c(1, 2, 14, 29, 44, 50, 57))

# Lấy dữ liệu của hai năm là 2010 và 2016 và sắp xếp luôn: 
df_1016 <- income %>% 
  select(province, gen_2016, gen_2010) %>% 
  mutate(gen_2010 = as.numeric(gen_2010), 
         gen_2016 = as.numeric(gen_2016), 
         province = stri_trans_general(province, "Latin-ASCII")) %>% 
  mutate(per = (gen_2016 / gen_2010 - 1)) %>% 
  mutate(above = case_when(per >= mean(per) ~ "Above Average", 
                           per < mean(per) ~ "Below Average")) %>% 
  arrange(per) %>% 
  mutate(province = factor(province, levels = province))


# Vẽ phác. Plot này cho thấy Hải Phòng, Bắc Ninh và
# Thái Nguyên là ba tỉnh tăng nhanh nhất: 

p <- df_1016 %>% 
  ggplot(aes(province, per)) + 
  geom_segment(aes(x = province, xend = province, 
                   y = 0, yend = per), color = "#0e668b", size = 1.2) + 
  geom_point(size = 4, color = "#0e668b") + 
  coord_flip() + 
  labs(x = NULL, y = NULL, 
       title = "Monthly Average Income Growth at Current Prices\nfrom 2010 to 2016 for 30 Provinces Selected", 
       caption = "Data Source: General Statistics Office Of Vietnam")

p

# Cải tiến: 
p + 
  theme_bw() + 
  theme(plot.background = element_rect(fill = "#f7f7f7")) + 
  theme(panel.background = element_rect(fill = "#f7f7f7")) + 
  theme(panel.grid.minor = element_blank()) + 
  theme(panel.grid.major.y = element_blank()) + 
  theme(panel.grid.major.x = element_line()) + 
  theme(axis.ticks = element_blank()) + 
  theme(panel.border = element_blank()) 

# Nên viết thành hàm: 
my_theme <- function(...) {
  theme_bw() + 
    theme(plot.background = element_rect(fill = "#f7f7f7")) + 
    theme(panel.background = element_rect(fill = "#f7f7f7")) + 
    theme(panel.grid.minor = element_blank()) + 
    theme(panel.grid.major.y = element_blank()) + 
    theme(panel.grid.major.x = element_line()) + 
    theme(axis.ticks = element_blank()) + 
    theme(panel.border = element_blank()) 
}

# Sử dụng hàm: 
p + my_theme()
      
# Với mục đích minh họa, giả sử chỉ chọn 40 tỉnh: 

df_1016 <- income %>% 
  select(province, gen_2016, gen_2010) %>% 
  slice(1:30) %>% 
  mutate(gen_2010 = as.numeric(gen_2010), 
         gen_2016 = as.numeric(gen_2016), 
         province = stri_trans_general(province, "Latin-ASCII")) %>% 
  mutate(per = (gen_2016 / gen_2010 - 1)) %>% 
  mutate(above = case_when(per >= mean(per) ~ "Above Average", 
                           per < mean(per) ~ "Below Average")) %>% 
  arrange(per) %>% 
  mutate(province = factor(province, levels = province)) 


# Vẽ và cải tiến luôn: 

library(scales)

df_1016 %>% 
  ggplot(aes(province, per)) + 
  geom_segment(aes(x = province, xend = province, 
                   y = 0, yend = per), color = "#0e668b", size = 1.2) + 
  geom_point(size = 4, color = "#0e668b") + 
  coord_flip() + 
  labs(x = NULL, y = NULL, 
       title = "Monthly Average Income Growth at Current Prices\nfrom 2010 to 2016 for 30 Provinces Selected", 
       caption = "Data Source: General Statistics Office Of Vietnam") + 
  my_theme() + 
  scale_y_continuous(labels = percent) ->> g

g

# Hiệu chỉnh: 
g + theme(axis.text.y = element_text(face = "bold", color = "#0e668b", size = 9))

# Nhấn mạnh đến 5 tình đứng đầu về tốc độ tăng: 
g + 
  geom_segment(data = df_1016 %>% top_n(5, per), 
               aes(x = province, xend = province, y = 0, yend = per), color = "orange", size = 1.2) + 
  geom_point(size = 4, color = "orange", data = df_1016 %>% top_n(5, per)) ->> g1

g1

# Hoặc thêm 5 ông bét bảng: 

g1 + 
  geom_segment(data = df_1016 %>% top_n(5, -per), 
               aes(x = province, xend = province, 
                   y = 0, yend = per), color = "purple", size = 1.2) + 
  geom_point(size = 4, color = "purple", data = df_1016 %>% top_n(5, -per)) ->> g2 

g2

# Hiệu chỉnh tiếp: 
g2 + 
  scale_y_continuous(expand = c(0, 0), 
                     labels = percent, 
                     breaks = seq(0, 1.75, by = 0.25), 
                     limits = c(0, 1.8)) ->> g3 

g3

# Tô màu cho hai nhóm: 
df_1016 %>% 
  ggplot(aes(province, per, color = above)) + 
  geom_segment(aes(x = province, xend = province, 
                   y = 0, yend = per), size = 1.2) + 
  geom_point(size = 4) + 
  scale_color_manual(values = c("orange", "#0e668b")) + 
  coord_flip() + 
  my_theme() + 
  theme(legend.position = "none") + 
  theme(axis.text.y = element_text(face = "bold", size = 8)) + 
  scale_y_continuous(expand = c(0, 0), 
                     labels = percent_format(), 
                     breaks = seq(0, 1.75, by = 0.25), 
                     limits = c(0, 1.8)) ->> g4 

g4


# Hiệu chỉnh thêm: 

g4 + 
  geom_segment(aes(y = 1.45, yend = 1.45, x = "Vinh Phuc", xend = "Quang Binh"),
               arrow = arrow(length = unit(0.2,"cm")), color = "orange", size = 1) +
  geom_segment(aes(y = 1.45, yend = 1.45, x = "Lao Cai", xend = "Quang Tri"),
               arrow = arrow(length = unit(0.2,"cm")), color = "#0e668b", size = 1) + 
  annotate("text", 
           x = "Vinh Phuc", 
           y = 1.5, 
           label = "Above Average", 
           color = "orange", 
           size = 4, 
           hjust = 0.1, 
           vjust = -1) + 
  annotate("text", 
           x = "Lao Cai", 
           y = 1.5, 
           label = "Below Average", 
           color = "#0e668b", 
           size = 4, 
           hjust = 0.1, 
           vjust = 2) ->> g5

g5 

# Hiệu chỉnh thêm nữa: 
g5 + 
  labs(x = NULL, y = NULL, 
       title = "Monthly Average Income Growth at Current Prices from 2010 to 2016 for\n30 Provinces Selected.", 
       subtitle = "According to a forecast by PricewaterhouseCoopers in February 2017, Vietnam may be the fastest-growing\nof the world's economies, with a potential annual GDP growth rate of about 5.1%,\nwhich would make its economy the 20th-largest in the world by 2050.", 
       caption = "Data Source: General Statistics Office Of Vietnam") 

#-----------------------------
#       Lollipop Chart 
#-----------------------------

# Sắp xếp theo gen_2016: 

income %<>%  
  select(province, gen_2010, gen_2016) %>% 
  mutate(gen_2010 = as.numeric(gen_2010), 
         gen_2016 = as.numeric(gen_2016)) %>% 
  arrange(gen_2016) %>% 
  mutate(province = factor(province, levels = province)) 


# Cách 1: 

income %>% 
  ggplot(aes(x = province)) + 
  geom_segment(aes(y = gen_2010, yend = gen_2016, 
                   x = province, xend = province), color = "gray40", size = 1) + 
  geom_point(aes(x = province, y = gen_2010, color = "i_love"), size = 3.5) + 
  geom_point(aes(x = province, y = gen_2016, color = "you"), size = 3.5) + 
  coord_flip() + 
  scale_color_manual(name = "Monthly Income:", labels = c(2010, 2016), 
                     values = c("#FFB5C5", "#EE3A8C")) + 
  labs(x = NULL, y = NULL, 
       title = "Monthly Average Income Growth at Current Prices\nfrom 2010 to 2016 for 63 Provinces of Vietnam.", 
       subtitle = "The data draw on GSO's compilation of internationally comparable statistics about\nglobal development and the quality of Vietnam people's lives.", 
       caption = "Data Source: General Statistics Office Of Vietnam") + 
  scale_y_continuous(expand = c(0, 0), 
                     breaks = seq(500, 6000, by = 500), 
                     limits = c(500, 5700)) ->> p
p

# Hiệu chỉnh: 
library(extrafont)
extrafont::loadfonts(device = "win")

p + 
  theme_bw() + 
  theme(plot.background = element_rect(fill = "white")) + 
  theme(panel.grid.minor = element_blank()) + 
  theme(panel.grid.major.y = element_blank()) + 
  theme(panel.grid.major.x = element_line()) + 
  theme(axis.ticks = element_blank()) + 
  theme(panel.border = element_blank()) + 
  theme(text = element_text(family = "Georgia", size = 13, color = "black")) + 
  theme(plot.subtitle = element_text(color = "gray20", size = 10, face = "italic")) + 
  theme(legend.title = element_text(size = 10, color = "gray20")) + 
  theme(legend.position = "top")

# Viết thành hàm cho theme: 

my_theme_for_lollipop <- function(...) {
  theme_bw() + 
    theme(plot.background = element_rect(fill = "white")) + 
    theme(panel.grid.minor = element_blank()) + 
    theme(panel.grid.major.y = element_blank()) + 
    theme(panel.grid.major.x = element_line()) + 
    theme(axis.ticks = element_blank()) + 
    theme(panel.border = element_blank()) + 
    theme(text = element_text(family = "Georgia", size = 13, color = "black")) + 
    theme(plot.subtitle = element_text(color = "gray20", size = 10, face = "italic")) + 
    theme(legend.title = element_text(size = 10, color = "gray20")) + 
    theme(legend.position = "top")
}

# Một phiên bản cải tiến: 

income %>% 
  select(province, gen_2010, gen_2016) %>% 
  mutate(gen_2010 = as.numeric(gen_2010), 
         gen_2016 = as.numeric(gen_2016)) %>% 
  arrange(gen_2016) %>% 
  mutate(province = factor(province, levels = province)) %>% 
  mutate(th = 500) %>% 
  ggplot(aes(province)) + 
  geom_segment(aes(y = th, yend = gen_2010, 
                   x = province, xend = province), color = "gray80", linetype = 2) + 
  geom_segment(aes(y = gen_2010, yend = gen_2016, 
                   x = province, xend = province), color = "gray40", size = 1) + 
  geom_point(aes(x = province, y = gen_2010, color = "a"), size = 3.5) + 
  geom_point(aes(x = province, y = gen_2016, color = "b"), size = 3.5) + 
  coord_flip() + 
  my_theme_for_lollipop() + 
  scale_color_manual(name = "Monthly Income:", labels = c(2010, 2016), 
                     values = c("#FFB5C5", "#EE3A8C")) + 
  labs(x = NULL, y = NULL, 
       title = "Monthly Average Income Growth at Current Prices from\n2010 to 2016 for 63 Provinces of Vietnam.", 
       subtitle = "The data draw on GSO's compilation of internationally comparable statistics about\nglobal development and the quality of Vietnam people's lives.", 
       caption = "Data Source: General Statistics Office Of Vietnam") + 
  scale_y_continuous(breaks = seq(500, 5500, by = 500), 
                     expand = c(0, 0), 
                     limits = c(500, 5700)) 




