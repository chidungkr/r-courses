


#----------------------------------------
#       Mục đính của Mini Projects : 
#  MIni Projects được thiết kế để nâng 
#  cao những kĩ năng sử dụng các hàm
#  cho Data Manipulation / Wrangling
#  đã được học ở lớp và gắn liền với 
#  những tình huống trong thực tế với 
#  bộ dữ liệu thực. 
#----------------------------------------


#========================================
#  Phần A: Phần này nhắc lại một số 
#  hàm quan trọng thường sử dụng cho 
#  Data Manipulation. Khuyến cáo là các
#  bạn nên ôn lại phần này. Nếu ai cảm
#  thấy chắc chắn thì có thể bỏ qua. 
#========================================


# Load các gói cần thiết: 
library(tidyverse) # Để sử dụng các hàm cho Data Wrangling. 
library(magrittr) # Để sử dụng họ toán tử pipe. 

#----------------------------------
#           rename()
#----------------------------------

# Lệnh này được sử dụng để đổi tên cho cột biến. Trước hết đọc dữ liệu
# với lựa chọn stringsAsFactors = FALSE để biến định tính ở dạng character: 

cps1988 <- read.csv("D:/GSO_R_Course/data_for_wrangling/CPS1988.csv", 
                    stringsAsFactors = FALSE)

# Rồi xem qua: 
cps1988 %>% head()
cps1988 %>% str()

# Đổi tên cho tất cả (hoặc một số cột biến): 
cps_rename1 <- cps1988 %>% 
  rename(luong = wage, 
         giao_duc = education, 
         kinh_nghiem = experience, 
         chung_toc = ethnicity, 
         do_thi = smsa, 
         khu_vuc = region, 
         lam_them = parttime)

# Một cách khác cũng rất hữu ích: 
names(cps1988) <- c("luong", "giao_duc", "kinh_nghiem", "chung_toc", "do_thi", 
                    "khu_vuc", "lam_them")

#----------------------------------
#           slice()
#----------------------------------

# Lệnh này dùng để chọn, ví dụ, dòng thứ 3 và 1001 của DF: 

my_df1 <- cps_rename1 %>% 
  slice(c(3, 1001))

# Chọn các dòng từ 5 đến 20, chọn dòng 100: 
my_df2 <- cps_rename1 %>% 
  slice(c(5:20, 100))

# Chọn dòng mà có giá trị lớn nhất ở luong: 

my_df3 <- cps_rename1 %>% 
  slice(which.max(luong))

# Chọn dòng mà quan sát có mức lương cao nhất - thấp nhất: 
my_df4 <- cps_rename1 %>% 
  slice(c(which.max(luong), which.min(luong)))


#----------------------------------
#           sample_n()
#----------------------------------

# Lệnh này dùng để lấy ngẫu nhiên n quan sát bất kì: 
data("trees")
trees %>% head()
trees %>% str()

# Lấy 10 quan sát trong tổng số 31 quan sát: 
tree_10 <- trees %>% sample_n(10)

# Hoặc lấy 30% các quan sát: 
tree_30 <- trees %>% sample_frac(0.3)

# Một cách khác để lấy ngẫu nhiên 10 quan sát theo một cách khác: 

tree_10_c2 <- trees[sample(nrow(trees), 10, replace = FALSE), ]

# (Nếu ai không hiểu cần chú ý giải thích kĩ hơn nữa). 

#----------------------------------
#           arrange()
#----------------------------------

# Lệnh này dùng để sắp xếp theo chiều tăng dần (giảm dần) ứng với cột biến nào đó: 
data("iris")
set.seed(1)
iris_small <- iris %>% sample_n(6)

# Sắp xếp theo chiều giảm dần của Sepal.Length: 

iris_small %>% arrange(-Sepal.Length)

# Còn nếu chiều giảm dần thì: 
iris_small %>% arrange(Sepal.Length)

# 2 quan sát cao nhất và thấp nhất: 
iris_small %>% 
  arrange(-Sepal.Length) %>% 
  slice(c(1:2, 5:6))

# Còn nếu là biến định tính: 
iris_small %>% 
  mutate(ID = c("A", "M", "U", "Z", "H", "P")) ->> iris_id

iris_id %>% arrange(ID)

#----------------------------------
#           filter()
#----------------------------------

# Lệnh này dùng để lọc các quan sát theo một (một số) điều kiện nào đó: 

iris_id %>% filter(Species == "setosa")
iris_id %>% filter(Species == "setosa" & Sepal.Length >= 5)
iris_id %>% filter(Sepal.Length >= 5.5 | Petal.Length >= 2)

# Cặp lệnh sau là tương đương: 
iris_id %>% filter(Species != "setosa")
iris_id %>% filter(Species == "versicolor" | Species == "virginica")

# Đương nhiên chúng là tương đương với: 
muon_lay <- c("versicolor", "virginica")
iris_id %>% filter(Species %in% muon_lay)


# Tương tự, hai lệnh sau là tương đương nhau về hiệu ứng: 
iris_id %>% filter(!Sepal.Length >= 5.5) 
iris_id %>% filter(Sepal.Length < 5.5) 

# Lọc dữ liệu không trùng nhau. Trước hết tạo một DF giả định: 

student <- data.frame(name = c("An", "Dung", "Hoang", "Hoa", "Trang"), 
                      grade = c(9, 8.1, 9, 9.2, 9.5), 
                      province = c("Ha Noi", "Nghe An", NA, "Thai Binh", "Hai Phong"), 
                      college = c("NEU", "FTU", "NEU", "VNU", "VNU"))

# Các đại biểu không đến từ cùng một đại học: 
student %>% filter(!duplicated(college))

# Các đại biểu không có số điểm như nhau: 
student %>% filter(!duplicated(grade))

# Đại biểu bị trùng điểm: 
student %>% filter(duplicated(grade))

# Đại biểu có đủ thông tin về quê quán: 
student %>% filter(!is.na(province))


#----------------------------------
#           case_when()
#----------------------------------

# Một kiểu ứng dụng: 

iris_id %>% 
  mutate(phan_loai = case_when(Sepal.Length < 5.5 ~ "Loai_A", 
                               Sepal.Length >= 5.5 & Sepal.Length < 6.5 ~ "Loai_B", 
                               Sepal.Length >= 6.5 ~ "Loai_C"))

# Một kiểu khác: 
chet_nguoi <- c("A", "H")

iris_id %>% 
  mutate(Die = case_when(ID %in% chet_nguoi ~ "Yes", 
                         !ID %in% chet_nguoi ~ "No"))
# Hoặc là: 
iris_id %>% 
  mutate(Die = case_when(Species == "virginica" ~ "Yes", 
                         Species != "virginica" ~ "No"))

# Một ứng dụng trong thực tế (dữ liệu hmeq.csv, phân loại thu nhập). 

#----------------------------------
#           gather()
#----------------------------------

# Lệnh này dùng để chuyển form của dữ liệu. Trước hết tạo một DF giả định: 

stock_price <- data.frame(ngay = 1:3, A = 4:6, B = 11:13, C = c(3, 1, 4))

# Chuyển về định dạng của Panel Data (long form): 

stock_price %>% 
  gather(a, b, -ngay)

stock_price %>% 
  gather(ticker, price, -ngay) ->> long_df

long_df

# Có thể làm ngược lại: 
long_df %>% 
  spread(ticker, price)

#----------------------------------
#           mutate()
#----------------------------------

# Lệnh này dùng để tạo ra cột biến mới. Ví dụ luong2 = luong^2: 
cps_rename1 <- cps_rename1 %>% mutate(luong2 = luong^2)

# Chuyển hóa tất cả cột biến là character về factor: 
cps_rename1_to_factor <- cps_rename1 %>% mutate_if(is.character, as.factor)

# Thách thức nhỏ: bình phương tất cả các cột biết định lượng. Làm như sau: 

binh_phuong <- function(x) {x^2}
cps_binh_phuong <- cps_rename1 %>% mutate_if(is.numeric, binh_phuong)


#----------------------------------
#           select()
#----------------------------------
cps1988 <- read.csv("D:/GSO_R_Course/data_for_wrangling/CPS1988.csv", 
                    stringsAsFactors = FALSE)

# Lệch này dùng để lựa chọn cột biến mong muốn từ một DF: 
df_small <- cps1988 %>% 
  select(wage, experience)

df_small %>% head()

# Hoặc chọn cột biến theo một tiêu chuẩn nào đó, ví dụ như, cột là biến số: 
df_num <- cps1988 %>% select_if(is.numeric)
df_num %>% head()

# Đáng chú ý là ta có thể vừa chọn cột biến, vừa đổi tên: 
df_rename_small <- cps1988 %>% 
  select(luong = wage, kinh_nghiem = experience)

df_rename_small %>% head()

# Cũng có thể dùng select để xóa một cột biến không mong muốn: 

df_remove <- df_rename_small %>% select(-luong)
cps1988_remove <- cps1988 %>% select(-wage, - smsa)

# Hoặc đưa, ví dụ, một cột biến lên vị trí đầu tiên và giữ nguyên tất cả: 
cps1988_first <- cps1988 %>% select(region, everything())

# Hoặc cột biến nào có chứa chữ a: 
cps1988_a <- cps1988 %>% select(contains("a"))

# Hoặc kết thúc là chữ e: 
cps1988_e <- cps1988 %>% select(ends_with("e"))


#------------------------
#   summarise_each()
#------------------------

# Load dữ liệu: 
pisa <- read.csv("D:/GSO_R_Course/data_for_wrangling/PISA DATA (INTERNATIONAL).CSV", 
                 stringsAsFactors = FALSE)

# Xem qua dữ liệu: 
pisa %>% dim()
pisa %>% str()

# Lấy ra một số cột biến: 
pisa_small <- pisa %>% select(Country = CNT, 
                              Gender = ST04Q01, 
                              MATH, SCIE, READ)

# Các thống kê cơ bản chung về điểm toán: 

pisa_small %>% 
  summarise_each(funs(mean, median, sd, min, max), MATH)

# Các thống kê tương ứng với từng quốc gia cho điêm Toán
# và sắp xếp theo chiều giảm dần của điểm trung bình: 

pisa_small %>% 
  group_by(Country) %>% 
  summarise_each(funs(mean, median, sd, min, max, n()), MATH) %>% 
  arrange(-mean)



#========================================================
#  Phần B: Đây là phần quan trọng vì nó minh họa cách 
#  thức xử lí vấn đề trong thực tế như thế nào.
#========================================================


#----------------------------------------------------------
#  Tình huống 1. Bộ dữ liệu wt16.dta (trính từ VHLSS 2016)
#  có một số tên tỉnh không trùng với tên tỉnh được cung 
#  cấp từ  https://www.gso.gov.vn/dmhc2015 của GSO. Nếu
#  chúng ta muốn nối hai bộ dữ liệu này theo tên tỉnh thì
#  vấn đề đầu tiên chúng ta cần giải quyết đồng nhất các 
#  khác biệt về tên tỉnh trước khi sử dụng right_joint(). 
#----------------------------------------------------------

#----------------------------------------
#     right_joint() / inner_joint()
#----------------------------------------

# Đọc dữ liệu từ wt16.dta: 
library(haven)
my_vhlss <- read_stata("wt16.dta") # Cách 1. 
my_vhlss <- read_stata("D:/GSO_R_Course/data_for_wrangling/wt16.dta") # Cách 2.  

# Xem qua mô tả về các cột biến: 
head(my_vhlss)
lapply(my_vhlss, class)
lapply(my_vhlss, attributes)

# Số lượng các tỉnh thành: 
my_vhlss$tinh %>% n_distinct()

# Các mã theo kiểu VHLSS: 
my_vhlss$tinh %>% unique()

# Mã các tỉnh của GSO (lấy từ https://www.gso.gov.vn/dmhc2015/): 

province_code <- read.table("D:/GSO_R_Course/data_for_wrangling/code_province_GSO.txt", 
                            stringsAsFactors = FALSE, header = TRUE)

# Xem qua dữ liệu mã tỉnh: 
province_code %>% str()
province_code %>% tail()

# Bỏ dòng 64 đồng thời đổi tên cho cột biến code:
province_code %<>% 
  slice(-64) %>% 
  rename(tinh = code)

# Dán lại nhãn cho tinh của my_vhlss (cách 1)
my_vhlss %<>% mutate(tinh = case_when(tinh == 1 ~ "01", 
                                      tinh == 2 ~ "02", 
                                      tinh == 4 ~ "04", 
                                      tinh == 6 ~ "06", 
                                      tinh == 8 ~ "08", 
                                      tinh >= 10 ~ as.character(tinh)))

# So sánh: 
my_vhlss$tinh %>% unique()
province_code$tinh %>% unique()


# Chuyển hóa tinh ở my_vhlss về character: 
my_vhlss %<>% mutate(tinh = as.character(tinh))
my_vhlss %>% head()

# Lúc này ta mới ghép dữ liệu được: 
total_df <- right_join(my_vhlss, province_code, by = "tinh")

# Xem qua: 
total_df %>% head()

# Có thể sử dụng inner_joint(): 
total_df_c2 <- inner_join(my_vhlss, province_code, by = "tinh")


# Phương án 2 là viết hàm thêm số 0 (trình bày kĩ hơn ở phần viết hàm): 

library(stringr)
add_zero <- function(x) {
  case_when(str_count(x) == 1 ~ str_c(0, x), 
            str_count(x) == 2 ~ x)
}



#--------------------------------------------------------
#  Tình huống 2: Sừ dụng bộ sỗ liệu flights_revised.csv
#  về các chuyến bay từ ba sân bay lớn ở New York. 
#   để  đưa ra nhiều Insights nhất có thể từ bộ 
#  dữ liệu này. Các cột biến với ý nghĩa như sau: 
#  1. dep_time là thời gian khởi hành thực tế, sched_dep_time
#  là thời gian khởi hành mà hãng bay công bố. 
#  2. dep_delay là thời gian khởi hành trễ. Nếu là số âm thì 
#   khởi hành sớm hơn dự định (và ngược lại)
#  3. carrier là thông tin về hãng hàng không. 
#  4. origin là sân bay cất cánh, dest là sân bay đến. 
#  5. distance là khoảng cách của tuyến bay. 
#  6. hour, minute là thời gian bay của các chuyến (giờ, phút). 
#--------------------------------------------------------

# Đọc dữ liệu và xem qua: 
fligh_df <- read_csv("D:/GSO_R_Course/data_for_wrangling/flights_revised.csv") 
fligh_df %>% dim()
fligh_df %>% str()

# Kiểm tra dữ liệu thiếu. Trước hết viết hàm kiểm tra dữ liệu thiếu: 

na_number <- function(x) {x %>% is.na() %>% sum()}
sapply(fligh_df, na_number) # Cách 1. 
fligh_df %>% summarise_all(na_number)

# Tạm thời chấp nhận phương án bỏ dữ liệu thiếu: 
fligh_df_full <- fligh_df %>% na.omit()
fligh_df_full %>% dim()

# Tên (kí hiệu) của các hãng hàng không: 
fligh_df$carrier %>% unique()

# Số hãng hàng không có trong bộ dữ liệu: 
fligh_df_full$carrier %>% unique() %>% length()
fligh_df_full$carrier %>% n_distinct()

# Chuyến bay có thời gian cất cánh sớm nhất - muộn nhất theo từng hãng: 
fligh_df_full %>% 
  group_by(carrier) %>% 
  top_n(1, dep_delay) %>% 
  select(dep_delay, carrier) %>% 
  arrange(-dep_delay) # Cất cánh muộn. 

fligh_df_full %>% 
  group_by(carrier) %>% 
  top_n(1, -dep_delay) %>% 
  select(dep_delay, carrier) %>% 
  arrange(dep_delay) # Cất cánh sớm. 

# 5 hãng hàng không có nhiều và ít nhất các chuyến bay: 
fligh_df_full %>% 
  group_by(carrier) %>% 
  count() %>% 
  arrange(-n) ->> number_flight

number_flight

# Thị phần của các hãng hàng không: 
number_flight %<>% mutate(market_share = n / nrow(fligh_df_full))
number_flight

# 6 hãng hàng không chiếm xấp xỉ 82% thị phần: 
number_flight %>% mutate(cum_percent = cumsum(market_share)) # Giải thích tại sao? 

number_flight %>% 
  ungroup() %>% 
  mutate(cum_percent = cumsum(market_share))

# Số lượng các chuyến bay cất cánh từ các sân bay khác nhau: 
fligh_df_full %>% 
  group_by(origin) %>% 
  count()

# Số lượng các chuyến bay đến các sân bay khác nhau: 
fligh_df_full %>% 
  group_by(dest) %>% 
  count() %>% 
  ungroup() %>% 
  arrange(-n) %>% 
  slice(c(1:5, (nrow(.) - 5):nrow(.)))

# Khoảng cách về Km: 
fligh_df_full %<>% mutate(distance_km = distance / 0.62137)

# Chuyến bay dài nhất và ngắn nhất: 
fligh_df_full %>% 
  filter(!duplicated(distance_km)) %>% 
  arrange(-distance_km) %>% 
  select(distance_km, carrier, everything()) ->> distance_df

distance_df %>% head()
distance_df %>% tail()

# Bổ sung thêm dữ liệu về tháng - ngày trong tuần: 
library(lubridate)

fligh_df_full %<>%  
  mutate(thang_c1 = month(time_hour, label = TRUE, abbr = TRUE), 
         thang_c2 = month(time_hour, label = FALSE, abbr = FALSE), 
         ngay = wday(time_hour, label = TRUE, abbr = TRUE))

# Số các chuyến bay theo ngày trong tuần và tháng: 

fligh_df_full %>% 
  group_by(ngay) %>% 
  count()

fligh_df_full %>% 
  group_by(thang_c1) %>% 
  count()

fligh_df_full %>% 
  group_by(thang_c1, ngay) %>% 
  count()

fligh_df_full %>% 
  group_by(thang_c1, ngay) %>% 
  count() %>% 
  ggplot(aes(ngay, n)) + 
  geom_col() + 
  facet_wrap(~ thang_c1)

# Bổ sung thêm thông tin về long - lat cho bộ dữ liệu với sân bay đến
# từ https://opendata.socrata.com/dataset/Airport-Codes-mapped-to-Latitude-Longitude-in-the-/rxrh-4cxm/data: 

geo_data <- read.csv("D:/GSO_R_Course/data_for_wrangling/long_lat_airport.csv", 
                     stringsAsFactors = FALSE)
geo_data %>% head()

# Đổi tên: 
geo_data %<>% rename(dest = locationID)

# So sánh qua: 
geo_data %>% head()

fligh_df_full %>% 
  select(dest, everything()) %>% 
  head()


# Bổ sung dữ liệu địa lí: 

fligh_df_full_long_lat <- inner_join(fligh_df_full, geo_data, by = "dest")