


#======================================================================
#  Text Processing
#  Data: http://www.mediafire.com/file/z0v19g6fs3ej85e/201711.rar/file
#======================================================================


#------------------
#   str_count()
#------------------

library(tidyverse)
library(magrittr)

# Đường dẫn đến các file dữ liệu: 
files_path <- dir("D:/GSO_R_Course/data_for_wrangling/201711", full.names = TRUE)

# Đọc tất cả dữ liệu này: 
library(readxl)
all_data <- lapply(files_path, read_excel)

# Nhớ rằng all_data là list: 
all_data %>% class()

# Xem qua: 
lapply(all_data, head)
lapply(all_data, names)

# Chuyển dữ liệu từ list về data frame: 
all_data_df <- do.call("bind_rows", all_data)
all_data_df %>% class()
all_data_df %>% head()

# Để không xẩy ra các kết quả nằm ngoài ý muốn nên để phông
# chữ cần xử lí về cùng một kiể, ví dụ, như Latin-ASCII: 

library(stringi)
all_data_df %<>% mutate(ID = stri_trans_general(ID, "Latin-ASCII"), 
                        TENCN = stri_trans_general(TENCN, "Latin-ASCII"))

# Xem qua một số: 
all_data_df$ID %>% head()

# Hiệu ứng của lệnh str-count(): 
text1 <- c("Dung", "An Chu", "Chi Dung", "Lop 12A")
text1 %>% str_count()


# Số lượng các kí tự được sử dụng: 
all_data_df$ID %>% str_count() %>% unique()

#------------------------
#       str_sub()
#------------------------

# Lấy ra hai kí tự đầu tiên: 
text1 %>% str_sub(start = 1, end = 2)

# Lấy ra hai kí tự cuối: 
text1 %>% str_sub(start = str_count(text1) - 1, end = str_count(text1))

# Chú ý rằng: 
str_count("GLSCB.MVT-0005-")

# Lấy ra loại tiền tệ giao dịch: 

all_data_df %<>% mutate(loai_tien = ID %>% str_sub(start = 16, end = 18))

# Các loại tiền được giao dịch nhiều nhất: 
all_data_df %>% 
  group_by(loai_tien) %>% 
  count() %>% 
  arrange(-n)

# Lấy ra thông tin về thời điểm giao dịch: 
all_data_df %<>% mutate(thoi_diem = ID %>% str_sub(20, 27))

# Chuyển hóa tiếp về thời gian thực: 

library(lubridate)
all_data_df %<>% mutate(thoi_diem_ymd = ymd(thoi_diem))

# Thứ trong tuần của giao dịch: 

all_data_df %<>% mutate(w_ngay = wday(thoi_diem_ymd, label = TRUE, abbr = TRUE))

all_data_df %>% 
  group_by(w_ngay) %>% 
  count()


#--------------------------
#       str_detect()
#--------------------------












