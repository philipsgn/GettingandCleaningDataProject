# run_analysis.R
# Getting and Cleaning Data - Course Project
# Author: <tên bạn>
# Purpose: tạo file tidy_data.txt từ bộ dữ liệu UCI HAR Dataset

# ----------------------------
# 0. Load thư viện
# ----------------------------
if (!require("dplyr")) install.packages("dplyr")
library(dplyr)

# ----------------------------
# 1. Đọc dữ liệu gốc
# ----------------------------
data_dir <- "UCI HAR Dataset"

# Đọc tên biến (features) và nhãn hoạt động
features <- read.table(file.path(data_dir, "features.txt"), stringsAsFactors = FALSE)
feature_names <- features$V2
activity_labels <- read.table(file.path(data_dir, "activity_labels.txt"), stringsAsFactors = FALSE)
colnames(activity_labels) <- c("activity_id", "activity_label")

# Hàm tiện lợi: đọc 1 tập dữ liệu (train hoặc test)
read_set <- function(set_name) {
  subject <- read.table(file.path(data_dir, set_name, paste0("subject_", set_name, ".txt")), col.names = "subject")
  x <- read.table(file.path(data_dir, set_name, paste0("X_", set_name, ".txt")))
  y <- read.table(file.path(data_dir, set_name, paste0("y_", set_name, ".txt")), col.names = "activity_id")
  colnames(x) <- feature_names
  cbind(subject, y, x)
}

# ----------------------------
# 2. Gộp train + test
# ----------------------------
train <- read_set("train")
test  <- read_set("test")
combined <- rbind(train, test)

# ----------------------------
# 3. Lọc biến mean() và std()
# ----------------------------
sel_features <- grepl("mean\\(\\)|std\\(\\)", feature_names)
selected_cols <- c(TRUE, TRUE, sel_features)  # subject + activity_id + features chọn lọc
data_extracted <- combined[, selected_cols]

# ----------------------------
# 4. Gán tên hoạt động mô tả
# ----------------------------
data_extracted <- merge(activity_labels, data_extracted, by = "activity_id", all.y = TRUE)
data_extracted <- data_extracted %>%
  select(subject, activity_label, everything(), -activity_id)

# ----------------------------
# 5. Làm sạch tên biến
# ----------------------------
clean_names <- colnames(data_extracted)
clean_names <- gsub("\\()", "", clean_names)
clean_names <- gsub("-", "_", clean_names)
clean_names <- gsub("^t", "time_", clean_names)
clean_names <- gsub("^f", "freq_", clean_names)
clean_names <- gsub("Acc", "Accelerometer", clean_names)
clean_names <- gsub("Gyro", "Gyroscope", clean_names)
clean_names <- gsub("Mag", "Magnitude", clean_names)
clean_names <- gsub("BodyBody", "Body", clean_names)
clean_names <- gsub("mean", "Mean", clean_names)
clean_names <- gsub("std", "STD", clean_names)
colnames(data_extracted) <- clean_names

# ----------------------------
# 6. Tạo dataset tidy: trung bình theo subject + activity
# ----------------------------
tidy_data <- data_extracted %>%
  group_by(subject, activity_label) %>%
  summarise_all(mean) %>%
  ungroup()

# Đặt tên biến rõ ràng hơn: prefix avg_
names(tidy_data)[-(1:2)] <- paste0("avg_", names(tidy_data)[-(1:2)])

# ----------------------------
# 7. Xuất file tidy_data.txt
# ----------------------------
write.table(tidy_data, file = "tidy_data.txt", row.name = FALSE)

message("✅ File tidy_data.txt đã được tạo thành công trong working directory.")
