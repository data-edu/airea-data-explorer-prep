library(dplyr)

# 筛选 2023 年数据
data_2023 <- ccrc_cip_comp %>% 
  filter(year == 2023)


# 2) 简单示例：只展示一级，没有再细分父节点的层次结构
#    如果有想要用 CIP 大类，需先自定义或根据 CIP 码前2位进行归类
treemap_data <- data_2023 %>%
  group_by(mfreq_green_cip_stitle1) %>%
  summarise(total_completions = sum(mfreq_green_cip_cmplt1, na.rm = TRUE)) %>%
  ungroup()

# 3) 给出父节点（此处只做单层，就统一设置为 "All Green Programs" 或其它常量）
treemap_data <- treemap_data %>%
  mutate(
    label   = mfreq_green_cip_stitle1,
    parent  = "All Green Programs",  
    value   = total_completions
  )


library(plotly)

fig <- plot_ly(
  data = treemap_data,
  type = "treemap",
  labels = ~label,
  parents = ~parent,
  values = ~value,
  # 可以显示相对于父节点的百分比
  textinfo = "label+value+percent parent",
  # 也可以设置相对于根节点的百分比: "label+value+percent root"
  branchvalues = "total"   # 或者 "remainder"
)

fig



library(dplyr)
library(plotly)

# 假设 ccrc_cip_comp 数据中字段 year 为年份
years <- sort(unique(ccrc_cip_comp$year))  # 2010:2023

# 创建一个空列表用于存放每个年份的 plotly 图形
plotly_list <- list()

for (yr in years) {
  # 筛选出当前年份的数据
  data_year <- ccrc_cip_comp %>% 
    filter(year == yr)
  
  # 按照绿色相关的专业标题进行聚合
  treemap_data <- data_year %>%
    group_by(mfreq_green_cip_stitle1) %>%
    summarise(total_completions = sum(mfreq_green_cip_cmplt1, na.rm = TRUE)) %>%
    ungroup() %>%
    # 设置单层树图，父节点统一为 "All Green Programs"
    mutate(
      label  = mfreq_green_cip_stitle1,
      parent = "All Green Programs",
      value  = total_completions
    )
  
  # 生成 plotly treemap 图形
  fig <- plot_ly(
    data = treemap_data,
    type = "treemap",
    labels = ~label,
    parents = ~parent,
    values = ~value,
    textinfo = "label+value+percent parent",
    branchvalues = "total"
  )
  
  # 将图形存入列表，列表名使用年份作为 key
  plotly_list[[as.character(yr)]] <- fig
}

# 将所有 plotly 图形列表存储成 RDS 文件
saveRDS(plotly_list, "Green_degree_treemap_plotly_list.rds")

message("RDS 文件已生成：Green_degree_treemap_plotly_list.rds")


##### top 10 + other treemap

library(dplyr)
library(plotly)

# 假设 ccrc_cip_comp 数据中字段 year 为年份
years <- sort(unique(ccrc_cip_comp$year))  # 2010:2023

# 创建一个空列表用于存放每个年份的 plotly 图形
plotly_list <- list()

for (yr in years) {
  # 筛选出当前年份的数据
  data_year <- ccrc_cip_comp %>% 
    filter(year == yr)
  
  # 按照绿色相关的专业标题进行聚合，并计算每个专业的总完成数
  aggregated_data <- data_year %>%
    group_by(mfreq_green_cip_stitle1) %>%
    summarise(total_completions = sum(mfreq_green_cip_cmplt1, na.rm = TRUE)) %>%
    ungroup() %>%
    arrange(desc(total_completions))
  
  # 如果类别超过 10 个，则只保留前 10 个，剩下的合并为 "Other"
  if(nrow(aggregated_data) > 10) {
    top10 <- aggregated_data[1:10, ]
    others <- aggregated_data[-(1:10), ]
    others_sum <- sum(others$total_completions)
    treemap_data <- bind_rows(
      top10,
      tibble(mfreq_green_cip_stitle1 = "Other", total_completions = others_sum)
    )
  } else {
    treemap_data <- aggregated_data
  }
  
  # 设置 treemap_data 的必要列
  treemap_data <- treemap_data %>%
    mutate(
      label  = mfreq_green_cip_stitle1,
      parent = "All Green Programs",
      value  = total_completions
    )
  
  # 生成 plotly treemap 图形
  fig <- plot_ly(
    data = treemap_data,
    type = "treemap",
    labels = ~label,
    parents = ~parent,
    values = ~value,
    textinfo = "label+value+percent parent",
    branchvalues = "total"
  )
  
  # 将图形存入列表，列表名使用年份作为 key
  plotly_list[[as.character(yr)]] <- fig
}

# 将所有 plotly 图形列表存储成 RDS 文件
saveRDS(plotly_list, "Green_degree_treemap_plotly_list.rds")

message("RDS 文件已生成：Green_degree_treemap_plotly_list.rds")


