# Performance Analysis with tidyquant
library(tidyquant)
(ra <- c("000001.SZ", "000002.SZ", "000004.SZ") %>% 
  tq_get(get = "stock.prices",
         from = "2010-01-01",
         to = "2015-12-31") %>% 
  group_by(symbol) %>% 
  tq_transmute(
    select = adjusted,
    mutate_fun = periodReturn,
    period = "monthly",
    col_rename = "ra"
  ))

# 下面我们使用深证100R指数得到基准价格：
library(Tushare)
api = pro_api('fd65bf1ee56215d0ce1a95d1fa3b453d97a1779438aca26c4b7937d1')
(rb <- api(api_name = "index_daily",
    ts_code = "399004.SZ",
    start_date = "20100101",
    end_date = "20151231") %>% 
  as_tibble() %>% 
  transmute(
    date = ymd(trade_date),
    symbol = as.character(ts_code),
    close = as.numeric(close)
  ) %>% 
  tq_transmute(
    select = close,
    mutate_fun = periodReturn,
    period = "monthly",
    col_rename = "rb"
  ))

# 合并两个数据集：
(rarb <- left_join(ra, rb, by = "date"))

# 使用 tq_performance 运行 CAPM 模型
(rarb_capm <- rarb %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.CAPM
  ))

# 我们比较关心增长测度和风险测度
rarb_capm %>% 
  dplyr::select(Alpha, Beta)

# tidyquant的工作流程
# 单独资产分析
# 查看 SharpeRatio 函数的参数：
args(SharpeRatio)

# Step 1A: 获取股票数据
(stock_prices <- c("000001.SZ", "000002.SZ", "000004.SZ") %>% 
  tq_get(
    get = "stock.prices",
    from = "2010-01-01",
    to = "2015-12-31"
  ))


(stock_returns_monthly <- stock_prices %>% 
  group_by(symbol) %>% 
  tq_transmute(
    select = adjusted,
    mutate_fun = periodReturn,
    period = "monthly",
    col_rename = "ra"
  ))

# Step4: 绩效分析
stock_returns_monthly %>% 
  tq_performance(Ra = ra,
                 Rb = NULL,
                 performance_fun = SharpeRatio)

# 修改无风险利率和置信水平
stock_returns_monthly %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = SharpeRatio,
    Rf = 0.03 / 12,
    p = 0.99
  )

# 资产组合的分析
# 组合资产
# 方法1: 使用权重向量
wts <- c(0.5, 0, 0.5)
(portfolio_returns_monthly <- stock_returns_monthly %>% 
  tq_portfolio(
    assets_col = symbol,
    returns_col = ra,
    weights = wts,
    col_rename = "ra"
  ))

# 方法2: 使用资产 + 权重 数据框
(wts_map <- tibble(
  symbols = c('000001.SZ', '000004.SZ'),
  weights = c(0.5, 0.5)
))

stock_returns_monthly %>% 
  tq_portfolio(
    assets_col = symbol,
    returns_col = ra,
    weights = wts_map,
    col_rename = "ra_using_wts_map"
  )

# 合并ra和rb
(rarb_single_portfolio <- left_join(portfolio_returns_monthly,
                                   rb,
                                   by = "date"))
# 计算 CAPM 模型
rarb_single_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.CAPM
  )

# 多个资产组合
(stock_returns_monthly_multi <- stock_returns_monthly %>% 
  tq_repeat_df(n = 3))
weights <- c(
  0.5, 0.25, 0.25,
  0.25, 0.5, 0.25,
  0.25, 0.25, 0.5
)
stocks = c('000001.SZ', '000002.SZ', '000004.SZ')
(weights_table <- tibble(stocks) %>% 
  tq_repeat_df(3) %>% 
  bind_cols(tibble(weights)) %>% 
  group_by(portfolio))
(portfolio_returns_monthly_multi <- stock_returns_monthly_multi %>% 
  tq_portfolio(assets_col = symbol,
               returns_col = ra,
               weights = weights_table,
               col_rename = 'ra'))
(rarb_multi_portfolio <- left_join(
  portfolio_returns_monthly_multi, rb,
  by = "date"
))

rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.CAPM
  )

rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = SharpeRatio
  )

# 查看所有可以使用的函数
tq_performance_fun_options()

# table.Stats
# 返回基本统计量
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = table.Stats
  )

# table.CAPM
# 返回与CAPM相关的度量
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.CAPM
  )

# table.AnnualizedReturns
# 返回年度回报、标准差和夏普比率
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = table.AnnualizedReturns
  )

# table.Correlation
# 返回与基准收益的回报率
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.Correlation
  )

# table.DownsideRisk
# 返回下行风险表，以便在多个工具或基金之间进行比较
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.DownsideRisk
  )

# table.DownsideRiskRatio
# 返回每月下行风险、年度下行风险等指标
rarb_multi_portfolio %>% 
  tq_performance(Ra = ra,
                 Rb = NULL,
                 performance_fun = table.DownsideRiskRatio)

# table.HigherMoments
# 返回分布的高阶矩和协矩
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.HigherMoments
  )

# table.InformationRatio
# 返回追踪误差表、年度追踪误差和信息比率
rarb_multi_portfolio %>% 
  tq_performance(Ra = ra,
                 Rb = rb,
                 performance_fun = table.InformationRatio)
# table.Variability
# 返回平均绝对差异、月标准差和年标准差
rarb_multi_portfolio %>% 
  tq_performance(Ra = ra,
                 Rb = NULL,
                 performance_fun = table.Variability)

# 在险值
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = VaR
  )

# 夏普比率
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = SharpeRatio
  )

# 自定义tq_portfolio
portfolio_returns_monthly %>% 
  ggplot(aes(x = date, y = ra)) + 
  geom_bar(stat = "identity", fill = palette_light()[[3]],
           width = 20) + 
  labs(title = "Portfolio Returns",
       caption = "Shows an above-zero trend meaning positive returns",
       x = "", y = "Monthly Returns") + 
  geom_smooth(method = "lm") + 
  theme_tq(base_family = enfont) + 
  scale_color_tq() + 
  scale_y_continuous(labels = scales::percent)

ggsave("tidyquantperformance1.svg")

# 使用 wealth.index = TRUE 参数，我们可以观察10000元是如何增长的：
wts <- c(0.5, 0, 0.5)
portfolio_growth_monthly <- stock_returns_monthly %>% 
  tq_portfolio(
    assets_col = symbol,
    returns_col = ra,
    weights = wts,
    col_rename = "investment.growth",
    wealth.index = TRUE
  ) %>% 
  mutate(investment.growth = investment.growth * 10000)
portfolio_growth_monthly %>% 
  ggplot(aes(x = date, y = investment.growth)) + 
  geom_line(size = 1, color = palette_light()[3]) +
  labs(x = "", y = "Portfolio Value",
       title = "Portfolio Growth",
       caption = "Now we can really visualize performance!") + 
  geom_smooth(method = "loess") + 
  theme_tq(base_family = enfont) + 
  scale_color_tq() + 
  scale_y_continuous(labels = scales::dollar)

ggsave("tidyquantperformance2.svg")

# 下面再比较三个投资组合的增长情况
portfolio_growth_monthly_multi <- stock_returns_monthly_multi %>% 
  tq_portfolio(
    assets_col = symbol,
    returns_col = ra,
    weights = weights_table,
    col_rename = "investment.growth",
    wealth.index = TRUE
  ) %>% 
  mutate(investment.growth = investment.growth * 10000)
portfolio_growth_monthly_multi %>% 
  ggplot(aes(x = date, y = investment.growth, color = factor(portfolio))) + 
  geom_line(size = 2) + 
  labs(title = "Portfolio Growth",
       subtitle = "Comparing Multiple Portfolios",
       caption = "Portfolio 3 is a Standout!",
       x = "", 
       y = "Portfolio Value",
       color = "Portfolio") + 
  geom_smooth(method = "loess") + 
  theme_tq(base_family = enfont) + 
  scale_color_tq() +
  scale_y_continuous(labels = scales::dollar)

ggsave("tidyquantperformance3.svg")
# Performance Analysis with tidyquant
library(tidyquant)
(ra <- c("000001.SZ", "000002.SZ", "000004.SZ") %>% 
  tq_get(get = "stock.prices",
         from = "2010-01-01",
         to = "2015-12-31") %>% 
  group_by(symbol) %>% 
  tq_transmute(
    select = adjusted,
    mutate_fun = periodReturn,
    period = "monthly",
    col_rename = "ra"
  ))

# 下面我们使用深证100R指数得到基准价格：
library(Tushare)
api = pro_api('fd65bf1ee56215d0ce1a95d1fa3b453d97a1779438aca26c4b7937d1')
(rb <- api(api_name = "index_daily",
    ts_code = "399004.SZ",
    start_date = "20100101",
    end_date = "20151231") %>% 
  as_tibble() %>% 
  transmute(
    date = ymd(trade_date),
    symbol = as.character(ts_code),
    close = as.numeric(close)
  ) %>% 
  tq_transmute(
    select = close,
    mutate_fun = periodReturn,
    period = "monthly",
    col_rename = "rb"
  ))

# 合并两个数据集：
(rarb <- left_join(ra, rb, by = "date"))

# 使用 tq_performance 运行 CAPM 模型
(rarb_capm <- rarb %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.CAPM
  ))

# 我们比较关心增长测度和风险测度
rarb_capm %>% 
  dplyr::select(Alpha, Beta)

# tidyquant的工作流程
# 单独资产分析
# 查看 SharpeRatio 函数的参数：
args(SharpeRatio)

# Step 1A: 获取股票数据
(stock_prices <- c("000001.SZ", "000002.SZ", "000004.SZ") %>% 
  tq_get(
    get = "stock.prices",
    from = "2010-01-01",
    to = "2015-12-31"
  ))


(stock_returns_monthly <- stock_prices %>% 
  group_by(symbol) %>% 
  tq_transmute(
    select = adjusted,
    mutate_fun = periodReturn,
    period = "monthly",
    col_rename = "ra"
  ))

# Step4: 绩效分析
stock_returns_monthly %>% 
  tq_performance(Ra = ra,
                 Rb = NULL,
                 performance_fun = SharpeRatio)

# 修改无风险利率和置信水平
stock_returns_monthly %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = SharpeRatio,
    Rf = 0.03 / 12,
    p = 0.99
  )

# 资产组合的分析
# 组合资产
# 方法1: 使用权重向量
wts <- c(0.5, 0, 0.5)
(portfolio_returns_monthly <- stock_returns_monthly %>% 
  tq_portfolio(
    assets_col = symbol,
    returns_col = ra,
    weights = wts,
    col_rename = "ra"
  ))

# 方法2: 使用资产 + 权重 数据框
(wts_map <- tibble(
  symbols = c('000001.SZ', '000004.SZ'),
  weights = c(0.5, 0.5)
))

stock_returns_monthly %>% 
  tq_portfolio(
    assets_col = symbol,
    returns_col = ra,
    weights = wts_map,
    col_rename = "ra_using_wts_map"
  )

# 合并ra和rb
(rarb_single_portfolio <- left_join(portfolio_returns_monthly,
                                   rb,
                                   by = "date"))
# 计算 CAPM 模型
rarb_single_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.CAPM
  )

# 多个资产组合
(stock_returns_monthly_multi <- stock_returns_monthly %>% 
  tq_repeat_df(n = 3))
weights <- c(
  0.5, 0.25, 0.25,
  0.25, 0.5, 0.25,
  0.25, 0.25, 0.5
)
stocks = c('000001.SZ', '000002.SZ', '000004.SZ')
(weights_table <- tibble(stocks) %>% 
  tq_repeat_df(3) %>% 
  bind_cols(tibble(weights)) %>% 
  group_by(portfolio))
(portfolio_returns_monthly_multi <- stock_returns_monthly_multi %>% 
  tq_portfolio(assets_col = symbol,
               returns_col = ra,
               weights = weights_table,
               col_rename = 'ra'))
(rarb_multi_portfolio <- left_join(
  portfolio_returns_monthly_multi, rb,
  by = "date"
))

rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.CAPM
  )

rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = SharpeRatio
  )

# 查看所有可以使用的函数
tq_performance_fun_options()

# table.Stats
# 返回基本统计量
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = table.Stats
  )

# table.CAPM
# 返回与CAPM相关的度量
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.CAPM
  )

# table.AnnualizedReturns
# 返回年度回报、标准差和夏普比率
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = table.AnnualizedReturns
  )

# table.Correlation
# 返回与基准收益的回报率
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.Correlation
  )

# table.DownsideRisk
# 返回下行风险表，以便在多个工具或基金之间进行比较
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.DownsideRisk
  )

# table.DownsideRiskRatio
# 返回每月下行风险、年度下行风险等指标
rarb_multi_portfolio %>% 
  tq_performance(Ra = ra,
                 Rb = NULL,
                 performance_fun = table.DownsideRiskRatio)

# table.HigherMoments
# 返回分布的高阶矩和协矩
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.HigherMoments
  )

# table.InformationRatio
# 返回追踪误差表、年度追踪误差和信息比率
rarb_multi_portfolio %>% 
  tq_performance(Ra = ra,
                 Rb = rb,
                 performance_fun = table.InformationRatio)
# table.Variability
# 返回平均绝对差异、月标准差和年标准差
rarb_multi_portfolio %>% 
  tq_performance(Ra = ra,
                 Rb = NULL,
                 performance_fun = table.Variability)

# 在险值
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = VaR
  )

# 夏普比率
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = SharpeRatio
  )

# 自定义tq_portfolio
portfolio_returns_monthly %>% 
  ggplot(aes(x = date, y = ra)) + 
  geom_bar(stat = "identity", fill = palette_light()[[3]],
           width = 20) + 
  labs(title = "Portfolio Returns",
       caption = "Shows an above-zero trend meaning positive returns",
       x = "", y = "Monthly Returns") + 
  geom_smooth(method = "lm") + 
  theme_tq(base_family = enfont) + 
  scale_color_tq() + 
  scale_y_continuous(labels = scales::percent)

ggsave("tidyquantperformance1.svg")

# 使用 wealth.index = TRUE 参数，我们可以观察10000元是如何增长的：
wts <- c(0.5, 0, 0.5)
portfolio_growth_monthly <- stock_returns_monthly %>% 
  tq_portfolio(
    assets_col = symbol,
    returns_col = ra,
    weights = wts,
    col_rename = "investment.growth",
    wealth.index = TRUE
  ) %>% 
  mutate(investment.growth = investment.growth * 10000)
portfolio_growth_monthly %>% 
  ggplot(aes(x = date, y = investment.growth)) + 
  geom_line(size = 1, color = palette_light()[3]) +
  labs(x = "", y = "Portfolio Value",
       title = "Portfolio Growth",
       caption = "Now we can really visualize performance!") + 
  geom_smooth(method = "loess") + 
  theme_tq(base_family = enfont) + 
  scale_color_tq() + 
  scale_y_continuous(labels = scales::dollar)

ggsave("tidyquantperformance2.svg")

# 下面再比较三个投资组合的增长情况
portfolio_growth_monthly_multi <- stock_returns_monthly_multi %>% 
  tq_portfolio(
    assets_col = symbol,
    returns_col = ra,
    weights = weights_table,
    col_rename = "investment.growth",
    wealth.index = TRUE
  ) %>% 
  mutate(investment.growth = investment.growth * 10000)
portfolio_growth_monthly_multi %>% 
  ggplot(aes(x = date, y = investment.growth, color = factor(portfolio))) + 
  geom_line(size = 2) + 
  labs(title = "Portfolio Growth",
       subtitle = "Comparing Multiple Portfolios",
       caption = "Portfolio 3 is a Standout!",
       x = "", 
       y = "Portfolio Value",
       color = "Portfolio") + 
  geom_smooth(method = "loess") + 
  theme_tq(base_family = enfont) + 
  scale_color_tq() +
  scale_y_continuous(labels = scales::dollar)

ggsave("tidyquantperformance3.svg")
# Performance Analysis with tidyquant
library(tidyquant)
(ra <- c("000001.SZ", "000002.SZ", "000004.SZ") %>% 
  tq_get(get = "stock.prices",
         from = "2010-01-01",
         to = "2015-12-31") %>% 
  group_by(symbol) %>% 
  tq_transmute(
    select = adjusted,
    mutate_fun = periodReturn,
    period = "monthly",
    col_rename = "ra"
  ))

# 下面我们使用深证100R指数得到基准价格：
library(Tushare)
api = pro_api('fd65bf1ee56215d0ce1a95d1fa3b453d97a1779438aca26c4b7937d1')
(rb <- api(api_name = "index_daily",
    ts_code = "399004.SZ",
    start_date = "20100101",
    end_date = "20151231") %>% 
  as_tibble() %>% 
  transmute(
    date = ymd(trade_date),
    symbol = as.character(ts_code),
    close = as.numeric(close)
  ) %>% 
  tq_transmute(
    select = close,
    mutate_fun = periodReturn,
    period = "monthly",
    col_rename = "rb"
  ))

# 合并两个数据集：
(rarb <- left_join(ra, rb, by = "date"))

# 使用 tq_performance 运行 CAPM 模型
(rarb_capm <- rarb %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.CAPM
  ))

# 我们比较关心增长测度和风险测度
rarb_capm %>% 
  dplyr::select(Alpha, Beta)

# tidyquant的工作流程
# 单独资产分析
# 查看 SharpeRatio 函数的参数：
args(SharpeRatio)

# Step 1A: 获取股票数据
(stock_prices <- c("000001.SZ", "000002.SZ", "000004.SZ") %>% 
  tq_get(
    get = "stock.prices",
    from = "2010-01-01",
    to = "2015-12-31"
  ))


(stock_returns_monthly <- stock_prices %>% 
  group_by(symbol) %>% 
  tq_transmute(
    select = adjusted,
    mutate_fun = periodReturn,
    period = "monthly",
    col_rename = "ra"
  ))

# Step4: 绩效分析
stock_returns_monthly %>% 
  tq_performance(Ra = ra,
                 Rb = NULL,
                 performance_fun = SharpeRatio)

# 修改无风险利率和置信水平
stock_returns_monthly %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = SharpeRatio,
    Rf = 0.03 / 12,
    p = 0.99
  )

# 资产组合的分析
# 组合资产
# 方法1: 使用权重向量
wts <- c(0.5, 0, 0.5)
(portfolio_returns_monthly <- stock_returns_monthly %>% 
  tq_portfolio(
    assets_col = symbol,
    returns_col = ra,
    weights = wts,
    col_rename = "ra"
  ))

# 方法2: 使用资产 + 权重 数据框
(wts_map <- tibble(
  symbols = c('000001.SZ', '000004.SZ'),
  weights = c(0.5, 0.5)
))

stock_returns_monthly %>% 
  tq_portfolio(
    assets_col = symbol,
    returns_col = ra,
    weights = wts_map,
    col_rename = "ra_using_wts_map"
  )

# 合并ra和rb
(rarb_single_portfolio <- left_join(portfolio_returns_monthly,
                                   rb,
                                   by = "date"))
# 计算 CAPM 模型
rarb_single_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.CAPM
  )

# 多个资产组合
(stock_returns_monthly_multi <- stock_returns_monthly %>% 
  tq_repeat_df(n = 3))
weights <- c(
  0.5, 0.25, 0.25,
  0.25, 0.5, 0.25,
  0.25, 0.25, 0.5
)
stocks = c('000001.SZ', '000002.SZ', '000004.SZ')
(weights_table <- tibble(stocks) %>% 
  tq_repeat_df(3) %>% 
  bind_cols(tibble(weights)) %>% 
  group_by(portfolio))
(portfolio_returns_monthly_multi <- stock_returns_monthly_multi %>% 
  tq_portfolio(assets_col = symbol,
               returns_col = ra,
               weights = weights_table,
               col_rename = 'ra'))
(rarb_multi_portfolio <- left_join(
  portfolio_returns_monthly_multi, rb,
  by = "date"
))

rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.CAPM
  )

rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = SharpeRatio
  )

# 查看所有可以使用的函数
tq_performance_fun_options()

# table.Stats
# 返回基本统计量
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = table.Stats
  )

# table.CAPM
# 返回与CAPM相关的度量
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.CAPM
  )

# table.AnnualizedReturns
# 返回年度回报、标准差和夏普比率
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = table.AnnualizedReturns
  )

# table.Correlation
# 返回与基准收益的回报率
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.Correlation
  )

# table.DownsideRisk
# 返回下行风险表，以便在多个工具或基金之间进行比较
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.DownsideRisk
  )

# table.DownsideRiskRatio
# 返回每月下行风险、年度下行风险等指标
rarb_multi_portfolio %>% 
  tq_performance(Ra = ra,
                 Rb = NULL,
                 performance_fun = table.DownsideRiskRatio)

# table.HigherMoments
# 返回分布的高阶矩和协矩
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.HigherMoments
  )

# table.InformationRatio
# 返回追踪误差表、年度追踪误差和信息比率
rarb_multi_portfolio %>% 
  tq_performance(Ra = ra,
                 Rb = rb,
                 performance_fun = table.InformationRatio)
# table.Variability
# 返回平均绝对差异、月标准差和年标准差
rarb_multi_portfolio %>% 
  tq_performance(Ra = ra,
                 Rb = NULL,
                 performance_fun = table.Variability)

# 在险值
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = VaR
  )

# 夏普比率
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = SharpeRatio
  )

# 自定义tq_portfolio
portfolio_returns_monthly %>% 
  ggplot(aes(x = date, y = ra)) + 
  geom_bar(stat = "identity", fill = palette_light()[[3]],
           width = 20) + 
  labs(title = "Portfolio Returns",
       caption = "Shows an above-zero trend meaning positive returns",
       x = "", y = "Monthly Returns") + 
  geom_smooth(method = "lm") + 
  theme_tq(base_family = enfont) + 
  scale_color_tq() + 
  scale_y_continuous(labels = scales::percent)

ggsave("tidyquantperformance1.svg")

# 使用 wealth.index = TRUE 参数，我们可以观察10000元是如何增长的：
wts <- c(0.5, 0, 0.5)
portfolio_growth_monthly <- stock_returns_monthly %>% 
  tq_portfolio(
    assets_col = symbol,
    returns_col = ra,
    weights = wts,
    col_rename = "investment.growth",
    wealth.index = TRUE
  ) %>% 
  mutate(investment.growth = investment.growth * 10000)
portfolio_growth_monthly %>% 
  ggplot(aes(x = date, y = investment.growth)) + 
  geom_line(size = 1, color = palette_light()[3]) +
  labs(x = "", y = "Portfolio Value",
       title = "Portfolio Growth",
       caption = "Now we can really visualize performance!") + 
  geom_smooth(method = "loess") + 
  theme_tq(base_family = enfont) + 
  scale_color_tq() + 
  scale_y_continuous(labels = scales::dollar)

ggsave("tidyquantperformance2.svg")

# 下面再比较三个投资组合的增长情况
portfolio_growth_monthly_multi <- stock_returns_monthly_multi %>% 
  tq_portfolio(
    assets_col = symbol,
    returns_col = ra,
    weights = weights_table,
    col_rename = "investment.growth",
    wealth.index = TRUE
  ) %>% 
  mutate(investment.growth = investment.growth * 10000)
portfolio_growth_monthly_multi %>% 
  ggplot(aes(x = date, y = investment.growth, color = factor(portfolio))) + 
  geom_line(size = 2) + 
  labs(title = "Portfolio Growth",
       subtitle = "Comparing Multiple Portfolios",
       caption = "Portfolio 3 is a Standout!",
       x = "", 
       y = "Portfolio Value",
       color = "Portfolio") + 
  geom_smooth(method = "loess") + 
  theme_tq(base_family = enfont) + 
  scale_color_tq() +
  scale_y_continuous(labels = scales::dollar)

ggsave("tidyquantperformance3.svg")
# Performance Analysis with tidyquant
library(tidyquant)
(ra <- c("000001.SZ", "000002.SZ", "000004.SZ") %>% 
  tq_get(get = "stock.prices",
         from = "2010-01-01",
         to = "2015-12-31") %>% 
  group_by(symbol) %>% 
  tq_transmute(
    select = adjusted,
    mutate_fun = periodReturn,
    period = "monthly",
    col_rename = "ra"
  ))

# 下面我们使用深证100R指数得到基准价格：
library(Tushare)
api = pro_api('fd65bf1ee56215d0ce1a95d1fa3b453d97a1779438aca26c4b7937d1')
(rb <- api(api_name = "index_daily",
    ts_code = "399004.SZ",
    start_date = "20100101",
    end_date = "20151231") %>% 
  as_tibble() %>% 
  transmute(
    date = ymd(trade_date),
    symbol = as.character(ts_code),
    close = as.numeric(close)
  ) %>% 
  tq_transmute(
    select = close,
    mutate_fun = periodReturn,
    period = "monthly",
    col_rename = "rb"
  ))

# 合并两个数据集：
(rarb <- left_join(ra, rb, by = "date"))

# 使用 tq_performance 运行 CAPM 模型
(rarb_capm <- rarb %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.CAPM
  ))

# 我们比较关心增长测度和风险测度
rarb_capm %>% 
  dplyr::select(Alpha, Beta)

# tidyquant的工作流程
# 单独资产分析
# 查看 SharpeRatio 函数的参数：
args(SharpeRatio)

# Step 1A: 获取股票数据
(stock_prices <- c("000001.SZ", "000002.SZ", "000004.SZ") %>% 
  tq_get(
    get = "stock.prices",
    from = "2010-01-01",
    to = "2015-12-31"
  ))


(stock_returns_monthly <- stock_prices %>% 
  group_by(symbol) %>% 
  tq_transmute(
    select = adjusted,
    mutate_fun = periodReturn,
    period = "monthly",
    col_rename = "ra"
  ))

# Step4: 绩效分析
stock_returns_monthly %>% 
  tq_performance(Ra = ra,
                 Rb = NULL,
                 performance_fun = SharpeRatio)

# 修改无风险利率和置信水平
stock_returns_monthly %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = SharpeRatio,
    Rf = 0.03 / 12,
    p = 0.99
  )

# 资产组合的分析
# 组合资产
# 方法1: 使用权重向量
wts <- c(0.5, 0, 0.5)
(portfolio_returns_monthly <- stock_returns_monthly %>% 
  tq_portfolio(
    assets_col = symbol,
    returns_col = ra,
    weights = wts,
    col_rename = "ra"
  ))

# 方法2: 使用资产 + 权重 数据框
(wts_map <- tibble(
  symbols = c('000001.SZ', '000004.SZ'),
  weights = c(0.5, 0.5)
))

stock_returns_monthly %>% 
  tq_portfolio(
    assets_col = symbol,
    returns_col = ra,
    weights = wts_map,
    col_rename = "ra_using_wts_map"
  )

# 合并ra和rb
(rarb_single_portfolio <- left_join(portfolio_returns_monthly,
                                   rb,
                                   by = "date"))
# 计算 CAPM 模型
rarb_single_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.CAPM
  )

# 多个资产组合
(stock_returns_monthly_multi <- stock_returns_monthly %>% 
  tq_repeat_df(n = 3))
weights <- c(
  0.5, 0.25, 0.25,
  0.25, 0.5, 0.25,
  0.25, 0.25, 0.5
)
stocks = c('000001.SZ', '000002.SZ', '000004.SZ')
(weights_table <- tibble(stocks) %>% 
  tq_repeat_df(3) %>% 
  bind_cols(tibble(weights)) %>% 
  group_by(portfolio))
(portfolio_returns_monthly_multi <- stock_returns_monthly_multi %>% 
  tq_portfolio(assets_col = symbol,
               returns_col = ra,
               weights = weights_table,
               col_rename = 'ra'))
(rarb_multi_portfolio <- left_join(
  portfolio_returns_monthly_multi, rb,
  by = "date"
))

rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.CAPM
  )

rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = SharpeRatio
  )

# 查看所有可以使用的函数
tq_performance_fun_options()

# table.Stats
# 返回基本统计量
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = table.Stats
  )

# table.CAPM
# 返回与CAPM相关的度量
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.CAPM
  )

# table.AnnualizedReturns
# 返回年度回报、标准差和夏普比率
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = table.AnnualizedReturns
  )

# table.Correlation
# 返回与基准收益的回报率
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.Correlation
  )

# table.DownsideRisk
# 返回下行风险表，以便在多个工具或基金之间进行比较
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.DownsideRisk
  )

# table.DownsideRiskRatio
# 返回每月下行风险、年度下行风险等指标
rarb_multi_portfolio %>% 
  tq_performance(Ra = ra,
                 Rb = NULL,
                 performance_fun = table.DownsideRiskRatio)

# table.HigherMoments
# 返回分布的高阶矩和协矩
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.HigherMoments
  )

# table.InformationRatio
# 返回追踪误差表、年度追踪误差和信息比率
rarb_multi_portfolio %>% 
  tq_performance(Ra = ra,
                 Rb = rb,
                 performance_fun = table.InformationRatio)
# table.Variability
# 返回平均绝对差异、月标准差和年标准差
rarb_multi_portfolio %>% 
  tq_performance(Ra = ra,
                 Rb = NULL,
                 performance_fun = table.Variability)

# 在险值
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = VaR
  )

# 夏普比率
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = SharpeRatio
  )

# 自定义tq_portfolio
portfolio_returns_monthly %>% 
  ggplot(aes(x = date, y = ra)) + 
  geom_bar(stat = "identity", fill = palette_light()[[3]],
           width = 20) + 
  labs(title = "Portfolio Returns",
       caption = "Shows an above-zero trend meaning positive returns",
       x = "", y = "Monthly Returns") + 
  geom_smooth(method = "lm") + 
  theme_tq(base_family = enfont) + 
  scale_color_tq() + 
  scale_y_continuous(labels = scales::percent)

ggsave("tidyquantperformance1.svg")

# 使用 wealth.index = TRUE 参数，我们可以观察10000元是如何增长的：
wts <- c(0.5, 0, 0.5)
portfolio_growth_monthly <- stock_returns_monthly %>% 
  tq_portfolio(
    assets_col = symbol,
    returns_col = ra,
    weights = wts,
    col_rename = "investment.growth",
    wealth.index = TRUE
  ) %>% 
  mutate(investment.growth = investment.growth * 10000)
portfolio_growth_monthly %>% 
  ggplot(aes(x = date, y = investment.growth)) + 
  geom_line(size = 1, color = palette_light()[3]) +
  labs(x = "", y = "Portfolio Value",
       title = "Portfolio Growth",
       caption = "Now we can really visualize performance!") + 
  geom_smooth(method = "loess") + 
  theme_tq(base_family = enfont) + 
  scale_color_tq() + 
  scale_y_continuous(labels = scales::dollar)

ggsave("tidyquantperformance2.svg")

# 下面再比较三个投资组合的增长情况
portfolio_growth_monthly_multi <- stock_returns_monthly_multi %>% 
  tq_portfolio(
    assets_col = symbol,
    returns_col = ra,
    weights = weights_table,
    col_rename = "investment.growth",
    wealth.index = TRUE
  ) %>% 
  mutate(investment.growth = investment.growth * 10000)
portfolio_growth_monthly_multi %>% 
  ggplot(aes(x = date, y = investment.growth, color = factor(portfolio))) + 
  geom_line(size = 2) + 
  labs(title = "Portfolio Growth",
       subtitle = "Comparing Multiple Portfolios",
       caption = "Portfolio 3 is a Standout!",
       x = "", 
       y = "Portfolio Value",
       color = "Portfolio") + 
  geom_smooth(method = "loess") + 
  theme_tq(base_family = enfont) + 
  scale_color_tq() +
  scale_y_continuous(labels = scales::dollar)

ggsave("tidyquantperformance3.svg")
# Performance Analysis with tidyquant
library(tidyquant)
(ra <- c("000001.SZ", "000002.SZ", "000004.SZ") %>% 
  tq_get(get = "stock.prices",
         from = "2010-01-01",
         to = "2015-12-31") %>% 
  group_by(symbol) %>% 
  tq_transmute(
    select = adjusted,
    mutate_fun = periodReturn,
    period = "monthly",
    col_rename = "ra"
  ))

# 下面我们使用深证100R指数得到基准价格：
library(Tushare)
api = pro_api('fd65bf1ee56215d0ce1a95d1fa3b453d97a1779438aca26c4b7937d1')
(rb <- api(api_name = "index_daily",
    ts_code = "399004.SZ",
    start_date = "20100101",
    end_date = "20151231") %>% 
  as_tibble() %>% 
  transmute(
    date = ymd(trade_date),
    symbol = as.character(ts_code),
    close = as.numeric(close)
  ) %>% 
  tq_transmute(
    select = close,
    mutate_fun = periodReturn,
    period = "monthly",
    col_rename = "rb"
  ))

# 合并两个数据集：
(rarb <- left_join(ra, rb, by = "date"))

# 使用 tq_performance 运行 CAPM 模型
(rarb_capm <- rarb %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.CAPM
  ))

# 我们比较关心增长测度和风险测度
rarb_capm %>% 
  dplyr::select(Alpha, Beta)

# tidyquant的工作流程
# 单独资产分析
# 查看 SharpeRatio 函数的参数：
args(SharpeRatio)

# Step 1A: 获取股票数据
(stock_prices <- c("000001.SZ", "000002.SZ", "000004.SZ") %>% 
  tq_get(
    get = "stock.prices",
    from = "2010-01-01",
    to = "2015-12-31"
  ))


(stock_returns_monthly <- stock_prices %>% 
  group_by(symbol) %>% 
  tq_transmute(
    select = adjusted,
    mutate_fun = periodReturn,
    period = "monthly",
    col_rename = "ra"
  ))

# Step4: 绩效分析
stock_returns_monthly %>% 
  tq_performance(Ra = ra,
                 Rb = NULL,
                 performance_fun = SharpeRatio)

# 修改无风险利率和置信水平
stock_returns_monthly %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = SharpeRatio,
    Rf = 0.03 / 12,
    p = 0.99
  )

# 资产组合的分析
# 组合资产
# 方法1: 使用权重向量
wts <- c(0.5, 0, 0.5)
(portfolio_returns_monthly <- stock_returns_monthly %>% 
  tq_portfolio(
    assets_col = symbol,
    returns_col = ra,
    weights = wts,
    col_rename = "ra"
  ))

# 方法2: 使用资产 + 权重 数据框
(wts_map <- tibble(
  symbols = c('000001.SZ', '000004.SZ'),
  weights = c(0.5, 0.5)
))

stock_returns_monthly %>% 
  tq_portfolio(
    assets_col = symbol,
    returns_col = ra,
    weights = wts_map,
    col_rename = "ra_using_wts_map"
  )

# 合并ra和rb
(rarb_single_portfolio <- left_join(portfolio_returns_monthly,
                                   rb,
                                   by = "date"))
# 计算 CAPM 模型
rarb_single_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.CAPM
  )

# 多个资产组合
(stock_returns_monthly_multi <- stock_returns_monthly %>% 
  tq_repeat_df(n = 3))
weights <- c(
  0.5, 0.25, 0.25,
  0.25, 0.5, 0.25,
  0.25, 0.25, 0.5
)
stocks = c('000001.SZ', '000002.SZ', '000004.SZ')
(weights_table <- tibble(stocks) %>% 
  tq_repeat_df(3) %>% 
  bind_cols(tibble(weights)) %>% 
  group_by(portfolio))
(portfolio_returns_monthly_multi <- stock_returns_monthly_multi %>% 
  tq_portfolio(assets_col = symbol,
               returns_col = ra,
               weights = weights_table,
               col_rename = 'ra'))
(rarb_multi_portfolio <- left_join(
  portfolio_returns_monthly_multi, rb,
  by = "date"
))

rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.CAPM
  )

rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = SharpeRatio
  )

# 查看所有可以使用的函数
tq_performance_fun_options()

# table.Stats
# 返回基本统计量
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = table.Stats
  )

# table.CAPM
# 返回与CAPM相关的度量
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.CAPM
  )

# table.AnnualizedReturns
# 返回年度回报、标准差和夏普比率
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = table.AnnualizedReturns
  )

# table.Correlation
# 返回与基准收益的回报率
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.Correlation
  )

# table.DownsideRisk
# 返回下行风险表，以便在多个工具或基金之间进行比较
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.DownsideRisk
  )

# table.DownsideRiskRatio
# 返回每月下行风险、年度下行风险等指标
rarb_multi_portfolio %>% 
  tq_performance(Ra = ra,
                 Rb = NULL,
                 performance_fun = table.DownsideRiskRatio)

# table.HigherMoments
# 返回分布的高阶矩和协矩
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.HigherMoments
  )

# table.InformationRatio
# 返回追踪误差表、年度追踪误差和信息比率
rarb_multi_portfolio %>% 
  tq_performance(Ra = ra,
                 Rb = rb,
                 performance_fun = table.InformationRatio)
# table.Variability
# 返回平均绝对差异、月标准差和年标准差
rarb_multi_portfolio %>% 
  tq_performance(Ra = ra,
                 Rb = NULL,
                 performance_fun = table.Variability)

# 在险值
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = VaR
  )

# 夏普比率
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = SharpeRatio
  )

# 自定义tq_portfolio
portfolio_returns_monthly %>% 
  ggplot(aes(x = date, y = ra)) + 
  geom_bar(stat = "identity", fill = palette_light()[[3]],
           width = 20) + 
  labs(title = "Portfolio Returns",
       caption = "Shows an above-zero trend meaning positive returns",
       x = "", y = "Monthly Returns") + 
  geom_smooth(method = "lm") + 
  theme_tq(base_family = enfont) + 
  scale_color_tq() + 
  scale_y_continuous(labels = scales::percent)

ggsave("tidyquantperformance1.svg")

# 使用 wealth.index = TRUE 参数，我们可以观察10000元是如何增长的：
wts <- c(0.5, 0, 0.5)
portfolio_growth_monthly <- stock_returns_monthly %>% 
  tq_portfolio(
    assets_col = symbol,
    returns_col = ra,
    weights = wts,
    col_rename = "investment.growth",
    wealth.index = TRUE
  ) %>% 
  mutate(investment.growth = investment.growth * 10000)
portfolio_growth_monthly %>% 
  ggplot(aes(x = date, y = investment.growth)) + 
  geom_line(size = 1, color = palette_light()[3]) +
  labs(x = "", y = "Portfolio Value",
       title = "Portfolio Growth",
       caption = "Now we can really visualize performance!") + 
  geom_smooth(method = "loess") + 
  theme_tq(base_family = enfont) + 
  scale_color_tq() + 
  scale_y_continuous(labels = scales::dollar)

ggsave("tidyquantperformance2.svg")

# 下面再比较三个投资组合的增长情况
portfolio_growth_monthly_multi <- stock_returns_monthly_multi %>% 
  tq_portfolio(
    assets_col = symbol,
    returns_col = ra,
    weights = weights_table,
    col_rename = "investment.growth",
    wealth.index = TRUE
  ) %>% 
  mutate(investment.growth = investment.growth * 10000)
portfolio_growth_monthly_multi %>% 
  ggplot(aes(x = date, y = investment.growth, color = factor(portfolio))) + 
  geom_line(size = 2) + 
  labs(title = "Portfolio Growth",
       subtitle = "Comparing Multiple Portfolios",
       caption = "Portfolio 3 is a Standout!",
       x = "", 
       y = "Portfolio Value",
       color = "Portfolio") + 
  geom_smooth(method = "loess") + 
  theme_tq(base_family = enfont) + 
  scale_color_tq() +
  scale_y_continuous(labels = scales::dollar)

ggsave("tidyquantperformance3.svg")
# Performance Analysis with tidyquant
library(tidyquant)
(ra <- c("000001.SZ", "000002.SZ", "000004.SZ") %>% 
  tq_get(get = "stock.prices",
         from = "2010-01-01",
         to = "2015-12-31") %>% 
  group_by(symbol) %>% 
  tq_transmute(
    select = adjusted,
    mutate_fun = periodReturn,
    period = "monthly",
    col_rename = "ra"
  ))

# 下面我们使用深证100R指数得到基准价格：
library(Tushare)
api = pro_api('fd65bf1ee56215d0ce1a95d1fa3b453d97a1779438aca26c4b7937d1')
(rb <- api(api_name = "index_daily",
    ts_code = "399004.SZ",
    start_date = "20100101",
    end_date = "20151231") %>% 
  as_tibble() %>% 
  transmute(
    date = ymd(trade_date),
    symbol = as.character(ts_code),
    close = as.numeric(close)
  ) %>% 
  tq_transmute(
    select = close,
    mutate_fun = periodReturn,
    period = "monthly",
    col_rename = "rb"
  ))

# 合并两个数据集：
(rarb <- left_join(ra, rb, by = "date"))

# 使用 tq_performance 运行 CAPM 模型
(rarb_capm <- rarb %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.CAPM
  ))

# 我们比较关心增长测度和风险测度
rarb_capm %>% 
  dplyr::select(Alpha, Beta)

# tidyquant的工作流程
# 单独资产分析
# 查看 SharpeRatio 函数的参数：
args(SharpeRatio)

# Step 1A: 获取股票数据
(stock_prices <- c("000001.SZ", "000002.SZ", "000004.SZ") %>% 
  tq_get(
    get = "stock.prices",
    from = "2010-01-01",
    to = "2015-12-31"
  ))


(stock_returns_monthly <- stock_prices %>% 
  group_by(symbol) %>% 
  tq_transmute(
    select = adjusted,
    mutate_fun = periodReturn,
    period = "monthly",
    col_rename = "ra"
  ))

# Step4: 绩效分析
stock_returns_monthly %>% 
  tq_performance(Ra = ra,
                 Rb = NULL,
                 performance_fun = SharpeRatio)

# 修改无风险利率和置信水平
stock_returns_monthly %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = SharpeRatio,
    Rf = 0.03 / 12,
    p = 0.99
  )

# 资产组合的分析
# 组合资产
# 方法1: 使用权重向量
wts <- c(0.5, 0, 0.5)
(portfolio_returns_monthly <- stock_returns_monthly %>% 
  tq_portfolio(
    assets_col = symbol,
    returns_col = ra,
    weights = wts,
    col_rename = "ra"
  ))

# 方法2: 使用资产 + 权重 数据框
(wts_map <- tibble(
  symbols = c('000001.SZ', '000004.SZ'),
  weights = c(0.5, 0.5)
))

stock_returns_monthly %>% 
  tq_portfolio(
    assets_col = symbol,
    returns_col = ra,
    weights = wts_map,
    col_rename = "ra_using_wts_map"
  )

# 合并ra和rb
(rarb_single_portfolio <- left_join(portfolio_returns_monthly,
                                   rb,
                                   by = "date"))
# 计算 CAPM 模型
rarb_single_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.CAPM
  )

# 多个资产组合
(stock_returns_monthly_multi <- stock_returns_monthly %>% 
  tq_repeat_df(n = 3))
weights <- c(
  0.5, 0.25, 0.25,
  0.25, 0.5, 0.25,
  0.25, 0.25, 0.5
)
stocks = c('000001.SZ', '000002.SZ', '000004.SZ')
(weights_table <- tibble(stocks) %>% 
  tq_repeat_df(3) %>% 
  bind_cols(tibble(weights)) %>% 
  group_by(portfolio))
(portfolio_returns_monthly_multi <- stock_returns_monthly_multi %>% 
  tq_portfolio(assets_col = symbol,
               returns_col = ra,
               weights = weights_table,
               col_rename = 'ra'))
(rarb_multi_portfolio <- left_join(
  portfolio_returns_monthly_multi, rb,
  by = "date"
))

rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.CAPM
  )

rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = SharpeRatio
  )

# 查看所有可以使用的函数
tq_performance_fun_options()

# table.Stats
# 返回基本统计量
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = table.Stats
  )

# table.CAPM
# 返回与CAPM相关的度量
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.CAPM
  )

# table.AnnualizedReturns
# 返回年度回报、标准差和夏普比率
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = table.AnnualizedReturns
  )

# table.Correlation
# 返回与基准收益的回报率
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.Correlation
  )

# table.DownsideRisk
# 返回下行风险表，以便在多个工具或基金之间进行比较
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.DownsideRisk
  )

# table.DownsideRiskRatio
# 返回每月下行风险、年度下行风险等指标
rarb_multi_portfolio %>% 
  tq_performance(Ra = ra,
                 Rb = NULL,
                 performance_fun = table.DownsideRiskRatio)

# table.HigherMoments
# 返回分布的高阶矩和协矩
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.HigherMoments
  )

# table.InformationRatio
# 返回追踪误差表、年度追踪误差和信息比率
rarb_multi_portfolio %>% 
  tq_performance(Ra = ra,
                 Rb = rb,
                 performance_fun = table.InformationRatio)
# table.Variability
# 返回平均绝对差异、月标准差和年标准差
rarb_multi_portfolio %>% 
  tq_performance(Ra = ra,
                 Rb = NULL,
                 performance_fun = table.Variability)

# 在险值
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = VaR
  )

# 夏普比率
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = SharpeRatio
  )

# 自定义tq_portfolio
portfolio_returns_monthly %>% 
  ggplot(aes(x = date, y = ra)) + 
  geom_bar(stat = "identity", fill = palette_light()[[3]],
           width = 20) + 
  labs(title = "Portfolio Returns",
       caption = "Shows an above-zero trend meaning positive returns",
       x = "", y = "Monthly Returns") + 
  geom_smooth(method = "lm") + 
  theme_tq(base_family = enfont) + 
  scale_color_tq() + 
  scale_y_continuous(labels = scales::percent)

ggsave("tidyquantperformance1.svg")

# 使用 wealth.index = TRUE 参数，我们可以观察10000元是如何增长的：
wts <- c(0.5, 0, 0.5)
portfolio_growth_monthly <- stock_returns_monthly %>% 
  tq_portfolio(
    assets_col = symbol,
    returns_col = ra,
    weights = wts,
    col_rename = "investment.growth",
    wealth.index = TRUE
  ) %>% 
  mutate(investment.growth = investment.growth * 10000)
portfolio_growth_monthly %>% 
  ggplot(aes(x = date, y = investment.growth)) + 
  geom_line(size = 1, color = palette_light()[3]) +
  labs(x = "", y = "Portfolio Value",
       title = "Portfolio Growth",
       caption = "Now we can really visualize performance!") + 
  geom_smooth(method = "loess") + 
  theme_tq(base_family = enfont) + 
  scale_color_tq() + 
  scale_y_continuous(labels = scales::dollar)

ggsave("tidyquantperformance2.svg")

# 下面再比较三个投资组合的增长情况
portfolio_growth_monthly_multi <- stock_returns_monthly_multi %>% 
  tq_portfolio(
    assets_col = symbol,
    returns_col = ra,
    weights = weights_table,
    col_rename = "investment.growth",
    wealth.index = TRUE
  ) %>% 
  mutate(investment.growth = investment.growth * 10000)
portfolio_growth_monthly_multi %>% 
  ggplot(aes(x = date, y = investment.growth, color = factor(portfolio))) + 
  geom_line(size = 2) + 
  labs(title = "Portfolio Growth",
       subtitle = "Comparing Multiple Portfolios",
       caption = "Portfolio 3 is a Standout!",
       x = "", 
       y = "Portfolio Value",
       color = "Portfolio") + 
  geom_smooth(method = "loess") + 
  theme_tq(base_family = enfont) + 
  scale_color_tq() +
  scale_y_continuous(labels = scales::dollar)

ggsave("tidyquantperformance3.svg")
# Performance Analysis with tidyquant
library(tidyquant)
(ra <- c("000001.SZ", "000002.SZ", "000004.SZ") %>% 
  tq_get(get = "stock.prices",
         from = "2010-01-01",
         to = "2015-12-31") %>% 
  group_by(symbol) %>% 
  tq_transmute(
    select = adjusted,
    mutate_fun = periodReturn,
    period = "monthly",
    col_rename = "ra"
  ))

# 下面我们使用深证100R指数得到基准价格：
library(Tushare)
api = pro_api('fd65bf1ee56215d0ce1a95d1fa3b453d97a1779438aca26c4b7937d1')
(rb <- api(api_name = "index_daily",
    ts_code = "399004.SZ",
    start_date = "20100101",
    end_date = "20151231") %>% 
  as_tibble() %>% 
  transmute(
    date = ymd(trade_date),
    symbol = as.character(ts_code),
    close = as.numeric(close)
  ) %>% 
  tq_transmute(
    select = close,
    mutate_fun = periodReturn,
    period = "monthly",
    col_rename = "rb"
  ))

# 合并两个数据集：
(rarb <- left_join(ra, rb, by = "date"))

# 使用 tq_performance 运行 CAPM 模型
(rarb_capm <- rarb %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.CAPM
  ))

# 我们比较关心增长测度和风险测度
rarb_capm %>% 
  dplyr::select(Alpha, Beta)

# tidyquant的工作流程
# 单独资产分析
# 查看 SharpeRatio 函数的参数：
args(SharpeRatio)

# Step 1A: 获取股票数据
(stock_prices <- c("000001.SZ", "000002.SZ", "000004.SZ") %>% 
  tq_get(
    get = "stock.prices",
    from = "2010-01-01",
    to = "2015-12-31"
  ))


(stock_returns_monthly <- stock_prices %>% 
  group_by(symbol) %>% 
  tq_transmute(
    select = adjusted,
    mutate_fun = periodReturn,
    period = "monthly",
    col_rename = "ra"
  ))

# Step4: 绩效分析
stock_returns_monthly %>% 
  tq_performance(Ra = ra,
                 Rb = NULL,
                 performance_fun = SharpeRatio)

# 修改无风险利率和置信水平
stock_returns_monthly %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = SharpeRatio,
    Rf = 0.03 / 12,
    p = 0.99
  )

# 资产组合的分析
# 组合资产
# 方法1: 使用权重向量
wts <- c(0.5, 0, 0.5)
(portfolio_returns_monthly <- stock_returns_monthly %>% 
  tq_portfolio(
    assets_col = symbol,
    returns_col = ra,
    weights = wts,
    col_rename = "ra"
  ))

# 方法2: 使用资产 + 权重 数据框
(wts_map <- tibble(
  symbols = c('000001.SZ', '000004.SZ'),
  weights = c(0.5, 0.5)
))

stock_returns_monthly %>% 
  tq_portfolio(
    assets_col = symbol,
    returns_col = ra,
    weights = wts_map,
    col_rename = "ra_using_wts_map"
  )

# 合并ra和rb
(rarb_single_portfolio <- left_join(portfolio_returns_monthly,
                                   rb,
                                   by = "date"))
# 计算 CAPM 模型
rarb_single_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.CAPM
  )

# 多个资产组合
(stock_returns_monthly_multi <- stock_returns_monthly %>% 
  tq_repeat_df(n = 3))
weights <- c(
  0.5, 0.25, 0.25,
  0.25, 0.5, 0.25,
  0.25, 0.25, 0.5
)
stocks = c('000001.SZ', '000002.SZ', '000004.SZ')
(weights_table <- tibble(stocks) %>% 
  tq_repeat_df(3) %>% 
  bind_cols(tibble(weights)) %>% 
  group_by(portfolio))
(portfolio_returns_monthly_multi <- stock_returns_monthly_multi %>% 
  tq_portfolio(assets_col = symbol,
               returns_col = ra,
               weights = weights_table,
               col_rename = 'ra'))
(rarb_multi_portfolio <- left_join(
  portfolio_returns_monthly_multi, rb,
  by = "date"
))

rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.CAPM
  )

rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = SharpeRatio
  )

# 查看所有可以使用的函数
tq_performance_fun_options()

# table.Stats
# 返回基本统计量
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = table.Stats
  )

# table.CAPM
# 返回与CAPM相关的度量
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.CAPM
  )

# table.AnnualizedReturns
# 返回年度回报、标准差和夏普比率
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = table.AnnualizedReturns
  )

# table.Correlation
# 返回与基准收益的回报率
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.Correlation
  )

# table.DownsideRisk
# 返回下行风险表，以便在多个工具或基金之间进行比较
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.DownsideRisk
  )

# table.DownsideRiskRatio
# 返回每月下行风险、年度下行风险等指标
rarb_multi_portfolio %>% 
  tq_performance(Ra = ra,
                 Rb = NULL,
                 performance_fun = table.DownsideRiskRatio)

# table.HigherMoments
# 返回分布的高阶矩和协矩
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.HigherMoments
  )

# table.InformationRatio
# 返回追踪误差表、年度追踪误差和信息比率
rarb_multi_portfolio %>% 
  tq_performance(Ra = ra,
                 Rb = rb,
                 performance_fun = table.InformationRatio)
# table.Variability
# 返回平均绝对差异、月标准差和年标准差
rarb_multi_portfolio %>% 
  tq_performance(Ra = ra,
                 Rb = NULL,
                 performance_fun = table.Variability)

# 在险值
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = VaR
  )

# 夏普比率
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = SharpeRatio
  )

# 自定义tq_portfolio
portfolio_returns_monthly %>% 
  ggplot(aes(x = date, y = ra)) + 
  geom_bar(stat = "identity", fill = palette_light()[[3]],
           width = 20) + 
  labs(title = "Portfolio Returns",
       caption = "Shows an above-zero trend meaning positive returns",
       x = "", y = "Monthly Returns") + 
  geom_smooth(method = "lm") + 
  theme_tq(base_family = enfont) + 
  scale_color_tq() + 
  scale_y_continuous(labels = scales::percent)

ggsave("tidyquantperformance1.svg")

# 使用 wealth.index = TRUE 参数，我们可以观察10000元是如何增长的：
wts <- c(0.5, 0, 0.5)
portfolio_growth_monthly <- stock_returns_monthly %>% 
  tq_portfolio(
    assets_col = symbol,
    returns_col = ra,
    weights = wts,
    col_rename = "investment.growth",
    wealth.index = TRUE
  ) %>% 
  mutate(investment.growth = investment.growth * 10000)
portfolio_growth_monthly %>% 
  ggplot(aes(x = date, y = investment.growth)) + 
  geom_line(size = 1, color = palette_light()[3]) +
  labs(x = "", y = "Portfolio Value",
       title = "Portfolio Growth",
       caption = "Now we can really visualize performance!") + 
  geom_smooth(method = "loess") + 
  theme_tq(base_family = enfont) + 
  scale_color_tq() + 
  scale_y_continuous(labels = scales::dollar)

ggsave("tidyquantperformance2.svg")

# 下面再比较三个投资组合的增长情况
portfolio_growth_monthly_multi <- stock_returns_monthly_multi %>% 
  tq_portfolio(
    assets_col = symbol,
    returns_col = ra,
    weights = weights_table,
    col_rename = "investment.growth",
    wealth.index = TRUE
  ) %>% 
  mutate(investment.growth = investment.growth * 10000)
portfolio_growth_monthly_multi %>% 
  ggplot(aes(x = date, y = investment.growth, color = factor(portfolio))) + 
  geom_line(size = 2) + 
  labs(title = "Portfolio Growth",
       subtitle = "Comparing Multiple Portfolios",
       caption = "Portfolio 3 is a Standout!",
       x = "", 
       y = "Portfolio Value",
       color = "Portfolio") + 
  geom_smooth(method = "loess") + 
  theme_tq(base_family = enfont) + 
  scale_color_tq() +
  scale_y_continuous(labels = scales::dollar)

ggsave("tidyquantperformance3.svg")
# Performance Analysis with tidyquant
library(tidyquant)
(ra <- c("000001.SZ", "000002.SZ", "000004.SZ") %>% 
  tq_get(get = "stock.prices",
         from = "2010-01-01",
         to = "2015-12-31") %>% 
  group_by(symbol) %>% 
  tq_transmute(
    select = adjusted,
    mutate_fun = periodReturn,
    period = "monthly",
    col_rename = "ra"
  ))

# 下面我们使用深证100R指数得到基准价格：
library(Tushare)
api = pro_api('fd65bf1ee56215d0ce1a95d1fa3b453d97a1779438aca26c4b7937d1')
(rb <- api(api_name = "index_daily",
    ts_code = "399004.SZ",
    start_date = "20100101",
    end_date = "20151231") %>% 
  as_tibble() %>% 
  transmute(
    date = ymd(trade_date),
    symbol = as.character(ts_code),
    close = as.numeric(close)
  ) %>% 
  tq_transmute(
    select = close,
    mutate_fun = periodReturn,
    period = "monthly",
    col_rename = "rb"
  ))

# 合并两个数据集：
(rarb <- left_join(ra, rb, by = "date"))

# 使用 tq_performance 运行 CAPM 模型
(rarb_capm <- rarb %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.CAPM
  ))

# 我们比较关心增长测度和风险测度
rarb_capm %>% 
  dplyr::select(Alpha, Beta)

# tidyquant的工作流程
# 单独资产分析
# 查看 SharpeRatio 函数的参数：
args(SharpeRatio)

# Step 1A: 获取股票数据
(stock_prices <- c("000001.SZ", "000002.SZ", "000004.SZ") %>% 
  tq_get(
    get = "stock.prices",
    from = "2010-01-01",
    to = "2015-12-31"
  ))


(stock_returns_monthly <- stock_prices %>% 
  group_by(symbol) %>% 
  tq_transmute(
    select = adjusted,
    mutate_fun = periodReturn,
    period = "monthly",
    col_rename = "ra"
  ))

# Step4: 绩效分析
stock_returns_monthly %>% 
  tq_performance(Ra = ra,
                 Rb = NULL,
                 performance_fun = SharpeRatio)

# 修改无风险利率和置信水平
stock_returns_monthly %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = SharpeRatio,
    Rf = 0.03 / 12,
    p = 0.99
  )

# 资产组合的分析
# 组合资产
# 方法1: 使用权重向量
wts <- c(0.5, 0, 0.5)
(portfolio_returns_monthly <- stock_returns_monthly %>% 
  tq_portfolio(
    assets_col = symbol,
    returns_col = ra,
    weights = wts,
    col_rename = "ra"
  ))

# 方法2: 使用资产 + 权重 数据框
(wts_map <- tibble(
  symbols = c('000001.SZ', '000004.SZ'),
  weights = c(0.5, 0.5)
))

stock_returns_monthly %>% 
  tq_portfolio(
    assets_col = symbol,
    returns_col = ra,
    weights = wts_map,
    col_rename = "ra_using_wts_map"
  )

# 合并ra和rb
(rarb_single_portfolio <- left_join(portfolio_returns_monthly,
                                   rb,
                                   by = "date"))
# 计算 CAPM 模型
rarb_single_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.CAPM
  )

# 多个资产组合
(stock_returns_monthly_multi <- stock_returns_monthly %>% 
  tq_repeat_df(n = 3))
weights <- c(
  0.5, 0.25, 0.25,
  0.25, 0.5, 0.25,
  0.25, 0.25, 0.5
)
stocks = c('000001.SZ', '000002.SZ', '000004.SZ')
(weights_table <- tibble(stocks) %>% 
  tq_repeat_df(3) %>% 
  bind_cols(tibble(weights)) %>% 
  group_by(portfolio))
(portfolio_returns_monthly_multi <- stock_returns_monthly_multi %>% 
  tq_portfolio(assets_col = symbol,
               returns_col = ra,
               weights = weights_table,
               col_rename = 'ra'))
(rarb_multi_portfolio <- left_join(
  portfolio_returns_monthly_multi, rb,
  by = "date"
))

rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.CAPM
  )

rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = SharpeRatio
  )

# 查看所有可以使用的函数
tq_performance_fun_options()

# table.Stats
# 返回基本统计量
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = table.Stats
  )

# table.CAPM
# 返回与CAPM相关的度量
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.CAPM
  )

# table.AnnualizedReturns
# 返回年度回报、标准差和夏普比率
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = table.AnnualizedReturns
  )

# table.Correlation
# 返回与基准收益的回报率
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.Correlation
  )

# table.DownsideRisk
# 返回下行风险表，以便在多个工具或基金之间进行比较
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.DownsideRisk
  )

# table.DownsideRiskRatio
# 返回每月下行风险、年度下行风险等指标
rarb_multi_portfolio %>% 
  tq_performance(Ra = ra,
                 Rb = NULL,
                 performance_fun = table.DownsideRiskRatio)

# table.HigherMoments
# 返回分布的高阶矩和协矩
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.HigherMoments
  )

# table.InformationRatio
# 返回追踪误差表、年度追踪误差和信息比率
rarb_multi_portfolio %>% 
  tq_performance(Ra = ra,
                 Rb = rb,
                 performance_fun = table.InformationRatio)
# table.Variability
# 返回平均绝对差异、月标准差和年标准差
rarb_multi_portfolio %>% 
  tq_performance(Ra = ra,
                 Rb = NULL,
                 performance_fun = table.Variability)

# 在险值
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = VaR
  )

# 夏普比率
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = SharpeRatio
  )

# 自定义tq_portfolio
portfolio_returns_monthly %>% 
  ggplot(aes(x = date, y = ra)) + 
  geom_bar(stat = "identity", fill = palette_light()[[3]],
           width = 20) + 
  labs(title = "Portfolio Returns",
       caption = "Shows an above-zero trend meaning positive returns",
       x = "", y = "Monthly Returns") + 
  geom_smooth(method = "lm") + 
  theme_tq(base_family = enfont) + 
  scale_color_tq() + 
  scale_y_continuous(labels = scales::percent)

ggsave("tidyquantperformance1.svg")

# 使用 wealth.index = TRUE 参数，我们可以观察10000元是如何增长的：
wts <- c(0.5, 0, 0.5)
portfolio_growth_monthly <- stock_returns_monthly %>% 
  tq_portfolio(
    assets_col = symbol,
    returns_col = ra,
    weights = wts,
    col_rename = "investment.growth",
    wealth.index = TRUE
  ) %>% 
  mutate(investment.growth = investment.growth * 10000)
portfolio_growth_monthly %>% 
  ggplot(aes(x = date, y = investment.growth)) + 
  geom_line(size = 1, color = palette_light()[3]) +
  labs(x = "", y = "Portfolio Value",
       title = "Portfolio Growth",
       caption = "Now we can really visualize performance!") + 
  geom_smooth(method = "loess") + 
  theme_tq(base_family = enfont) + 
  scale_color_tq() + 
  scale_y_continuous(labels = scales::dollar)

ggsave("tidyquantperformance2.svg")

# 下面再比较三个投资组合的增长情况
portfolio_growth_monthly_multi <- stock_returns_monthly_multi %>% 
  tq_portfolio(
    assets_col = symbol,
    returns_col = ra,
    weights = weights_table,
    col_rename = "investment.growth",
    wealth.index = TRUE
  ) %>% 
  mutate(investment.growth = investment.growth * 10000)
portfolio_growth_monthly_multi %>% 
  ggplot(aes(x = date, y = investment.growth, color = factor(portfolio))) + 
  geom_line(size = 2) + 
  labs(title = "Portfolio Growth",
       subtitle = "Comparing Multiple Portfolios",
       caption = "Portfolio 3 is a Standout!",
       x = "", 
       y = "Portfolio Value",
       color = "Portfolio") + 
  geom_smooth(method = "loess") + 
  theme_tq(base_family = enfont) + 
  scale_color_tq() +
  scale_y_continuous(labels = scales::dollar)

ggsave("tidyquantperformance3.svg")
# Performance Analysis with tidyquant
library(tidyquant)
(ra <- c("000001.SZ", "000002.SZ", "000004.SZ") %>% 
  tq_get(get = "stock.prices",
         from = "2010-01-01",
         to = "2015-12-31") %>% 
  group_by(symbol) %>% 
  tq_transmute(
    select = adjusted,
    mutate_fun = periodReturn,
    period = "monthly",
    col_rename = "ra"
  ))

# 下面我们使用深证100R指数得到基准价格：
library(Tushare)
api = pro_api('fd65bf1ee56215d0ce1a95d1fa3b453d97a1779438aca26c4b7937d1')
(rb <- api(api_name = "index_daily",
    ts_code = "399004.SZ",
    start_date = "20100101",
    end_date = "20151231") %>% 
  as_tibble() %>% 
  transmute(
    date = ymd(trade_date),
    symbol = as.character(ts_code),
    close = as.numeric(close)
  ) %>% 
  tq_transmute(
    select = close,
    mutate_fun = periodReturn,
    period = "monthly",
    col_rename = "rb"
  ))

# 合并两个数据集：
(rarb <- left_join(ra, rb, by = "date"))

# 使用 tq_performance 运行 CAPM 模型
(rarb_capm <- rarb %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.CAPM
  ))

# 我们比较关心增长测度和风险测度
rarb_capm %>% 
  dplyr::select(Alpha, Beta)

# tidyquant的工作流程
# 单独资产分析
# 查看 SharpeRatio 函数的参数：
args(SharpeRatio)

# Step 1A: 获取股票数据
(stock_prices <- c("000001.SZ", "000002.SZ", "000004.SZ") %>% 
  tq_get(
    get = "stock.prices",
    from = "2010-01-01",
    to = "2015-12-31"
  ))


(stock_returns_monthly <- stock_prices %>% 
  group_by(symbol) %>% 
  tq_transmute(
    select = adjusted,
    mutate_fun = periodReturn,
    period = "monthly",
    col_rename = "ra"
  ))

# Step4: 绩效分析
stock_returns_monthly %>% 
  tq_performance(Ra = ra,
                 Rb = NULL,
                 performance_fun = SharpeRatio)

# 修改无风险利率和置信水平
stock_returns_monthly %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = SharpeRatio,
    Rf = 0.03 / 12,
    p = 0.99
  )

# 资产组合的分析
# 组合资产
# 方法1: 使用权重向量
wts <- c(0.5, 0, 0.5)
(portfolio_returns_monthly <- stock_returns_monthly %>% 
  tq_portfolio(
    assets_col = symbol,
    returns_col = ra,
    weights = wts,
    col_rename = "ra"
  ))

# 方法2: 使用资产 + 权重 数据框
(wts_map <- tibble(
  symbols = c('000001.SZ', '000004.SZ'),
  weights = c(0.5, 0.5)
))

stock_returns_monthly %>% 
  tq_portfolio(
    assets_col = symbol,
    returns_col = ra,
    weights = wts_map,
    col_rename = "ra_using_wts_map"
  )

# 合并ra和rb
(rarb_single_portfolio <- left_join(portfolio_returns_monthly,
                                   rb,
                                   by = "date"))
# 计算 CAPM 模型
rarb_single_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.CAPM
  )

# 多个资产组合
(stock_returns_monthly_multi <- stock_returns_monthly %>% 
  tq_repeat_df(n = 3))
weights <- c(
  0.5, 0.25, 0.25,
  0.25, 0.5, 0.25,
  0.25, 0.25, 0.5
)
stocks = c('000001.SZ', '000002.SZ', '000004.SZ')
(weights_table <- tibble(stocks) %>% 
  tq_repeat_df(3) %>% 
  bind_cols(tibble(weights)) %>% 
  group_by(portfolio))
(portfolio_returns_monthly_multi <- stock_returns_monthly_multi %>% 
  tq_portfolio(assets_col = symbol,
               returns_col = ra,
               weights = weights_table,
               col_rename = 'ra'))
(rarb_multi_portfolio <- left_join(
  portfolio_returns_monthly_multi, rb,
  by = "date"
))

rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.CAPM
  )

rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = SharpeRatio
  )

# 查看所有可以使用的函数
tq_performance_fun_options()

# table.Stats
# 返回基本统计量
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = table.Stats
  )

# table.CAPM
# 返回与CAPM相关的度量
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.CAPM
  )

# table.AnnualizedReturns
# 返回年度回报、标准差和夏普比率
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = table.AnnualizedReturns
  )

# table.Correlation
# 返回与基准收益的回报率
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.Correlation
  )

# table.DownsideRisk
# 返回下行风险表，以便在多个工具或基金之间进行比较
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.DownsideRisk
  )

# table.DownsideRiskRatio
# 返回每月下行风险、年度下行风险等指标
rarb_multi_portfolio %>% 
  tq_performance(Ra = ra,
                 Rb = NULL,
                 performance_fun = table.DownsideRiskRatio)

# table.HigherMoments
# 返回分布的高阶矩和协矩
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.HigherMoments
  )

# table.InformationRatio
# 返回追踪误差表、年度追踪误差和信息比率
rarb_multi_portfolio %>% 
  tq_performance(Ra = ra,
                 Rb = rb,
                 performance_fun = table.InformationRatio)
# table.Variability
# 返回平均绝对差异、月标准差和年标准差
rarb_multi_portfolio %>% 
  tq_performance(Ra = ra,
                 Rb = NULL,
                 performance_fun = table.Variability)

# 在险值
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = VaR
  )

# 夏普比率
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = SharpeRatio
  )

# 自定义tq_portfolio
portfolio_returns_monthly %>% 
  ggplot(aes(x = date, y = ra)) + 
  geom_bar(stat = "identity", fill = palette_light()[[3]],
           width = 20) + 
  labs(title = "Portfolio Returns",
       caption = "Shows an above-zero trend meaning positive returns",
       x = "", y = "Monthly Returns") + 
  geom_smooth(method = "lm") + 
  theme_tq(base_family = enfont) + 
  scale_color_tq() + 
  scale_y_continuous(labels = scales::percent)

ggsave("tidyquantperformance1.svg")

# 使用 wealth.index = TRUE 参数，我们可以观察10000元是如何增长的：
wts <- c(0.5, 0, 0.5)
portfolio_growth_monthly <- stock_returns_monthly %>% 
  tq_portfolio(
    assets_col = symbol,
    returns_col = ra,
    weights = wts,
    col_rename = "investment.growth",
    wealth.index = TRUE
  ) %>% 
  mutate(investment.growth = investment.growth * 10000)
portfolio_growth_monthly %>% 
  ggplot(aes(x = date, y = investment.growth)) + 
  geom_line(size = 1, color = palette_light()[3]) +
  labs(x = "", y = "Portfolio Value",
       title = "Portfolio Growth",
       caption = "Now we can really visualize performance!") + 
  geom_smooth(method = "loess") + 
  theme_tq(base_family = enfont) + 
  scale_color_tq() + 
  scale_y_continuous(labels = scales::dollar)

ggsave("tidyquantperformance2.svg")

# 下面再比较三个投资组合的增长情况
portfolio_growth_monthly_multi <- stock_returns_monthly_multi %>% 
  tq_portfolio(
    assets_col = symbol,
    returns_col = ra,
    weights = weights_table,
    col_rename = "investment.growth",
    wealth.index = TRUE
  ) %>% 
  mutate(investment.growth = investment.growth * 10000)
portfolio_growth_monthly_multi %>% 
  ggplot(aes(x = date, y = investment.growth, color = factor(portfolio))) + 
  geom_line(size = 2) + 
  labs(title = "Portfolio Growth",
       subtitle = "Comparing Multiple Portfolios",
       caption = "Portfolio 3 is a Standout!",
       x = "", 
       y = "Portfolio Value",
       color = "Portfolio") + 
  geom_smooth(method = "loess") + 
  theme_tq(base_family = enfont) + 
  scale_color_tq() +
  scale_y_continuous(labels = scales::dollar)

ggsave("tidyquantperformance3.svg")
# Performance Analysis with tidyquant
library(tidyquant)
(ra <- c("000001.SZ", "000002.SZ", "000004.SZ") %>% 
  tq_get(get = "stock.prices",
         from = "2010-01-01",
         to = "2015-12-31") %>% 
  group_by(symbol) %>% 
  tq_transmute(
    select = adjusted,
    mutate_fun = periodReturn,
    period = "monthly",
    col_rename = "ra"
  ))

# 下面我们使用深证100R指数得到基准价格：
library(Tushare)
api = pro_api('fd65bf1ee56215d0ce1a95d1fa3b453d97a1779438aca26c4b7937d1')
(rb <- api(api_name = "index_daily",
    ts_code = "399004.SZ",
    start_date = "20100101",
    end_date = "20151231") %>% 
  as_tibble() %>% 
  transmute(
    date = ymd(trade_date),
    symbol = as.character(ts_code),
    close = as.numeric(close)
  ) %>% 
  tq_transmute(
    select = close,
    mutate_fun = periodReturn,
    period = "monthly",
    col_rename = "rb"
  ))

# 合并两个数据集：
(rarb <- left_join(ra, rb, by = "date"))

# 使用 tq_performance 运行 CAPM 模型
(rarb_capm <- rarb %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.CAPM
  ))

# 我们比较关心增长测度和风险测度
rarb_capm %>% 
  dplyr::select(Alpha, Beta)

# tidyquant的工作流程
# 单独资产分析
# 查看 SharpeRatio 函数的参数：
args(SharpeRatio)

# Step 1A: 获取股票数据
(stock_prices <- c("000001.SZ", "000002.SZ", "000004.SZ") %>% 
  tq_get(
    get = "stock.prices",
    from = "2010-01-01",
    to = "2015-12-31"
  ))


(stock_returns_monthly <- stock_prices %>% 
  group_by(symbol) %>% 
  tq_transmute(
    select = adjusted,
    mutate_fun = periodReturn,
    period = "monthly",
    col_rename = "ra"
  ))

# Step4: 绩效分析
stock_returns_monthly %>% 
  tq_performance(Ra = ra,
                 Rb = NULL,
                 performance_fun = SharpeRatio)

# 修改无风险利率和置信水平
stock_returns_monthly %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = SharpeRatio,
    Rf = 0.03 / 12,
    p = 0.99
  )

# 资产组合的分析
# 组合资产
# 方法1: 使用权重向量
wts <- c(0.5, 0, 0.5)
(portfolio_returns_monthly <- stock_returns_monthly %>% 
  tq_portfolio(
    assets_col = symbol,
    returns_col = ra,
    weights = wts,
    col_rename = "ra"
  ))

# 方法2: 使用资产 + 权重 数据框
(wts_map <- tibble(
  symbols = c('000001.SZ', '000004.SZ'),
  weights = c(0.5, 0.5)
))

stock_returns_monthly %>% 
  tq_portfolio(
    assets_col = symbol,
    returns_col = ra,
    weights = wts_map,
    col_rename = "ra_using_wts_map"
  )

# 合并ra和rb
(rarb_single_portfolio <- left_join(portfolio_returns_monthly,
                                   rb,
                                   by = "date"))
# 计算 CAPM 模型
rarb_single_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.CAPM
  )

# 多个资产组合
(stock_returns_monthly_multi <- stock_returns_monthly %>% 
  tq_repeat_df(n = 3))
weights <- c(
  0.5, 0.25, 0.25,
  0.25, 0.5, 0.25,
  0.25, 0.25, 0.5
)
stocks = c('000001.SZ', '000002.SZ', '000004.SZ')
(weights_table <- tibble(stocks) %>% 
  tq_repeat_df(3) %>% 
  bind_cols(tibble(weights)) %>% 
  group_by(portfolio))
(portfolio_returns_monthly_multi <- stock_returns_monthly_multi %>% 
  tq_portfolio(assets_col = symbol,
               returns_col = ra,
               weights = weights_table,
               col_rename = 'ra'))
(rarb_multi_portfolio <- left_join(
  portfolio_returns_monthly_multi, rb,
  by = "date"
))

rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.CAPM
  )

rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = SharpeRatio
  )

# 查看所有可以使用的函数
tq_performance_fun_options()

# table.Stats
# 返回基本统计量
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = table.Stats
  )

# table.CAPM
# 返回与CAPM相关的度量
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.CAPM
  )

# table.AnnualizedReturns
# 返回年度回报、标准差和夏普比率
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = table.AnnualizedReturns
  )

# table.Correlation
# 返回与基准收益的回报率
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.Correlation
  )

# table.DownsideRisk
# 返回下行风险表，以便在多个工具或基金之间进行比较
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.DownsideRisk
  )

# table.DownsideRiskRatio
# 返回每月下行风险、年度下行风险等指标
rarb_multi_portfolio %>% 
  tq_performance(Ra = ra,
                 Rb = NULL,
                 performance_fun = table.DownsideRiskRatio)

# table.HigherMoments
# 返回分布的高阶矩和协矩
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.HigherMoments
  )

# table.InformationRatio
# 返回追踪误差表、年度追踪误差和信息比率
rarb_multi_portfolio %>% 
  tq_performance(Ra = ra,
                 Rb = rb,
                 performance_fun = table.InformationRatio)
# table.Variability
# 返回平均绝对差异、月标准差和年标准差
rarb_multi_portfolio %>% 
  tq_performance(Ra = ra,
                 Rb = NULL,
                 performance_fun = table.Variability)

# 在险值
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = VaR
  )

# 夏普比率
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = SharpeRatio
  )

# 自定义tq_portfolio
portfolio_returns_monthly %>% 
  ggplot(aes(x = date, y = ra)) + 
  geom_bar(stat = "identity", fill = palette_light()[[3]],
           width = 20) + 
  labs(title = "Portfolio Returns",
       caption = "Shows an above-zero trend meaning positive returns",
       x = "", y = "Monthly Returns") + 
  geom_smooth(method = "lm") + 
  theme_tq(base_family = enfont) + 
  scale_color_tq() + 
  scale_y_continuous(labels = scales::percent)

ggsave("tidyquantperformance1.svg")

# 使用 wealth.index = TRUE 参数，我们可以观察10000元是如何增长的：
wts <- c(0.5, 0, 0.5)
portfolio_growth_monthly <- stock_returns_monthly %>% 
  tq_portfolio(
    assets_col = symbol,
    returns_col = ra,
    weights = wts,
    col_rename = "investment.growth",
    wealth.index = TRUE
  ) %>% 
  mutate(investment.growth = investment.growth * 10000)
portfolio_growth_monthly %>% 
  ggplot(aes(x = date, y = investment.growth)) + 
  geom_line(size = 1, color = palette_light()[3]) +
  labs(x = "", y = "Portfolio Value",
       title = "Portfolio Growth",
       caption = "Now we can really visualize performance!") + 
  geom_smooth(method = "loess") + 
  theme_tq(base_family = enfont) + 
  scale_color_tq() + 
  scale_y_continuous(labels = scales::dollar)

ggsave("tidyquantperformance2.svg")

# 下面再比较三个投资组合的增长情况
portfolio_growth_monthly_multi <- stock_returns_monthly_multi %>% 
  tq_portfolio(
    assets_col = symbol,
    returns_col = ra,
    weights = weights_table,
    col_rename = "investment.growth",
    wealth.index = TRUE
  ) %>% 
  mutate(investment.growth = investment.growth * 10000)
portfolio_growth_monthly_multi %>% 
  ggplot(aes(x = date, y = investment.growth, color = factor(portfolio))) + 
  geom_line(size = 2) + 
  labs(title = "Portfolio Growth",
       subtitle = "Comparing Multiple Portfolios",
       caption = "Portfolio 3 is a Standout!",
       x = "", 
       y = "Portfolio Value",
       color = "Portfolio") + 
  geom_smooth(method = "loess") + 
  theme_tq(base_family = enfont) + 
  scale_color_tq() +
  scale_y_continuous(labels = scales::dollar)

ggsave("tidyquantperformance3.svg")
# Performance Analysis with tidyquant
library(tidyquant)
(ra <- c("000001.SZ", "000002.SZ", "000004.SZ") %>% 
  tq_get(get = "stock.prices",
         from = "2010-01-01",
         to = "2015-12-31") %>% 
  group_by(symbol) %>% 
  tq_transmute(
    select = adjusted,
    mutate_fun = periodReturn,
    period = "monthly",
    col_rename = "ra"
  ))

# 下面我们使用深证100R指数得到基准价格：
library(Tushare)
api = pro_api('fd65bf1ee56215d0ce1a95d1fa3b453d97a1779438aca26c4b7937d1')
(rb <- api(api_name = "index_daily",
    ts_code = "399004.SZ",
    start_date = "20100101",
    end_date = "20151231") %>% 
  as_tibble() %>% 
  transmute(
    date = ymd(trade_date),
    symbol = as.character(ts_code),
    close = as.numeric(close)
  ) %>% 
  tq_transmute(
    select = close,
    mutate_fun = periodReturn,
    period = "monthly",
    col_rename = "rb"
  ))

# 合并两个数据集：
(rarb <- left_join(ra, rb, by = "date"))

# 使用 tq_performance 运行 CAPM 模型
(rarb_capm <- rarb %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.CAPM
  ))

# 我们比较关心增长测度和风险测度
rarb_capm %>% 
  dplyr::select(Alpha, Beta)

# tidyquant的工作流程
# 单独资产分析
# 查看 SharpeRatio 函数的参数：
args(SharpeRatio)

# Step 1A: 获取股票数据
(stock_prices <- c("000001.SZ", "000002.SZ", "000004.SZ") %>% 
  tq_get(
    get = "stock.prices",
    from = "2010-01-01",
    to = "2015-12-31"
  ))


(stock_returns_monthly <- stock_prices %>% 
  group_by(symbol) %>% 
  tq_transmute(
    select = adjusted,
    mutate_fun = periodReturn,
    period = "monthly",
    col_rename = "ra"
  ))

# Step4: 绩效分析
stock_returns_monthly %>% 
  tq_performance(Ra = ra,
                 Rb = NULL,
                 performance_fun = SharpeRatio)

# 修改无风险利率和置信水平
stock_returns_monthly %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = SharpeRatio,
    Rf = 0.03 / 12,
    p = 0.99
  )

# 资产组合的分析
# 组合资产
# 方法1: 使用权重向量
wts <- c(0.5, 0, 0.5)
(portfolio_returns_monthly <- stock_returns_monthly %>% 
  tq_portfolio(
    assets_col = symbol,
    returns_col = ra,
    weights = wts,
    col_rename = "ra"
  ))

# 方法2: 使用资产 + 权重 数据框
(wts_map <- tibble(
  symbols = c('000001.SZ', '000004.SZ'),
  weights = c(0.5, 0.5)
))

stock_returns_monthly %>% 
  tq_portfolio(
    assets_col = symbol,
    returns_col = ra,
    weights = wts_map,
    col_rename = "ra_using_wts_map"
  )

# 合并ra和rb
(rarb_single_portfolio <- left_join(portfolio_returns_monthly,
                                   rb,
                                   by = "date"))
# 计算 CAPM 模型
rarb_single_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.CAPM
  )

# 多个资产组合
(stock_returns_monthly_multi <- stock_returns_monthly %>% 
  tq_repeat_df(n = 3))
weights <- c(
  0.5, 0.25, 0.25,
  0.25, 0.5, 0.25,
  0.25, 0.25, 0.5
)
stocks = c('000001.SZ', '000002.SZ', '000004.SZ')
(weights_table <- tibble(stocks) %>% 
  tq_repeat_df(3) %>% 
  bind_cols(tibble(weights)) %>% 
  group_by(portfolio))
(portfolio_returns_monthly_multi <- stock_returns_monthly_multi %>% 
  tq_portfolio(assets_col = symbol,
               returns_col = ra,
               weights = weights_table,
               col_rename = 'ra'))
(rarb_multi_portfolio <- left_join(
  portfolio_returns_monthly_multi, rb,
  by = "date"
))

rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.CAPM
  )

rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = SharpeRatio
  )

# 查看所有可以使用的函数
tq_performance_fun_options()

# table.Stats
# 返回基本统计量
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = table.Stats
  )

# table.CAPM
# 返回与CAPM相关的度量
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.CAPM
  )

# table.AnnualizedReturns
# 返回年度回报、标准差和夏普比率
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = table.AnnualizedReturns
  )

# table.Correlation
# 返回与基准收益的回报率
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.Correlation
  )

# table.DownsideRisk
# 返回下行风险表，以便在多个工具或基金之间进行比较
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.DownsideRisk
  )

# table.DownsideRiskRatio
# 返回每月下行风险、年度下行风险等指标
rarb_multi_portfolio %>% 
  tq_performance(Ra = ra,
                 Rb = NULL,
                 performance_fun = table.DownsideRiskRatio)

# table.HigherMoments
# 返回分布的高阶矩和协矩
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.HigherMoments
  )

# table.InformationRatio
# 返回追踪误差表、年度追踪误差和信息比率
rarb_multi_portfolio %>% 
  tq_performance(Ra = ra,
                 Rb = rb,
                 performance_fun = table.InformationRatio)
# table.Variability
# 返回平均绝对差异、月标准差和年标准差
rarb_multi_portfolio %>% 
  tq_performance(Ra = ra,
                 Rb = NULL,
                 performance_fun = table.Variability)

# 在险值
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = VaR
  )

# 夏普比率
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = SharpeRatio
  )

# 自定义tq_portfolio
portfolio_returns_monthly %>% 
  ggplot(aes(x = date, y = ra)) + 
  geom_bar(stat = "identity", fill = palette_light()[[3]],
           width = 20) + 
  labs(title = "Portfolio Returns",
       caption = "Shows an above-zero trend meaning positive returns",
       x = "", y = "Monthly Returns") + 
  geom_smooth(method = "lm") + 
  theme_tq(base_family = enfont) + 
  scale_color_tq() + 
  scale_y_continuous(labels = scales::percent)

ggsave("tidyquantperformance1.svg")

# 使用 wealth.index = TRUE 参数，我们可以观察10000元是如何增长的：
wts <- c(0.5, 0, 0.5)
portfolio_growth_monthly <- stock_returns_monthly %>% 
  tq_portfolio(
    assets_col = symbol,
    returns_col = ra,
    weights = wts,
    col_rename = "investment.growth",
    wealth.index = TRUE
  ) %>% 
  mutate(investment.growth = investment.growth * 10000)
portfolio_growth_monthly %>% 
  ggplot(aes(x = date, y = investment.growth)) + 
  geom_line(size = 1, color = palette_light()[3]) +
  labs(x = "", y = "Portfolio Value",
       title = "Portfolio Growth",
       caption = "Now we can really visualize performance!") + 
  geom_smooth(method = "loess") + 
  theme_tq(base_family = enfont) + 
  scale_color_tq() + 
  scale_y_continuous(labels = scales::dollar)

ggsave("tidyquantperformance2.svg")

# 下面再比较三个投资组合的增长情况
portfolio_growth_monthly_multi <- stock_returns_monthly_multi %>% 
  tq_portfolio(
    assets_col = symbol,
    returns_col = ra,
    weights = weights_table,
    col_rename = "investment.growth",
    wealth.index = TRUE
  ) %>% 
  mutate(investment.growth = investment.growth * 10000)
portfolio_growth_monthly_multi %>% 
  ggplot(aes(x = date, y = investment.growth, color = factor(portfolio))) + 
  geom_line(size = 2) + 
  labs(title = "Portfolio Growth",
       subtitle = "Comparing Multiple Portfolios",
       caption = "Portfolio 3 is a Standout!",
       x = "", 
       y = "Portfolio Value",
       color = "Portfolio") + 
  geom_smooth(method = "loess") + 
  theme_tq(base_family = enfont) + 
  scale_color_tq() +
  scale_y_continuous(labels = scales::dollar)

ggsave("tidyquantperformance3.svg")
# Performance Analysis with tidyquant
library(tidyquant)
(ra <- c("000001.SZ", "000002.SZ", "000004.SZ") %>% 
  tq_get(get = "stock.prices",
         from = "2010-01-01",
         to = "2015-12-31") %>% 
  group_by(symbol) %>% 
  tq_transmute(
    select = adjusted,
    mutate_fun = periodReturn,
    period = "monthly",
    col_rename = "ra"
  ))

# 下面我们使用深证100R指数得到基准价格：
library(Tushare)
api = pro_api('fd65bf1ee56215d0ce1a95d1fa3b453d97a1779438aca26c4b7937d1')
(rb <- api(api_name = "index_daily",
    ts_code = "399004.SZ",
    start_date = "20100101",
    end_date = "20151231") %>% 
  as_tibble() %>% 
  transmute(
    date = ymd(trade_date),
    symbol = as.character(ts_code),
    close = as.numeric(close)
  ) %>% 
  tq_transmute(
    select = close,
    mutate_fun = periodReturn,
    period = "monthly",
    col_rename = "rb"
  ))

# 合并两个数据集：
(rarb <- left_join(ra, rb, by = "date"))

# 使用 tq_performance 运行 CAPM 模型
(rarb_capm <- rarb %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.CAPM
  ))

# 我们比较关心增长测度和风险测度
rarb_capm %>% 
  dplyr::select(Alpha, Beta)

# tidyquant的工作流程
# 单独资产分析
# 查看 SharpeRatio 函数的参数：
args(SharpeRatio)

# Step 1A: 获取股票数据
(stock_prices <- c("000001.SZ", "000002.SZ", "000004.SZ") %>% 
  tq_get(
    get = "stock.prices",
    from = "2010-01-01",
    to = "2015-12-31"
  ))


(stock_returns_monthly <- stock_prices %>% 
  group_by(symbol) %>% 
  tq_transmute(
    select = adjusted,
    mutate_fun = periodReturn,
    period = "monthly",
    col_rename = "ra"
  ))

# Step4: 绩效分析
stock_returns_monthly %>% 
  tq_performance(Ra = ra,
                 Rb = NULL,
                 performance_fun = SharpeRatio)

# 修改无风险利率和置信水平
stock_returns_monthly %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = SharpeRatio,
    Rf = 0.03 / 12,
    p = 0.99
  )

# 资产组合的分析
# 组合资产
# 方法1: 使用权重向量
wts <- c(0.5, 0, 0.5)
(portfolio_returns_monthly <- stock_returns_monthly %>% 
  tq_portfolio(
    assets_col = symbol,
    returns_col = ra,
    weights = wts,
    col_rename = "ra"
  ))

# 方法2: 使用资产 + 权重 数据框
(wts_map <- tibble(
  symbols = c('000001.SZ', '000004.SZ'),
  weights = c(0.5, 0.5)
))

stock_returns_monthly %>% 
  tq_portfolio(
    assets_col = symbol,
    returns_col = ra,
    weights = wts_map,
    col_rename = "ra_using_wts_map"
  )

# 合并ra和rb
(rarb_single_portfolio <- left_join(portfolio_returns_monthly,
                                   rb,
                                   by = "date"))
# 计算 CAPM 模型
rarb_single_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.CAPM
  )

# 多个资产组合
(stock_returns_monthly_multi <- stock_returns_monthly %>% 
  tq_repeat_df(n = 3))
weights <- c(
  0.5, 0.25, 0.25,
  0.25, 0.5, 0.25,
  0.25, 0.25, 0.5
)
stocks = c('000001.SZ', '000002.SZ', '000004.SZ')
(weights_table <- tibble(stocks) %>% 
  tq_repeat_df(3) %>% 
  bind_cols(tibble(weights)) %>% 
  group_by(portfolio))
(portfolio_returns_monthly_multi <- stock_returns_monthly_multi %>% 
  tq_portfolio(assets_col = symbol,
               returns_col = ra,
               weights = weights_table,
               col_rename = 'ra'))
(rarb_multi_portfolio <- left_join(
  portfolio_returns_monthly_multi, rb,
  by = "date"
))

rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.CAPM
  )

rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = SharpeRatio
  )

# 查看所有可以使用的函数
tq_performance_fun_options()

# table.Stats
# 返回基本统计量
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = table.Stats
  )

# table.CAPM
# 返回与CAPM相关的度量
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.CAPM
  )

# table.AnnualizedReturns
# 返回年度回报、标准差和夏普比率
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = table.AnnualizedReturns
  )

# table.Correlation
# 返回与基准收益的回报率
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.Correlation
  )

# table.DownsideRisk
# 返回下行风险表，以便在多个工具或基金之间进行比较
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.DownsideRisk
  )

# table.DownsideRiskRatio
# 返回每月下行风险、年度下行风险等指标
rarb_multi_portfolio %>% 
  tq_performance(Ra = ra,
                 Rb = NULL,
                 performance_fun = table.DownsideRiskRatio)

# table.HigherMoments
# 返回分布的高阶矩和协矩
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.HigherMoments
  )

# table.InformationRatio
# 返回追踪误差表、年度追踪误差和信息比率
rarb_multi_portfolio %>% 
  tq_performance(Ra = ra,
                 Rb = rb,
                 performance_fun = table.InformationRatio)
# table.Variability
# 返回平均绝对差异、月标准差和年标准差
rarb_multi_portfolio %>% 
  tq_performance(Ra = ra,
                 Rb = NULL,
                 performance_fun = table.Variability)

# 在险值
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = VaR
  )

# 夏普比率
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = SharpeRatio
  )

# 自定义tq_portfolio
portfolio_returns_monthly %>% 
  ggplot(aes(x = date, y = ra)) + 
  geom_bar(stat = "identity", fill = palette_light()[[3]],
           width = 20) + 
  labs(title = "Portfolio Returns",
       caption = "Shows an above-zero trend meaning positive returns",
       x = "", y = "Monthly Returns") + 
  geom_smooth(method = "lm") + 
  theme_tq(base_family = enfont) + 
  scale_color_tq() + 
  scale_y_continuous(labels = scales::percent)

ggsave("tidyquantperformance1.svg")

# 使用 wealth.index = TRUE 参数，我们可以观察10000元是如何增长的：
wts <- c(0.5, 0, 0.5)
portfolio_growth_monthly <- stock_returns_monthly %>% 
  tq_portfolio(
    assets_col = symbol,
    returns_col = ra,
    weights = wts,
    col_rename = "investment.growth",
    wealth.index = TRUE
  ) %>% 
  mutate(investment.growth = investment.growth * 10000)
portfolio_growth_monthly %>% 
  ggplot(aes(x = date, y = investment.growth)) + 
  geom_line(size = 1, color = palette_light()[3]) +
  labs(x = "", y = "Portfolio Value",
       title = "Portfolio Growth",
       caption = "Now we can really visualize performance!") + 
  geom_smooth(method = "loess") + 
  theme_tq(base_family = enfont) + 
  scale_color_tq() + 
  scale_y_continuous(labels = scales::dollar)

ggsave("tidyquantperformance2.svg")

# 下面再比较三个投资组合的增长情况
portfolio_growth_monthly_multi <- stock_returns_monthly_multi %>% 
  tq_portfolio(
    assets_col = symbol,
    returns_col = ra,
    weights = weights_table,
    col_rename = "investment.growth",
    wealth.index = TRUE
  ) %>% 
  mutate(investment.growth = investment.growth * 10000)
portfolio_growth_monthly_multi %>% 
  ggplot(aes(x = date, y = investment.growth, color = factor(portfolio))) + 
  geom_line(size = 2) + 
  labs(title = "Portfolio Growth",
       subtitle = "Comparing Multiple Portfolios",
       caption = "Portfolio 3 is a Standout!",
       x = "", 
       y = "Portfolio Value",
       color = "Portfolio") + 
  geom_smooth(method = "loess") + 
  theme_tq(base_family = enfont) + 
  scale_color_tq() +
  scale_y_continuous(labels = scales::dollar)

ggsave("tidyquantperformance3.svg")
# Performance Analysis with tidyquant
library(tidyquant)
(ra <- c("000001.SZ", "000002.SZ", "000004.SZ") %>% 
  tq_get(get = "stock.prices",
         from = "2010-01-01",
         to = "2015-12-31") %>% 
  group_by(symbol) %>% 
  tq_transmute(
    select = adjusted,
    mutate_fun = periodReturn,
    period = "monthly",
    col_rename = "ra"
  ))

# 下面我们使用深证100R指数得到基准价格：
library(Tushare)
api = pro_api('fd65bf1ee56215d0ce1a95d1fa3b453d97a1779438aca26c4b7937d1')
(rb <- api(api_name = "index_daily",
    ts_code = "399004.SZ",
    start_date = "20100101",
    end_date = "20151231") %>% 
  as_tibble() %>% 
  transmute(
    date = ymd(trade_date),
    symbol = as.character(ts_code),
    close = as.numeric(close)
  ) %>% 
  tq_transmute(
    select = close,
    mutate_fun = periodReturn,
    period = "monthly",
    col_rename = "rb"
  ))

# 合并两个数据集：
(rarb <- left_join(ra, rb, by = "date"))

# 使用 tq_performance 运行 CAPM 模型
(rarb_capm <- rarb %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.CAPM
  ))

# 我们比较关心增长测度和风险测度
rarb_capm %>% 
  dplyr::select(Alpha, Beta)

# tidyquant的工作流程
# 单独资产分析
# 查看 SharpeRatio 函数的参数：
args(SharpeRatio)

# Step 1A: 获取股票数据
(stock_prices <- c("000001.SZ", "000002.SZ", "000004.SZ") %>% 
  tq_get(
    get = "stock.prices",
    from = "2010-01-01",
    to = "2015-12-31"
  ))


(stock_returns_monthly <- stock_prices %>% 
  group_by(symbol) %>% 
  tq_transmute(
    select = adjusted,
    mutate_fun = periodReturn,
    period = "monthly",
    col_rename = "ra"
  ))

# Step4: 绩效分析
stock_returns_monthly %>% 
  tq_performance(Ra = ra,
                 Rb = NULL,
                 performance_fun = SharpeRatio)

# 修改无风险利率和置信水平
stock_returns_monthly %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = SharpeRatio,
    Rf = 0.03 / 12,
    p = 0.99
  )

# 资产组合的分析
# 组合资产
# 方法1: 使用权重向量
wts <- c(0.5, 0, 0.5)
(portfolio_returns_monthly <- stock_returns_monthly %>% 
  tq_portfolio(
    assets_col = symbol,
    returns_col = ra,
    weights = wts,
    col_rename = "ra"
  ))

# 方法2: 使用资产 + 权重 数据框
(wts_map <- tibble(
  symbols = c('000001.SZ', '000004.SZ'),
  weights = c(0.5, 0.5)
))

stock_returns_monthly %>% 
  tq_portfolio(
    assets_col = symbol,
    returns_col = ra,
    weights = wts_map,
    col_rename = "ra_using_wts_map"
  )

# 合并ra和rb
(rarb_single_portfolio <- left_join(portfolio_returns_monthly,
                                   rb,
                                   by = "date"))
# 计算 CAPM 模型
rarb_single_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.CAPM
  )

# 多个资产组合
(stock_returns_monthly_multi <- stock_returns_monthly %>% 
  tq_repeat_df(n = 3))
weights <- c(
  0.5, 0.25, 0.25,
  0.25, 0.5, 0.25,
  0.25, 0.25, 0.5
)
stocks = c('000001.SZ', '000002.SZ', '000004.SZ')
(weights_table <- tibble(stocks) %>% 
  tq_repeat_df(3) %>% 
  bind_cols(tibble(weights)) %>% 
  group_by(portfolio))
(portfolio_returns_monthly_multi <- stock_returns_monthly_multi %>% 
  tq_portfolio(assets_col = symbol,
               returns_col = ra,
               weights = weights_table,
               col_rename = 'ra'))
(rarb_multi_portfolio <- left_join(
  portfolio_returns_monthly_multi, rb,
  by = "date"
))

rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.CAPM
  )

rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = SharpeRatio
  )

# 查看所有可以使用的函数
tq_performance_fun_options()

# table.Stats
# 返回基本统计量
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = table.Stats
  )

# table.CAPM
# 返回与CAPM相关的度量
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.CAPM
  )

# table.AnnualizedReturns
# 返回年度回报、标准差和夏普比率
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = table.AnnualizedReturns
  )

# table.Correlation
# 返回与基准收益的回报率
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.Correlation
  )

# table.DownsideRisk
# 返回下行风险表，以便在多个工具或基金之间进行比较
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.DownsideRisk
  )

# table.DownsideRiskRatio
# 返回每月下行风险、年度下行风险等指标
rarb_multi_portfolio %>% 
  tq_performance(Ra = ra,
                 Rb = NULL,
                 performance_fun = table.DownsideRiskRatio)

# table.HigherMoments
# 返回分布的高阶矩和协矩
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.HigherMoments
  )

# table.InformationRatio
# 返回追踪误差表、年度追踪误差和信息比率
rarb_multi_portfolio %>% 
  tq_performance(Ra = ra,
                 Rb = rb,
                 performance_fun = table.InformationRatio)
# table.Variability
# 返回平均绝对差异、月标准差和年标准差
rarb_multi_portfolio %>% 
  tq_performance(Ra = ra,
                 Rb = NULL,
                 performance_fun = table.Variability)

# 在险值
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = VaR
  )

# 夏普比率
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = SharpeRatio
  )

# 自定义tq_portfolio
portfolio_returns_monthly %>% 
  ggplot(aes(x = date, y = ra)) + 
  geom_bar(stat = "identity", fill = palette_light()[[3]],
           width = 20) + 
  labs(title = "Portfolio Returns",
       caption = "Shows an above-zero trend meaning positive returns",
       x = "", y = "Monthly Returns") + 
  geom_smooth(method = "lm") + 
  theme_tq(base_family = enfont) + 
  scale_color_tq() + 
  scale_y_continuous(labels = scales::percent)

ggsave("tidyquantperformance1.svg")

# 使用 wealth.index = TRUE 参数，我们可以观察10000元是如何增长的：
wts <- c(0.5, 0, 0.5)
portfolio_growth_monthly <- stock_returns_monthly %>% 
  tq_portfolio(
    assets_col = symbol,
    returns_col = ra,
    weights = wts,
    col_rename = "investment.growth",
    wealth.index = TRUE
  ) %>% 
  mutate(investment.growth = investment.growth * 10000)
portfolio_growth_monthly %>% 
  ggplot(aes(x = date, y = investment.growth)) + 
  geom_line(size = 1, color = palette_light()[3]) +
  labs(x = "", y = "Portfolio Value",
       title = "Portfolio Growth",
       caption = "Now we can really visualize performance!") + 
  geom_smooth(method = "loess") + 
  theme_tq(base_family = enfont) + 
  scale_color_tq() + 
  scale_y_continuous(labels = scales::dollar)

ggsave("tidyquantperformance2.svg")

# 下面再比较三个投资组合的增长情况
portfolio_growth_monthly_multi <- stock_returns_monthly_multi %>% 
  tq_portfolio(
    assets_col = symbol,
    returns_col = ra,
    weights = weights_table,
    col_rename = "investment.growth",
    wealth.index = TRUE
  ) %>% 
  mutate(investment.growth = investment.growth * 10000)
portfolio_growth_monthly_multi %>% 
  ggplot(aes(x = date, y = investment.growth, color = factor(portfolio))) + 
  geom_line(size = 2) + 
  labs(title = "Portfolio Growth",
       subtitle = "Comparing Multiple Portfolios",
       caption = "Portfolio 3 is a Standout!",
       x = "", 
       y = "Portfolio Value",
       color = "Portfolio") + 
  geom_smooth(method = "loess") + 
  theme_tq(base_family = enfont) + 
  scale_color_tq() +
  scale_y_continuous(labels = scales::dollar)

ggsave("tidyquantperformance3.svg")
# Performance Analysis with tidyquant
library(tidyquant)
(ra <- c("000001.SZ", "000002.SZ", "000004.SZ") %>% 
  tq_get(get = "stock.prices",
         from = "2010-01-01",
         to = "2015-12-31") %>% 
  group_by(symbol) %>% 
  tq_transmute(
    select = adjusted,
    mutate_fun = periodReturn,
    period = "monthly",
    col_rename = "ra"
  ))

# 下面我们使用深证100R指数得到基准价格：
library(Tushare)
api = pro_api('fd65bf1ee56215d0ce1a95d1fa3b453d97a1779438aca26c4b7937d1')
(rb <- api(api_name = "index_daily",
    ts_code = "399004.SZ",
    start_date = "20100101",
    end_date = "20151231") %>% 
  as_tibble() %>% 
  transmute(
    date = ymd(trade_date),
    symbol = as.character(ts_code),
    close = as.numeric(close)
  ) %>% 
  tq_transmute(
    select = close,
    mutate_fun = periodReturn,
    period = "monthly",
    col_rename = "rb"
  ))

# 合并两个数据集：
(rarb <- left_join(ra, rb, by = "date"))

# 使用 tq_performance 运行 CAPM 模型
(rarb_capm <- rarb %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.CAPM
  ))

# 我们比较关心增长测度和风险测度
rarb_capm %>% 
  dplyr::select(Alpha, Beta)

# tidyquant的工作流程
# 单独资产分析
# 查看 SharpeRatio 函数的参数：
args(SharpeRatio)

# Step 1A: 获取股票数据
(stock_prices <- c("000001.SZ", "000002.SZ", "000004.SZ") %>% 
  tq_get(
    get = "stock.prices",
    from = "2010-01-01",
    to = "2015-12-31"
  ))


(stock_returns_monthly <- stock_prices %>% 
  group_by(symbol) %>% 
  tq_transmute(
    select = adjusted,
    mutate_fun = periodReturn,
    period = "monthly",
    col_rename = "ra"
  ))

# Step4: 绩效分析
stock_returns_monthly %>% 
  tq_performance(Ra = ra,
                 Rb = NULL,
                 performance_fun = SharpeRatio)

# 修改无风险利率和置信水平
stock_returns_monthly %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = SharpeRatio,
    Rf = 0.03 / 12,
    p = 0.99
  )

# 资产组合的分析
# 组合资产
# 方法1: 使用权重向量
wts <- c(0.5, 0, 0.5)
(portfolio_returns_monthly <- stock_returns_monthly %>% 
  tq_portfolio(
    assets_col = symbol,
    returns_col = ra,
    weights = wts,
    col_rename = "ra"
  ))

# 方法2: 使用资产 + 权重 数据框
(wts_map <- tibble(
  symbols = c('000001.SZ', '000004.SZ'),
  weights = c(0.5, 0.5)
))

stock_returns_monthly %>% 
  tq_portfolio(
    assets_col = symbol,
    returns_col = ra,
    weights = wts_map,
    col_rename = "ra_using_wts_map"
  )

# 合并ra和rb
(rarb_single_portfolio <- left_join(portfolio_returns_monthly,
                                   rb,
                                   by = "date"))
# 计算 CAPM 模型
rarb_single_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.CAPM
  )

# 多个资产组合
(stock_returns_monthly_multi <- stock_returns_monthly %>% 
  tq_repeat_df(n = 3))
weights <- c(
  0.5, 0.25, 0.25,
  0.25, 0.5, 0.25,
  0.25, 0.25, 0.5
)
stocks = c('000001.SZ', '000002.SZ', '000004.SZ')
(weights_table <- tibble(stocks) %>% 
  tq_repeat_df(3) %>% 
  bind_cols(tibble(weights)) %>% 
  group_by(portfolio))
(portfolio_returns_monthly_multi <- stock_returns_monthly_multi %>% 
  tq_portfolio(assets_col = symbol,
               returns_col = ra,
               weights = weights_table,
               col_rename = 'ra'))
(rarb_multi_portfolio <- left_join(
  portfolio_returns_monthly_multi, rb,
  by = "date"
))

rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.CAPM
  )

rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = SharpeRatio
  )

# 查看所有可以使用的函数
tq_performance_fun_options()

# table.Stats
# 返回基本统计量
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = table.Stats
  )

# table.CAPM
# 返回与CAPM相关的度量
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.CAPM
  )

# table.AnnualizedReturns
# 返回年度回报、标准差和夏普比率
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = table.AnnualizedReturns
  )

# table.Correlation
# 返回与基准收益的回报率
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.Correlation
  )

# table.DownsideRisk
# 返回下行风险表，以便在多个工具或基金之间进行比较
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.DownsideRisk
  )

# table.DownsideRiskRatio
# 返回每月下行风险、年度下行风险等指标
rarb_multi_portfolio %>% 
  tq_performance(Ra = ra,
                 Rb = NULL,
                 performance_fun = table.DownsideRiskRatio)

# table.HigherMoments
# 返回分布的高阶矩和协矩
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.HigherMoments
  )

# table.InformationRatio
# 返回追踪误差表、年度追踪误差和信息比率
rarb_multi_portfolio %>% 
  tq_performance(Ra = ra,
                 Rb = rb,
                 performance_fun = table.InformationRatio)
# table.Variability
# 返回平均绝对差异、月标准差和年标准差
rarb_multi_portfolio %>% 
  tq_performance(Ra = ra,
                 Rb = NULL,
                 performance_fun = table.Variability)

# 在险值
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = VaR
  )

# 夏普比率
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = SharpeRatio
  )

# 自定义tq_portfolio
portfolio_returns_monthly %>% 
  ggplot(aes(x = date, y = ra)) + 
  geom_bar(stat = "identity", fill = palette_light()[[3]],
           width = 20) + 
  labs(title = "Portfolio Returns",
       caption = "Shows an above-zero trend meaning positive returns",
       x = "", y = "Monthly Returns") + 
  geom_smooth(method = "lm") + 
  theme_tq(base_family = enfont) + 
  scale_color_tq() + 
  scale_y_continuous(labels = scales::percent)

ggsave("tidyquantperformance1.svg")

# 使用 wealth.index = TRUE 参数，我们可以观察10000元是如何增长的：
wts <- c(0.5, 0, 0.5)
portfolio_growth_monthly <- stock_returns_monthly %>% 
  tq_portfolio(
    assets_col = symbol,
    returns_col = ra,
    weights = wts,
    col_rename = "investment.growth",
    wealth.index = TRUE
  ) %>% 
  mutate(investment.growth = investment.growth * 10000)
portfolio_growth_monthly %>% 
  ggplot(aes(x = date, y = investment.growth)) + 
  geom_line(size = 1, color = palette_light()[3]) +
  labs(x = "", y = "Portfolio Value",
       title = "Portfolio Growth",
       caption = "Now we can really visualize performance!") + 
  geom_smooth(method = "loess") + 
  theme_tq(base_family = enfont) + 
  scale_color_tq() + 
  scale_y_continuous(labels = scales::dollar)

ggsave("tidyquantperformance2.svg")

# 下面再比较三个投资组合的增长情况
portfolio_growth_monthly_multi <- stock_returns_monthly_multi %>% 
  tq_portfolio(
    assets_col = symbol,
    returns_col = ra,
    weights = weights_table,
    col_rename = "investment.growth",
    wealth.index = TRUE
  ) %>% 
  mutate(investment.growth = investment.growth * 10000)
portfolio_growth_monthly_multi %>% 
  ggplot(aes(x = date, y = investment.growth, color = factor(portfolio))) + 
  geom_line(size = 2) + 
  labs(title = "Portfolio Growth",
       subtitle = "Comparing Multiple Portfolios",
       caption = "Portfolio 3 is a Standout!",
       x = "", 
       y = "Portfolio Value",
       color = "Portfolio") + 
  geom_smooth(method = "loess") + 
  theme_tq(base_family = enfont) + 
  scale_color_tq() +
  scale_y_continuous(labels = scales::dollar)

ggsave("tidyquantperformance3.svg")
# Performance Analysis with tidyquant
library(tidyquant)
(ra <- c("000001.SZ", "000002.SZ", "000004.SZ") %>% 
  tq_get(get = "stock.prices",
         from = "2010-01-01",
         to = "2015-12-31") %>% 
  group_by(symbol) %>% 
  tq_transmute(
    select = adjusted,
    mutate_fun = periodReturn,
    period = "monthly",
    col_rename = "ra"
  ))

# 下面我们使用深证100R指数得到基准价格：
library(Tushare)
api = pro_api('fd65bf1ee56215d0ce1a95d1fa3b453d97a1779438aca26c4b7937d1')
(rb <- api(api_name = "index_daily",
    ts_code = "399004.SZ",
    start_date = "20100101",
    end_date = "20151231") %>% 
  as_tibble() %>% 
  transmute(
    date = ymd(trade_date),
    symbol = as.character(ts_code),
    close = as.numeric(close)
  ) %>% 
  tq_transmute(
    select = close,
    mutate_fun = periodReturn,
    period = "monthly",
    col_rename = "rb"
  ))

# 合并两个数据集：
(rarb <- left_join(ra, rb, by = "date"))

# 使用 tq_performance 运行 CAPM 模型
(rarb_capm <- rarb %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.CAPM
  ))

# 我们比较关心增长测度和风险测度
rarb_capm %>% 
  dplyr::select(Alpha, Beta)

# tidyquant的工作流程
# 单独资产分析
# 查看 SharpeRatio 函数的参数：
args(SharpeRatio)

# Step 1A: 获取股票数据
(stock_prices <- c("000001.SZ", "000002.SZ", "000004.SZ") %>% 
  tq_get(
    get = "stock.prices",
    from = "2010-01-01",
    to = "2015-12-31"
  ))


(stock_returns_monthly <- stock_prices %>% 
  group_by(symbol) %>% 
  tq_transmute(
    select = adjusted,
    mutate_fun = periodReturn,
    period = "monthly",
    col_rename = "ra"
  ))

# Step4: 绩效分析
stock_returns_monthly %>% 
  tq_performance(Ra = ra,
                 Rb = NULL,
                 performance_fun = SharpeRatio)

# 修改无风险利率和置信水平
stock_returns_monthly %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = SharpeRatio,
    Rf = 0.03 / 12,
    p = 0.99
  )

# 资产组合的分析
# 组合资产
# 方法1: 使用权重向量
wts <- c(0.5, 0, 0.5)
(portfolio_returns_monthly <- stock_returns_monthly %>% 
  tq_portfolio(
    assets_col = symbol,
    returns_col = ra,
    weights = wts,
    col_rename = "ra"
  ))

# 方法2: 使用资产 + 权重 数据框
(wts_map <- tibble(
  symbols = c('000001.SZ', '000004.SZ'),
  weights = c(0.5, 0.5)
))

stock_returns_monthly %>% 
  tq_portfolio(
    assets_col = symbol,
    returns_col = ra,
    weights = wts_map,
    col_rename = "ra_using_wts_map"
  )

# 合并ra和rb
(rarb_single_portfolio <- left_join(portfolio_returns_monthly,
                                   rb,
                                   by = "date"))
# 计算 CAPM 模型
rarb_single_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.CAPM
  )

# 多个资产组合
(stock_returns_monthly_multi <- stock_returns_monthly %>% 
  tq_repeat_df(n = 3))
weights <- c(
  0.5, 0.25, 0.25,
  0.25, 0.5, 0.25,
  0.25, 0.25, 0.5
)
stocks = c('000001.SZ', '000002.SZ', '000004.SZ')
(weights_table <- tibble(stocks) %>% 
  tq_repeat_df(3) %>% 
  bind_cols(tibble(weights)) %>% 
  group_by(portfolio))
(portfolio_returns_monthly_multi <- stock_returns_monthly_multi %>% 
  tq_portfolio(assets_col = symbol,
               returns_col = ra,
               weights = weights_table,
               col_rename = 'ra'))
(rarb_multi_portfolio <- left_join(
  portfolio_returns_monthly_multi, rb,
  by = "date"
))

rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.CAPM
  )

rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = SharpeRatio
  )

# 查看所有可以使用的函数
tq_performance_fun_options()

# table.Stats
# 返回基本统计量
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = table.Stats
  )

# table.CAPM
# 返回与CAPM相关的度量
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.CAPM
  )

# table.AnnualizedReturns
# 返回年度回报、标准差和夏普比率
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = table.AnnualizedReturns
  )

# table.Correlation
# 返回与基准收益的回报率
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.Correlation
  )

# table.DownsideRisk
# 返回下行风险表，以便在多个工具或基金之间进行比较
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.DownsideRisk
  )

# table.DownsideRiskRatio
# 返回每月下行风险、年度下行风险等指标
rarb_multi_portfolio %>% 
  tq_performance(Ra = ra,
                 Rb = NULL,
                 performance_fun = table.DownsideRiskRatio)

# table.HigherMoments
# 返回分布的高阶矩和协矩
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.HigherMoments
  )

# table.InformationRatio
# 返回追踪误差表、年度追踪误差和信息比率
rarb_multi_portfolio %>% 
  tq_performance(Ra = ra,
                 Rb = rb,
                 performance_fun = table.InformationRatio)
# table.Variability
# 返回平均绝对差异、月标准差和年标准差
rarb_multi_portfolio %>% 
  tq_performance(Ra = ra,
                 Rb = NULL,
                 performance_fun = table.Variability)

# 在险值
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = VaR
  )

# 夏普比率
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = SharpeRatio
  )

# 自定义tq_portfolio
portfolio_returns_monthly %>% 
  ggplot(aes(x = date, y = ra)) + 
  geom_bar(stat = "identity", fill = palette_light()[[3]],
           width = 20) + 
  labs(title = "Portfolio Returns",
       caption = "Shows an above-zero trend meaning positive returns",
       x = "", y = "Monthly Returns") + 
  geom_smooth(method = "lm") + 
  theme_tq(base_family = enfont) + 
  scale_color_tq() + 
  scale_y_continuous(labels = scales::percent)

ggsave("tidyquantperformance1.svg")

# 使用 wealth.index = TRUE 参数，我们可以观察10000元是如何增长的：
wts <- c(0.5, 0, 0.5)
portfolio_growth_monthly <- stock_returns_monthly %>% 
  tq_portfolio(
    assets_col = symbol,
    returns_col = ra,
    weights = wts,
    col_rename = "investment.growth",
    wealth.index = TRUE
  ) %>% 
  mutate(investment.growth = investment.growth * 10000)
portfolio_growth_monthly %>% 
  ggplot(aes(x = date, y = investment.growth)) + 
  geom_line(size = 1, color = palette_light()[3]) +
  labs(x = "", y = "Portfolio Value",
       title = "Portfolio Growth",
       caption = "Now we can really visualize performance!") + 
  geom_smooth(method = "loess") + 
  theme_tq(base_family = enfont) + 
  scale_color_tq() + 
  scale_y_continuous(labels = scales::dollar)

ggsave("tidyquantperformance2.svg")

# 下面再比较三个投资组合的增长情况
portfolio_growth_monthly_multi <- stock_returns_monthly_multi %>% 
  tq_portfolio(
    assets_col = symbol,
    returns_col = ra,
    weights = weights_table,
    col_rename = "investment.growth",
    wealth.index = TRUE
  ) %>% 
  mutate(investment.growth = investment.growth * 10000)
portfolio_growth_monthly_multi %>% 
  ggplot(aes(x = date, y = investment.growth, color = factor(portfolio))) + 
  geom_line(size = 2) + 
  labs(title = "Portfolio Growth",
       subtitle = "Comparing Multiple Portfolios",
       caption = "Portfolio 3 is a Standout!",
       x = "", 
       y = "Portfolio Value",
       color = "Portfolio") + 
  geom_smooth(method = "loess") + 
  theme_tq(base_family = enfont) + 
  scale_color_tq() +
  scale_y_continuous(labels = scales::dollar)

ggsave("tidyquantperformance3.svg")
# Performance Analysis with tidyquant
library(tidyquant)
(ra <- c("000001.SZ", "000002.SZ", "000004.SZ") %>% 
  tq_get(get = "stock.prices",
         from = "2010-01-01",
         to = "2015-12-31") %>% 
  group_by(symbol) %>% 
  tq_transmute(
    select = adjusted,
    mutate_fun = periodReturn,
    period = "monthly",
    col_rename = "ra"
  ))

# 下面我们使用深证100R指数得到基准价格：
library(Tushare)
api = pro_api('fd65bf1ee56215d0ce1a95d1fa3b453d97a1779438aca26c4b7937d1')
(rb <- api(api_name = "index_daily",
    ts_code = "399004.SZ",
    start_date = "20100101",
    end_date = "20151231") %>% 
  as_tibble() %>% 
  transmute(
    date = ymd(trade_date),
    symbol = as.character(ts_code),
    close = as.numeric(close)
  ) %>% 
  tq_transmute(
    select = close,
    mutate_fun = periodReturn,
    period = "monthly",
    col_rename = "rb"
  ))

# 合并两个数据集：
(rarb <- left_join(ra, rb, by = "date"))

# 使用 tq_performance 运行 CAPM 模型
(rarb_capm <- rarb %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.CAPM
  ))

# 我们比较关心增长测度和风险测度
rarb_capm %>% 
  dplyr::select(Alpha, Beta)

# tidyquant的工作流程
# 单独资产分析
# 查看 SharpeRatio 函数的参数：
args(SharpeRatio)

# Step 1A: 获取股票数据
(stock_prices <- c("000001.SZ", "000002.SZ", "000004.SZ") %>% 
  tq_get(
    get = "stock.prices",
    from = "2010-01-01",
    to = "2015-12-31"
  ))


(stock_returns_monthly <- stock_prices %>% 
  group_by(symbol) %>% 
  tq_transmute(
    select = adjusted,
    mutate_fun = periodReturn,
    period = "monthly",
    col_rename = "ra"
  ))

# Step4: 绩效分析
stock_returns_monthly %>% 
  tq_performance(Ra = ra,
                 Rb = NULL,
                 performance_fun = SharpeRatio)

# 修改无风险利率和置信水平
stock_returns_monthly %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = SharpeRatio,
    Rf = 0.03 / 12,
    p = 0.99
  )

# 资产组合的分析
# 组合资产
# 方法1: 使用权重向量
wts <- c(0.5, 0, 0.5)
(portfolio_returns_monthly <- stock_returns_monthly %>% 
  tq_portfolio(
    assets_col = symbol,
    returns_col = ra,
    weights = wts,
    col_rename = "ra"
  ))

# 方法2: 使用资产 + 权重 数据框
(wts_map <- tibble(
  symbols = c('000001.SZ', '000004.SZ'),
  weights = c(0.5, 0.5)
))

stock_returns_monthly %>% 
  tq_portfolio(
    assets_col = symbol,
    returns_col = ra,
    weights = wts_map,
    col_rename = "ra_using_wts_map"
  )

# 合并ra和rb
(rarb_single_portfolio <- left_join(portfolio_returns_monthly,
                                   rb,
                                   by = "date"))
# 计算 CAPM 模型
rarb_single_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.CAPM
  )

# 多个资产组合
(stock_returns_monthly_multi <- stock_returns_monthly %>% 
  tq_repeat_df(n = 3))
weights <- c(
  0.5, 0.25, 0.25,
  0.25, 0.5, 0.25,
  0.25, 0.25, 0.5
)
stocks = c('000001.SZ', '000002.SZ', '000004.SZ')
(weights_table <- tibble(stocks) %>% 
  tq_repeat_df(3) %>% 
  bind_cols(tibble(weights)) %>% 
  group_by(portfolio))
(portfolio_returns_monthly_multi <- stock_returns_monthly_multi %>% 
  tq_portfolio(assets_col = symbol,
               returns_col = ra,
               weights = weights_table,
               col_rename = 'ra'))
(rarb_multi_portfolio <- left_join(
  portfolio_returns_monthly_multi, rb,
  by = "date"
))

rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.CAPM
  )

rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = SharpeRatio
  )

# 查看所有可以使用的函数
tq_performance_fun_options()

# table.Stats
# 返回基本统计量
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = table.Stats
  )

# table.CAPM
# 返回与CAPM相关的度量
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.CAPM
  )

# table.AnnualizedReturns
# 返回年度回报、标准差和夏普比率
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = table.AnnualizedReturns
  )

# table.Correlation
# 返回与基准收益的回报率
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.Correlation
  )

# table.DownsideRisk
# 返回下行风险表，以便在多个工具或基金之间进行比较
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.DownsideRisk
  )

# table.DownsideRiskRatio
# 返回每月下行风险、年度下行风险等指标
rarb_multi_portfolio %>% 
  tq_performance(Ra = ra,
                 Rb = NULL,
                 performance_fun = table.DownsideRiskRatio)

# table.HigherMoments
# 返回分布的高阶矩和协矩
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.HigherMoments
  )

# table.InformationRatio
# 返回追踪误差表、年度追踪误差和信息比率
rarb_multi_portfolio %>% 
  tq_performance(Ra = ra,
                 Rb = rb,
                 performance_fun = table.InformationRatio)
# table.Variability
# 返回平均绝对差异、月标准差和年标准差
rarb_multi_portfolio %>% 
  tq_performance(Ra = ra,
                 Rb = NULL,
                 performance_fun = table.Variability)

# 在险值
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = VaR
  )

# 夏普比率
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = SharpeRatio
  )

# 自定义tq_portfolio
portfolio_returns_monthly %>% 
  ggplot(aes(x = date, y = ra)) + 
  geom_bar(stat = "identity", fill = palette_light()[[3]],
           width = 20) + 
  labs(title = "Portfolio Returns",
       caption = "Shows an above-zero trend meaning positive returns",
       x = "", y = "Monthly Returns") + 
  geom_smooth(method = "lm") + 
  theme_tq(base_family = enfont) + 
  scale_color_tq() + 
  scale_y_continuous(labels = scales::percent)

ggsave("tidyquantperformance1.svg")

# 使用 wealth.index = TRUE 参数，我们可以观察10000元是如何增长的：
wts <- c(0.5, 0, 0.5)
portfolio_growth_monthly <- stock_returns_monthly %>% 
  tq_portfolio(
    assets_col = symbol,
    returns_col = ra,
    weights = wts,
    col_rename = "investment.growth",
    wealth.index = TRUE
  ) %>% 
  mutate(investment.growth = investment.growth * 10000)
portfolio_growth_monthly %>% 
  ggplot(aes(x = date, y = investment.growth)) + 
  geom_line(size = 1, color = palette_light()[3]) +
  labs(x = "", y = "Portfolio Value",
       title = "Portfolio Growth",
       caption = "Now we can really visualize performance!") + 
  geom_smooth(method = "loess") + 
  theme_tq(base_family = enfont) + 
  scale_color_tq() + 
  scale_y_continuous(labels = scales::dollar)

ggsave("tidyquantperformance2.svg")

# 下面再比较三个投资组合的增长情况
portfolio_growth_monthly_multi <- stock_returns_monthly_multi %>% 
  tq_portfolio(
    assets_col = symbol,
    returns_col = ra,
    weights = weights_table,
    col_rename = "investment.growth",
    wealth.index = TRUE
  ) %>% 
  mutate(investment.growth = investment.growth * 10000)
portfolio_growth_monthly_multi %>% 
  ggplot(aes(x = date, y = investment.growth, color = factor(portfolio))) + 
  geom_line(size = 2) + 
  labs(title = "Portfolio Growth",
       subtitle = "Comparing Multiple Portfolios",
       caption = "Portfolio 3 is a Standout!",
       x = "", 
       y = "Portfolio Value",
       color = "Portfolio") + 
  geom_smooth(method = "loess") + 
  theme_tq(base_family = enfont) + 
  scale_color_tq() +
  scale_y_continuous(labels = scales::dollar)

ggsave("tidyquantperformance3.svg")
# Performance Analysis with tidyquant
library(tidyquant)
(ra <- c("000001.SZ", "000002.SZ", "000004.SZ") %>% 
  tq_get(get = "stock.prices",
         from = "2010-01-01",
         to = "2015-12-31") %>% 
  group_by(symbol) %>% 
  tq_transmute(
    select = adjusted,
    mutate_fun = periodReturn,
    period = "monthly",
    col_rename = "ra"
  ))

# 下面我们使用深证100R指数得到基准价格：
library(Tushare)
api = pro_api('fd65bf1ee56215d0ce1a95d1fa3b453d97a1779438aca26c4b7937d1')
(rb <- api(api_name = "index_daily",
    ts_code = "399004.SZ",
    start_date = "20100101",
    end_date = "20151231") %>% 
  as_tibble() %>% 
  transmute(
    date = ymd(trade_date),
    symbol = as.character(ts_code),
    close = as.numeric(close)
  ) %>% 
  tq_transmute(
    select = close,
    mutate_fun = periodReturn,
    period = "monthly",
    col_rename = "rb"
  ))

# 合并两个数据集：
(rarb <- left_join(ra, rb, by = "date"))

# 使用 tq_performance 运行 CAPM 模型
(rarb_capm <- rarb %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.CAPM
  ))

# 我们比较关心增长测度和风险测度
rarb_capm %>% 
  dplyr::select(Alpha, Beta)

# tidyquant的工作流程
# 单独资产分析
# 查看 SharpeRatio 函数的参数：
args(SharpeRatio)

# Step 1A: 获取股票数据
(stock_prices <- c("000001.SZ", "000002.SZ", "000004.SZ") %>% 
  tq_get(
    get = "stock.prices",
    from = "2010-01-01",
    to = "2015-12-31"
  ))


(stock_returns_monthly <- stock_prices %>% 
  group_by(symbol) %>% 
  tq_transmute(
    select = adjusted,
    mutate_fun = periodReturn,
    period = "monthly",
    col_rename = "ra"
  ))

# Step4: 绩效分析
stock_returns_monthly %>% 
  tq_performance(Ra = ra,
                 Rb = NULL,
                 performance_fun = SharpeRatio)

# 修改无风险利率和置信水平
stock_returns_monthly %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = SharpeRatio,
    Rf = 0.03 / 12,
    p = 0.99
  )

# 资产组合的分析
# 组合资产
# 方法1: 使用权重向量
wts <- c(0.5, 0, 0.5)
(portfolio_returns_monthly <- stock_returns_monthly %>% 
  tq_portfolio(
    assets_col = symbol,
    returns_col = ra,
    weights = wts,
    col_rename = "ra"
  ))

# 方法2: 使用资产 + 权重 数据框
(wts_map <- tibble(
  symbols = c('000001.SZ', '000004.SZ'),
  weights = c(0.5, 0.5)
))

stock_returns_monthly %>% 
  tq_portfolio(
    assets_col = symbol,
    returns_col = ra,
    weights = wts_map,
    col_rename = "ra_using_wts_map"
  )

# 合并ra和rb
(rarb_single_portfolio <- left_join(portfolio_returns_monthly,
                                   rb,
                                   by = "date"))
# 计算 CAPM 模型
rarb_single_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.CAPM
  )

# 多个资产组合
(stock_returns_monthly_multi <- stock_returns_monthly %>% 
  tq_repeat_df(n = 3))
weights <- c(
  0.5, 0.25, 0.25,
  0.25, 0.5, 0.25,
  0.25, 0.25, 0.5
)
stocks = c('000001.SZ', '000002.SZ', '000004.SZ')
(weights_table <- tibble(stocks) %>% 
  tq_repeat_df(3) %>% 
  bind_cols(tibble(weights)) %>% 
  group_by(portfolio))
(portfolio_returns_monthly_multi <- stock_returns_monthly_multi %>% 
  tq_portfolio(assets_col = symbol,
               returns_col = ra,
               weights = weights_table,
               col_rename = 'ra'))
(rarb_multi_portfolio <- left_join(
  portfolio_returns_monthly_multi, rb,
  by = "date"
))

rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.CAPM
  )

rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = SharpeRatio
  )

# 查看所有可以使用的函数
tq_performance_fun_options()

# table.Stats
# 返回基本统计量
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = table.Stats
  )

# table.CAPM
# 返回与CAPM相关的度量
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.CAPM
  )

# table.AnnualizedReturns
# 返回年度回报、标准差和夏普比率
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = table.AnnualizedReturns
  )

# table.Correlation
# 返回与基准收益的回报率
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.Correlation
  )

# table.DownsideRisk
# 返回下行风险表，以便在多个工具或基金之间进行比较
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.DownsideRisk
  )

# table.DownsideRiskRatio
# 返回每月下行风险、年度下行风险等指标
rarb_multi_portfolio %>% 
  tq_performance(Ra = ra,
                 Rb = NULL,
                 performance_fun = table.DownsideRiskRatio)

# table.HigherMoments
# 返回分布的高阶矩和协矩
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.HigherMoments
  )

# table.InformationRatio
# 返回追踪误差表、年度追踪误差和信息比率
rarb_multi_portfolio %>% 
  tq_performance(Ra = ra,
                 Rb = rb,
                 performance_fun = table.InformationRatio)
# table.Variability
# 返回平均绝对差异、月标准差和年标准差
rarb_multi_portfolio %>% 
  tq_performance(Ra = ra,
                 Rb = NULL,
                 performance_fun = table.Variability)

# 在险值
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = VaR
  )

# 夏普比率
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = SharpeRatio
  )

# 自定义tq_portfolio
portfolio_returns_monthly %>% 
  ggplot(aes(x = date, y = ra)) + 
  geom_bar(stat = "identity", fill = palette_light()[[3]],
           width = 20) + 
  labs(title = "Portfolio Returns",
       caption = "Shows an above-zero trend meaning positive returns",
       x = "", y = "Monthly Returns") + 
  geom_smooth(method = "lm") + 
  theme_tq(base_family = enfont) + 
  scale_color_tq() + 
  scale_y_continuous(labels = scales::percent)

ggsave("tidyquantperformance1.svg")

# 使用 wealth.index = TRUE 参数，我们可以观察10000元是如何增长的：
wts <- c(0.5, 0, 0.5)
portfolio_growth_monthly <- stock_returns_monthly %>% 
  tq_portfolio(
    assets_col = symbol,
    returns_col = ra,
    weights = wts,
    col_rename = "investment.growth",
    wealth.index = TRUE
  ) %>% 
  mutate(investment.growth = investment.growth * 10000)
portfolio_growth_monthly %>% 
  ggplot(aes(x = date, y = investment.growth)) + 
  geom_line(size = 1, color = palette_light()[3]) +
  labs(x = "", y = "Portfolio Value",
       title = "Portfolio Growth",
       caption = "Now we can really visualize performance!") + 
  geom_smooth(method = "loess") + 
  theme_tq(base_family = enfont) + 
  scale_color_tq() + 
  scale_y_continuous(labels = scales::dollar)

ggsave("tidyquantperformance2.svg")

# 下面再比较三个投资组合的增长情况
portfolio_growth_monthly_multi <- stock_returns_monthly_multi %>% 
  tq_portfolio(
    assets_col = symbol,
    returns_col = ra,
    weights = weights_table,
    col_rename = "investment.growth",
    wealth.index = TRUE
  ) %>% 
  mutate(investment.growth = investment.growth * 10000)
portfolio_growth_monthly_multi %>% 
  ggplot(aes(x = date, y = investment.growth, color = factor(portfolio))) + 
  geom_line(size = 2) + 
  labs(title = "Portfolio Growth",
       subtitle = "Comparing Multiple Portfolios",
       caption = "Portfolio 3 is a Standout!",
       x = "", 
       y = "Portfolio Value",
       color = "Portfolio") + 
  geom_smooth(method = "loess") + 
  theme_tq(base_family = enfont) + 
  scale_color_tq() +
  scale_y_continuous(labels = scales::dollar)

ggsave("tidyquantperformance3.svg")
# Performance Analysis with tidyquant
library(tidyquant)
(ra <- c("000001.SZ", "000002.SZ", "000004.SZ") %>% 
  tq_get(get = "stock.prices",
         from = "2010-01-01",
         to = "2015-12-31") %>% 
  group_by(symbol) %>% 
  tq_transmute(
    select = adjusted,
    mutate_fun = periodReturn,
    period = "monthly",
    col_rename = "ra"
  ))

# 下面我们使用深证100R指数得到基准价格：
library(Tushare)
api = pro_api('fd65bf1ee56215d0ce1a95d1fa3b453d97a1779438aca26c4b7937d1')
(rb <- api(api_name = "index_daily",
    ts_code = "399004.SZ",
    start_date = "20100101",
    end_date = "20151231") %>% 
  as_tibble() %>% 
  transmute(
    date = ymd(trade_date),
    symbol = as.character(ts_code),
    close = as.numeric(close)
  ) %>% 
  tq_transmute(
    select = close,
    mutate_fun = periodReturn,
    period = "monthly",
    col_rename = "rb"
  ))

# 合并两个数据集：
(rarb <- left_join(ra, rb, by = "date"))

# 使用 tq_performance 运行 CAPM 模型
(rarb_capm <- rarb %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.CAPM
  ))

# 我们比较关心增长测度和风险测度
rarb_capm %>% 
  dplyr::select(Alpha, Beta)

# tidyquant的工作流程
# 单独资产分析
# 查看 SharpeRatio 函数的参数：
args(SharpeRatio)

# Step 1A: 获取股票数据
(stock_prices <- c("000001.SZ", "000002.SZ", "000004.SZ") %>% 
  tq_get(
    get = "stock.prices",
    from = "2010-01-01",
    to = "2015-12-31"
  ))


(stock_returns_monthly <- stock_prices %>% 
  group_by(symbol) %>% 
  tq_transmute(
    select = adjusted,
    mutate_fun = periodReturn,
    period = "monthly",
    col_rename = "ra"
  ))

# Step4: 绩效分析
stock_returns_monthly %>% 
  tq_performance(Ra = ra,
                 Rb = NULL,
                 performance_fun = SharpeRatio)

# 修改无风险利率和置信水平
stock_returns_monthly %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = SharpeRatio,
    Rf = 0.03 / 12,
    p = 0.99
  )

# 资产组合的分析
# 组合资产
# 方法1: 使用权重向量
wts <- c(0.5, 0, 0.5)
(portfolio_returns_monthly <- stock_returns_monthly %>% 
  tq_portfolio(
    assets_col = symbol,
    returns_col = ra,
    weights = wts,
    col_rename = "ra"
  ))

# 方法2: 使用资产 + 权重 数据框
(wts_map <- tibble(
  symbols = c('000001.SZ', '000004.SZ'),
  weights = c(0.5, 0.5)
))

stock_returns_monthly %>% 
  tq_portfolio(
    assets_col = symbol,
    returns_col = ra,
    weights = wts_map,
    col_rename = "ra_using_wts_map"
  )

# 合并ra和rb
(rarb_single_portfolio <- left_join(portfolio_returns_monthly,
                                   rb,
                                   by = "date"))
# 计算 CAPM 模型
rarb_single_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.CAPM
  )

# 多个资产组合
(stock_returns_monthly_multi <- stock_returns_monthly %>% 
  tq_repeat_df(n = 3))
weights <- c(
  0.5, 0.25, 0.25,
  0.25, 0.5, 0.25,
  0.25, 0.25, 0.5
)
stocks = c('000001.SZ', '000002.SZ', '000004.SZ')
(weights_table <- tibble(stocks) %>% 
  tq_repeat_df(3) %>% 
  bind_cols(tibble(weights)) %>% 
  group_by(portfolio))
(portfolio_returns_monthly_multi <- stock_returns_monthly_multi %>% 
  tq_portfolio(assets_col = symbol,
               returns_col = ra,
               weights = weights_table,
               col_rename = 'ra'))
(rarb_multi_portfolio <- left_join(
  portfolio_returns_monthly_multi, rb,
  by = "date"
))

rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.CAPM
  )

rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = SharpeRatio
  )

# 查看所有可以使用的函数
tq_performance_fun_options()

# table.Stats
# 返回基本统计量
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = table.Stats
  )

# table.CAPM
# 返回与CAPM相关的度量
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.CAPM
  )

# table.AnnualizedReturns
# 返回年度回报、标准差和夏普比率
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = table.AnnualizedReturns
  )

# table.Correlation
# 返回与基准收益的回报率
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.Correlation
  )

# table.DownsideRisk
# 返回下行风险表，以便在多个工具或基金之间进行比较
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.DownsideRisk
  )

# table.DownsideRiskRatio
# 返回每月下行风险、年度下行风险等指标
rarb_multi_portfolio %>% 
  tq_performance(Ra = ra,
                 Rb = NULL,
                 performance_fun = table.DownsideRiskRatio)

# table.HigherMoments
# 返回分布的高阶矩和协矩
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.HigherMoments
  )

# table.InformationRatio
# 返回追踪误差表、年度追踪误差和信息比率
rarb_multi_portfolio %>% 
  tq_performance(Ra = ra,
                 Rb = rb,
                 performance_fun = table.InformationRatio)
# table.Variability
# 返回平均绝对差异、月标准差和年标准差
rarb_multi_portfolio %>% 
  tq_performance(Ra = ra,
                 Rb = NULL,
                 performance_fun = table.Variability)

# 在险值
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = VaR
  )

# 夏普比率
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = SharpeRatio
  )

# 自定义tq_portfolio
portfolio_returns_monthly %>% 
  ggplot(aes(x = date, y = ra)) + 
  geom_bar(stat = "identity", fill = palette_light()[[3]],
           width = 20) + 
  labs(title = "Portfolio Returns",
       caption = "Shows an above-zero trend meaning positive returns",
       x = "", y = "Monthly Returns") + 
  geom_smooth(method = "lm") + 
  theme_tq(base_family = enfont) + 
  scale_color_tq() + 
  scale_y_continuous(labels = scales::percent)

ggsave("tidyquantperformance1.svg")

# 使用 wealth.index = TRUE 参数，我们可以观察10000元是如何增长的：
wts <- c(0.5, 0, 0.5)
portfolio_growth_monthly <- stock_returns_monthly %>% 
  tq_portfolio(
    assets_col = symbol,
    returns_col = ra,
    weights = wts,
    col_rename = "investment.growth",
    wealth.index = TRUE
  ) %>% 
  mutate(investment.growth = investment.growth * 10000)
portfolio_growth_monthly %>% 
  ggplot(aes(x = date, y = investment.growth)) + 
  geom_line(size = 1, color = palette_light()[3]) +
  labs(x = "", y = "Portfolio Value",
       title = "Portfolio Growth",
       caption = "Now we can really visualize performance!") + 
  geom_smooth(method = "loess") + 
  theme_tq(base_family = enfont) + 
  scale_color_tq() + 
  scale_y_continuous(labels = scales::dollar)

ggsave("tidyquantperformance2.svg")

# 下面再比较三个投资组合的增长情况
portfolio_growth_monthly_multi <- stock_returns_monthly_multi %>% 
  tq_portfolio(
    assets_col = symbol,
    returns_col = ra,
    weights = weights_table,
    col_rename = "investment.growth",
    wealth.index = TRUE
  ) %>% 
  mutate(investment.growth = investment.growth * 10000)
portfolio_growth_monthly_multi %>% 
  ggplot(aes(x = date, y = investment.growth, color = factor(portfolio))) + 
  geom_line(size = 2) + 
  labs(title = "Portfolio Growth",
       subtitle = "Comparing Multiple Portfolios",
       caption = "Portfolio 3 is a Standout!",
       x = "", 
       y = "Portfolio Value",
       color = "Portfolio") + 
  geom_smooth(method = "loess") + 
  theme_tq(base_family = enfont) + 
  scale_color_tq() +
  scale_y_continuous(labels = scales::dollar)

ggsave("tidyquantperformance3.svg")
# Performance Analysis with tidyquant
library(tidyquant)
(ra <- c("000001.SZ", "000002.SZ", "000004.SZ") %>% 
  tq_get(get = "stock.prices",
         from = "2010-01-01",
         to = "2015-12-31") %>% 
  group_by(symbol) %>% 
  tq_transmute(
    select = adjusted,
    mutate_fun = periodReturn,
    period = "monthly",
    col_rename = "ra"
  ))

# 下面我们使用深证100R指数得到基准价格：
library(Tushare)
api = pro_api('fd65bf1ee56215d0ce1a95d1fa3b453d97a1779438aca26c4b7937d1')
(rb <- api(api_name = "index_daily",
    ts_code = "399004.SZ",
    start_date = "20100101",
    end_date = "20151231") %>% 
  as_tibble() %>% 
  transmute(
    date = ymd(trade_date),
    symbol = as.character(ts_code),
    close = as.numeric(close)
  ) %>% 
  tq_transmute(
    select = close,
    mutate_fun = periodReturn,
    period = "monthly",
    col_rename = "rb"
  ))

# 合并两个数据集：
(rarb <- left_join(ra, rb, by = "date"))

# 使用 tq_performance 运行 CAPM 模型
(rarb_capm <- rarb %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.CAPM
  ))

# 我们比较关心增长测度和风险测度
rarb_capm %>% 
  dplyr::select(Alpha, Beta)

# tidyquant的工作流程
# 单独资产分析
# 查看 SharpeRatio 函数的参数：
args(SharpeRatio)

# Step 1A: 获取股票数据
(stock_prices <- c("000001.SZ", "000002.SZ", "000004.SZ") %>% 
  tq_get(
    get = "stock.prices",
    from = "2010-01-01",
    to = "2015-12-31"
  ))


(stock_returns_monthly <- stock_prices %>% 
  group_by(symbol) %>% 
  tq_transmute(
    select = adjusted,
    mutate_fun = periodReturn,
    period = "monthly",
    col_rename = "ra"
  ))

# Step4: 绩效分析
stock_returns_monthly %>% 
  tq_performance(Ra = ra,
                 Rb = NULL,
                 performance_fun = SharpeRatio)

# 修改无风险利率和置信水平
stock_returns_monthly %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = SharpeRatio,
    Rf = 0.03 / 12,
    p = 0.99
  )

# 资产组合的分析
# 组合资产
# 方法1: 使用权重向量
wts <- c(0.5, 0, 0.5)
(portfolio_returns_monthly <- stock_returns_monthly %>% 
  tq_portfolio(
    assets_col = symbol,
    returns_col = ra,
    weights = wts,
    col_rename = "ra"
  ))

# 方法2: 使用资产 + 权重 数据框
(wts_map <- tibble(
  symbols = c('000001.SZ', '000004.SZ'),
  weights = c(0.5, 0.5)
))

stock_returns_monthly %>% 
  tq_portfolio(
    assets_col = symbol,
    returns_col = ra,
    weights = wts_map,
    col_rename = "ra_using_wts_map"
  )

# 合并ra和rb
(rarb_single_portfolio <- left_join(portfolio_returns_monthly,
                                   rb,
                                   by = "date"))
# 计算 CAPM 模型
rarb_single_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.CAPM
  )

# 多个资产组合
(stock_returns_monthly_multi <- stock_returns_monthly %>% 
  tq_repeat_df(n = 3))
weights <- c(
  0.5, 0.25, 0.25,
  0.25, 0.5, 0.25,
  0.25, 0.25, 0.5
)
stocks = c('000001.SZ', '000002.SZ', '000004.SZ')
(weights_table <- tibble(stocks) %>% 
  tq_repeat_df(3) %>% 
  bind_cols(tibble(weights)) %>% 
  group_by(portfolio))
(portfolio_returns_monthly_multi <- stock_returns_monthly_multi %>% 
  tq_portfolio(assets_col = symbol,
               returns_col = ra,
               weights = weights_table,
               col_rename = 'ra'))
(rarb_multi_portfolio <- left_join(
  portfolio_returns_monthly_multi, rb,
  by = "date"
))

rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.CAPM
  )

rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = SharpeRatio
  )

# 查看所有可以使用的函数
tq_performance_fun_options()

# table.Stats
# 返回基本统计量
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = table.Stats
  )

# table.CAPM
# 返回与CAPM相关的度量
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.CAPM
  )

# table.AnnualizedReturns
# 返回年度回报、标准差和夏普比率
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = table.AnnualizedReturns
  )

# table.Correlation
# 返回与基准收益的回报率
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.Correlation
  )

# table.DownsideRisk
# 返回下行风险表，以便在多个工具或基金之间进行比较
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.DownsideRisk
  )

# table.DownsideRiskRatio
# 返回每月下行风险、年度下行风险等指标
rarb_multi_portfolio %>% 
  tq_performance(Ra = ra,
                 Rb = NULL,
                 performance_fun = table.DownsideRiskRatio)

# table.HigherMoments
# 返回分布的高阶矩和协矩
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.HigherMoments
  )

# table.InformationRatio
# 返回追踪误差表、年度追踪误差和信息比率
rarb_multi_portfolio %>% 
  tq_performance(Ra = ra,
                 Rb = rb,
                 performance_fun = table.InformationRatio)
# table.Variability
# 返回平均绝对差异、月标准差和年标准差
rarb_multi_portfolio %>% 
  tq_performance(Ra = ra,
                 Rb = NULL,
                 performance_fun = table.Variability)

# 在险值
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = VaR
  )

# 夏普比率
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = SharpeRatio
  )

# 自定义tq_portfolio
portfolio_returns_monthly %>% 
  ggplot(aes(x = date, y = ra)) + 
  geom_bar(stat = "identity", fill = palette_light()[[3]],
           width = 20) + 
  labs(title = "Portfolio Returns",
       caption = "Shows an above-zero trend meaning positive returns",
       x = "", y = "Monthly Returns") + 
  geom_smooth(method = "lm") + 
  theme_tq(base_family = enfont) + 
  scale_color_tq() + 
  scale_y_continuous(labels = scales::percent)

ggsave("tidyquantperformance1.svg")

# 使用 wealth.index = TRUE 参数，我们可以观察10000元是如何增长的：
wts <- c(0.5, 0, 0.5)
portfolio_growth_monthly <- stock_returns_monthly %>% 
  tq_portfolio(
    assets_col = symbol,
    returns_col = ra,
    weights = wts,
    col_rename = "investment.growth",
    wealth.index = TRUE
  ) %>% 
  mutate(investment.growth = investment.growth * 10000)
portfolio_growth_monthly %>% 
  ggplot(aes(x = date, y = investment.growth)) + 
  geom_line(size = 1, color = palette_light()[3]) +
  labs(x = "", y = "Portfolio Value",
       title = "Portfolio Growth",
       caption = "Now we can really visualize performance!") + 
  geom_smooth(method = "loess") + 
  theme_tq(base_family = enfont) + 
  scale_color_tq() + 
  scale_y_continuous(labels = scales::dollar)

ggsave("tidyquantperformance2.svg")

# 下面再比较三个投资组合的增长情况
portfolio_growth_monthly_multi <- stock_returns_monthly_multi %>% 
  tq_portfolio(
    assets_col = symbol,
    returns_col = ra,
    weights = weights_table,
    col_rename = "investment.growth",
    wealth.index = TRUE
  ) %>% 
  mutate(investment.growth = investment.growth * 10000)
portfolio_growth_monthly_multi %>% 
  ggplot(aes(x = date, y = investment.growth, color = factor(portfolio))) + 
  geom_line(size = 2) + 
  labs(title = "Portfolio Growth",
       subtitle = "Comparing Multiple Portfolios",
       caption = "Portfolio 3 is a Standout!",
       x = "", 
       y = "Portfolio Value",
       color = "Portfolio") + 
  geom_smooth(method = "loess") + 
  theme_tq(base_family = enfont) + 
  scale_color_tq() +
  scale_y_continuous(labels = scales::dollar)

ggsave("tidyquantperformance3.svg")
# Performance Analysis with tidyquant
library(tidyquant)
(ra <- c("000001.SZ", "000002.SZ", "000004.SZ") %>% 
  tq_get(get = "stock.prices",
         from = "2010-01-01",
         to = "2015-12-31") %>% 
  group_by(symbol) %>% 
  tq_transmute(
    select = adjusted,
    mutate_fun = periodReturn,
    period = "monthly",
    col_rename = "ra"
  ))

# 下面我们使用深证100R指数得到基准价格：
library(Tushare)
api = pro_api('fd65bf1ee56215d0ce1a95d1fa3b453d97a1779438aca26c4b7937d1')
(rb <- api(api_name = "index_daily",
    ts_code = "399004.SZ",
    start_date = "20100101",
    end_date = "20151231") %>% 
  as_tibble() %>% 
  transmute(
    date = ymd(trade_date),
    symbol = as.character(ts_code),
    close = as.numeric(close)
  ) %>% 
  tq_transmute(
    select = close,
    mutate_fun = periodReturn,
    period = "monthly",
    col_rename = "rb"
  ))

# 合并两个数据集：
(rarb <- left_join(ra, rb, by = "date"))

# 使用 tq_performance 运行 CAPM 模型
(rarb_capm <- rarb %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.CAPM
  ))

# 我们比较关心增长测度和风险测度
rarb_capm %>% 
  dplyr::select(Alpha, Beta)

# tidyquant的工作流程
# 单独资产分析
# 查看 SharpeRatio 函数的参数：
args(SharpeRatio)

# Step 1A: 获取股票数据
(stock_prices <- c("000001.SZ", "000002.SZ", "000004.SZ") %>% 
  tq_get(
    get = "stock.prices",
    from = "2010-01-01",
    to = "2015-12-31"
  ))


(stock_returns_monthly <- stock_prices %>% 
  group_by(symbol) %>% 
  tq_transmute(
    select = adjusted,
    mutate_fun = periodReturn,
    period = "monthly",
    col_rename = "ra"
  ))

# Step4: 绩效分析
stock_returns_monthly %>% 
  tq_performance(Ra = ra,
                 Rb = NULL,
                 performance_fun = SharpeRatio)

# 修改无风险利率和置信水平
stock_returns_monthly %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = SharpeRatio,
    Rf = 0.03 / 12,
    p = 0.99
  )

# 资产组合的分析
# 组合资产
# 方法1: 使用权重向量
wts <- c(0.5, 0, 0.5)
(portfolio_returns_monthly <- stock_returns_monthly %>% 
  tq_portfolio(
    assets_col = symbol,
    returns_col = ra,
    weights = wts,
    col_rename = "ra"
  ))

# 方法2: 使用资产 + 权重 数据框
(wts_map <- tibble(
  symbols = c('000001.SZ', '000004.SZ'),
  weights = c(0.5, 0.5)
))

stock_returns_monthly %>% 
  tq_portfolio(
    assets_col = symbol,
    returns_col = ra,
    weights = wts_map,
    col_rename = "ra_using_wts_map"
  )

# 合并ra和rb
(rarb_single_portfolio <- left_join(portfolio_returns_monthly,
                                   rb,
                                   by = "date"))
# 计算 CAPM 模型
rarb_single_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.CAPM
  )

# 多个资产组合
(stock_returns_monthly_multi <- stock_returns_monthly %>% 
  tq_repeat_df(n = 3))
weights <- c(
  0.5, 0.25, 0.25,
  0.25, 0.5, 0.25,
  0.25, 0.25, 0.5
)
stocks = c('000001.SZ', '000002.SZ', '000004.SZ')
(weights_table <- tibble(stocks) %>% 
  tq_repeat_df(3) %>% 
  bind_cols(tibble(weights)) %>% 
  group_by(portfolio))
(portfolio_returns_monthly_multi <- stock_returns_monthly_multi %>% 
  tq_portfolio(assets_col = symbol,
               returns_col = ra,
               weights = weights_table,
               col_rename = 'ra'))
(rarb_multi_portfolio <- left_join(
  portfolio_returns_monthly_multi, rb,
  by = "date"
))

rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.CAPM
  )

rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = SharpeRatio
  )

# 查看所有可以使用的函数
tq_performance_fun_options()

# table.Stats
# 返回基本统计量
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = table.Stats
  )

# table.CAPM
# 返回与CAPM相关的度量
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.CAPM
  )

# table.AnnualizedReturns
# 返回年度回报、标准差和夏普比率
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = table.AnnualizedReturns
  )

# table.Correlation
# 返回与基准收益的回报率
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.Correlation
  )

# table.DownsideRisk
# 返回下行风险表，以便在多个工具或基金之间进行比较
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.DownsideRisk
  )

# table.DownsideRiskRatio
# 返回每月下行风险、年度下行风险等指标
rarb_multi_portfolio %>% 
  tq_performance(Ra = ra,
                 Rb = NULL,
                 performance_fun = table.DownsideRiskRatio)

# table.HigherMoments
# 返回分布的高阶矩和协矩
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.HigherMoments
  )

# table.InformationRatio
# 返回追踪误差表、年度追踪误差和信息比率
rarb_multi_portfolio %>% 
  tq_performance(Ra = ra,
                 Rb = rb,
                 performance_fun = table.InformationRatio)
# table.Variability
# 返回平均绝对差异、月标准差和年标准差
rarb_multi_portfolio %>% 
  tq_performance(Ra = ra,
                 Rb = NULL,
                 performance_fun = table.Variability)

# 在险值
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = VaR
  )

# 夏普比率
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = SharpeRatio
  )

# 自定义tq_portfolio
portfolio_returns_monthly %>% 
  ggplot(aes(x = date, y = ra)) + 
  geom_bar(stat = "identity", fill = palette_light()[[3]],
           width = 20) + 
  labs(title = "Portfolio Returns",
       caption = "Shows an above-zero trend meaning positive returns",
       x = "", y = "Monthly Returns") + 
  geom_smooth(method = "lm") + 
  theme_tq(base_family = enfont) + 
  scale_color_tq() + 
  scale_y_continuous(labels = scales::percent)

ggsave("tidyquantperformance1.svg")

# 使用 wealth.index = TRUE 参数，我们可以观察10000元是如何增长的：
wts <- c(0.5, 0, 0.5)
portfolio_growth_monthly <- stock_returns_monthly %>% 
  tq_portfolio(
    assets_col = symbol,
    returns_col = ra,
    weights = wts,
    col_rename = "investment.growth",
    wealth.index = TRUE
  ) %>% 
  mutate(investment.growth = investment.growth * 10000)
portfolio_growth_monthly %>% 
  ggplot(aes(x = date, y = investment.growth)) + 
  geom_line(size = 1, color = palette_light()[3]) +
  labs(x = "", y = "Portfolio Value",
       title = "Portfolio Growth",
       caption = "Now we can really visualize performance!") + 
  geom_smooth(method = "loess") + 
  theme_tq(base_family = enfont) + 
  scale_color_tq() + 
  scale_y_continuous(labels = scales::dollar)

ggsave("tidyquantperformance2.svg")

# 下面再比较三个投资组合的增长情况
portfolio_growth_monthly_multi <- stock_returns_monthly_multi %>% 
  tq_portfolio(
    assets_col = symbol,
    returns_col = ra,
    weights = weights_table,
    col_rename = "investment.growth",
    wealth.index = TRUE
  ) %>% 
  mutate(investment.growth = investment.growth * 10000)
portfolio_growth_monthly_multi %>% 
  ggplot(aes(x = date, y = investment.growth, color = factor(portfolio))) + 
  geom_line(size = 2) + 
  labs(title = "Portfolio Growth",
       subtitle = "Comparing Multiple Portfolios",
       caption = "Portfolio 3 is a Standout!",
       x = "", 
       y = "Portfolio Value",
       color = "Portfolio") + 
  geom_smooth(method = "loess") + 
  theme_tq(base_family = enfont) + 
  scale_color_tq() +
  scale_y_continuous(labels = scales::dollar)

ggsave("tidyquantperformance3.svg")
# Performance Analysis with tidyquant
library(tidyquant)
(ra <- c("000001.SZ", "000002.SZ", "000004.SZ") %>% 
  tq_get(get = "stock.prices",
         from = "2010-01-01",
         to = "2015-12-31") %>% 
  group_by(symbol) %>% 
  tq_transmute(
    select = adjusted,
    mutate_fun = periodReturn,
    period = "monthly",
    col_rename = "ra"
  ))

# 下面我们使用深证100R指数得到基准价格：
library(Tushare)
api = pro_api('fd65bf1ee56215d0ce1a95d1fa3b453d97a1779438aca26c4b7937d1')
(rb <- api(api_name = "index_daily",
    ts_code = "399004.SZ",
    start_date = "20100101",
    end_date = "20151231") %>% 
  as_tibble() %>% 
  transmute(
    date = ymd(trade_date),
    symbol = as.character(ts_code),
    close = as.numeric(close)
  ) %>% 
  tq_transmute(
    select = close,
    mutate_fun = periodReturn,
    period = "monthly",
    col_rename = "rb"
  ))

# 合并两个数据集：
(rarb <- left_join(ra, rb, by = "date"))

# 使用 tq_performance 运行 CAPM 模型
(rarb_capm <- rarb %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.CAPM
  ))

# 我们比较关心增长测度和风险测度
rarb_capm %>% 
  dplyr::select(Alpha, Beta)

# tidyquant的工作流程
# 单独资产分析
# 查看 SharpeRatio 函数的参数：
args(SharpeRatio)

# Step 1A: 获取股票数据
(stock_prices <- c("000001.SZ", "000002.SZ", "000004.SZ") %>% 
  tq_get(
    get = "stock.prices",
    from = "2010-01-01",
    to = "2015-12-31"
  ))


(stock_returns_monthly <- stock_prices %>% 
  group_by(symbol) %>% 
  tq_transmute(
    select = adjusted,
    mutate_fun = periodReturn,
    period = "monthly",
    col_rename = "ra"
  ))

# Step4: 绩效分析
stock_returns_monthly %>% 
  tq_performance(Ra = ra,
                 Rb = NULL,
                 performance_fun = SharpeRatio)

# 修改无风险利率和置信水平
stock_returns_monthly %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = SharpeRatio,
    Rf = 0.03 / 12,
    p = 0.99
  )

# 资产组合的分析
# 组合资产
# 方法1: 使用权重向量
wts <- c(0.5, 0, 0.5)
(portfolio_returns_monthly <- stock_returns_monthly %>% 
  tq_portfolio(
    assets_col = symbol,
    returns_col = ra,
    weights = wts,
    col_rename = "ra"
  ))

# 方法2: 使用资产 + 权重 数据框
(wts_map <- tibble(
  symbols = c('000001.SZ', '000004.SZ'),
  weights = c(0.5, 0.5)
))

stock_returns_monthly %>% 
  tq_portfolio(
    assets_col = symbol,
    returns_col = ra,
    weights = wts_map,
    col_rename = "ra_using_wts_map"
  )

# 合并ra和rb
(rarb_single_portfolio <- left_join(portfolio_returns_monthly,
                                   rb,
                                   by = "date"))
# 计算 CAPM 模型
rarb_single_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.CAPM
  )

# 多个资产组合
(stock_returns_monthly_multi <- stock_returns_monthly %>% 
  tq_repeat_df(n = 3))
weights <- c(
  0.5, 0.25, 0.25,
  0.25, 0.5, 0.25,
  0.25, 0.25, 0.5
)
stocks = c('000001.SZ', '000002.SZ', '000004.SZ')
(weights_table <- tibble(stocks) %>% 
  tq_repeat_df(3) %>% 
  bind_cols(tibble(weights)) %>% 
  group_by(portfolio))
(portfolio_returns_monthly_multi <- stock_returns_monthly_multi %>% 
  tq_portfolio(assets_col = symbol,
               returns_col = ra,
               weights = weights_table,
               col_rename = 'ra'))
(rarb_multi_portfolio <- left_join(
  portfolio_returns_monthly_multi, rb,
  by = "date"
))

rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.CAPM
  )

rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = SharpeRatio
  )

# 查看所有可以使用的函数
tq_performance_fun_options()

# table.Stats
# 返回基本统计量
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = table.Stats
  )

# table.CAPM
# 返回与CAPM相关的度量
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.CAPM
  )

# table.AnnualizedReturns
# 返回年度回报、标准差和夏普比率
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = table.AnnualizedReturns
  )

# table.Correlation
# 返回与基准收益的回报率
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.Correlation
  )

# table.DownsideRisk
# 返回下行风险表，以便在多个工具或基金之间进行比较
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.DownsideRisk
  )

# table.DownsideRiskRatio
# 返回每月下行风险、年度下行风险等指标
rarb_multi_portfolio %>% 
  tq_performance(Ra = ra,
                 Rb = NULL,
                 performance_fun = table.DownsideRiskRatio)

# table.HigherMoments
# 返回分布的高阶矩和协矩
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.HigherMoments
  )

# table.InformationRatio
# 返回追踪误差表、年度追踪误差和信息比率
rarb_multi_portfolio %>% 
  tq_performance(Ra = ra,
                 Rb = rb,
                 performance_fun = table.InformationRatio)
# table.Variability
# 返回平均绝对差异、月标准差和年标准差
rarb_multi_portfolio %>% 
  tq_performance(Ra = ra,
                 Rb = NULL,
                 performance_fun = table.Variability)

# 在险值
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = VaR
  )

# 夏普比率
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = SharpeRatio
  )

# 自定义tq_portfolio
portfolio_returns_monthly %>% 
  ggplot(aes(x = date, y = ra)) + 
  geom_bar(stat = "identity", fill = palette_light()[[3]],
           width = 20) + 
  labs(title = "Portfolio Returns",
       caption = "Shows an above-zero trend meaning positive returns",
       x = "", y = "Monthly Returns") + 
  geom_smooth(method = "lm") + 
  theme_tq(base_family = enfont) + 
  scale_color_tq() + 
  scale_y_continuous(labels = scales::percent)

ggsave("tidyquantperformance1.svg")

# 使用 wealth.index = TRUE 参数，我们可以观察10000元是如何增长的：
wts <- c(0.5, 0, 0.5)
portfolio_growth_monthly <- stock_returns_monthly %>% 
  tq_portfolio(
    assets_col = symbol,
    returns_col = ra,
    weights = wts,
    col_rename = "investment.growth",
    wealth.index = TRUE
  ) %>% 
  mutate(investment.growth = investment.growth * 10000)
portfolio_growth_monthly %>% 
  ggplot(aes(x = date, y = investment.growth)) + 
  geom_line(size = 1, color = palette_light()[3]) +
  labs(x = "", y = "Portfolio Value",
       title = "Portfolio Growth",
       caption = "Now we can really visualize performance!") + 
  geom_smooth(method = "loess") + 
  theme_tq(base_family = enfont) + 
  scale_color_tq() + 
  scale_y_continuous(labels = scales::dollar)

ggsave("tidyquantperformance2.svg")

# 下面再比较三个投资组合的增长情况
portfolio_growth_monthly_multi <- stock_returns_monthly_multi %>% 
  tq_portfolio(
    assets_col = symbol,
    returns_col = ra,
    weights = weights_table,
    col_rename = "investment.growth",
    wealth.index = TRUE
  ) %>% 
  mutate(investment.growth = investment.growth * 10000)
portfolio_growth_monthly_multi %>% 
  ggplot(aes(x = date, y = investment.growth, color = factor(portfolio))) + 
  geom_line(size = 2) + 
  labs(title = "Portfolio Growth",
       subtitle = "Comparing Multiple Portfolios",
       caption = "Portfolio 3 is a Standout!",
       x = "", 
       y = "Portfolio Value",
       color = "Portfolio") + 
  geom_smooth(method = "loess") + 
  theme_tq(base_family = enfont) + 
  scale_color_tq() +
  scale_y_continuous(labels = scales::dollar)

ggsave("tidyquantperformance3.svg")
# Performance Analysis with tidyquant
library(tidyquant)
(ra <- c("000001.SZ", "000002.SZ", "000004.SZ") %>% 
  tq_get(get = "stock.prices",
         from = "2010-01-01",
         to = "2015-12-31") %>% 
  group_by(symbol) %>% 
  tq_transmute(
    select = adjusted,
    mutate_fun = periodReturn,
    period = "monthly",
    col_rename = "ra"
  ))

# 下面我们使用深证100R指数得到基准价格：
library(Tushare)
api = pro_api('fd65bf1ee56215d0ce1a95d1fa3b453d97a1779438aca26c4b7937d1')
(rb <- api(api_name = "index_daily",
    ts_code = "399004.SZ",
    start_date = "20100101",
    end_date = "20151231") %>% 
  as_tibble() %>% 
  transmute(
    date = ymd(trade_date),
    symbol = as.character(ts_code),
    close = as.numeric(close)
  ) %>% 
  tq_transmute(
    select = close,
    mutate_fun = periodReturn,
    period = "monthly",
    col_rename = "rb"
  ))

# 合并两个数据集：
(rarb <- left_join(ra, rb, by = "date"))

# 使用 tq_performance 运行 CAPM 模型
(rarb_capm <- rarb %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.CAPM
  ))

# 我们比较关心增长测度和风险测度
rarb_capm %>% 
  dplyr::select(Alpha, Beta)

# tidyquant的工作流程
# 单独资产分析
# 查看 SharpeRatio 函数的参数：
args(SharpeRatio)

# Step 1A: 获取股票数据
(stock_prices <- c("000001.SZ", "000002.SZ", "000004.SZ") %>% 
  tq_get(
    get = "stock.prices",
    from = "2010-01-01",
    to = "2015-12-31"
  ))


(stock_returns_monthly <- stock_prices %>% 
  group_by(symbol) %>% 
  tq_transmute(
    select = adjusted,
    mutate_fun = periodReturn,
    period = "monthly",
    col_rename = "ra"
  ))

# Step4: 绩效分析
stock_returns_monthly %>% 
  tq_performance(Ra = ra,
                 Rb = NULL,
                 performance_fun = SharpeRatio)

# 修改无风险利率和置信水平
stock_returns_monthly %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = SharpeRatio,
    Rf = 0.03 / 12,
    p = 0.99
  )

# 资产组合的分析
# 组合资产
# 方法1: 使用权重向量
wts <- c(0.5, 0, 0.5)
(portfolio_returns_monthly <- stock_returns_monthly %>% 
  tq_portfolio(
    assets_col = symbol,
    returns_col = ra,
    weights = wts,
    col_rename = "ra"
  ))

# 方法2: 使用资产 + 权重 数据框
(wts_map <- tibble(
  symbols = c('000001.SZ', '000004.SZ'),
  weights = c(0.5, 0.5)
))

stock_returns_monthly %>% 
  tq_portfolio(
    assets_col = symbol,
    returns_col = ra,
    weights = wts_map,
    col_rename = "ra_using_wts_map"
  )

# 合并ra和rb
(rarb_single_portfolio <- left_join(portfolio_returns_monthly,
                                   rb,
                                   by = "date"))
# 计算 CAPM 模型
rarb_single_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.CAPM
  )

# 多个资产组合
(stock_returns_monthly_multi <- stock_returns_monthly %>% 
  tq_repeat_df(n = 3))
weights <- c(
  0.5, 0.25, 0.25,
  0.25, 0.5, 0.25,
  0.25, 0.25, 0.5
)
stocks = c('000001.SZ', '000002.SZ', '000004.SZ')
(weights_table <- tibble(stocks) %>% 
  tq_repeat_df(3) %>% 
  bind_cols(tibble(weights)) %>% 
  group_by(portfolio))
(portfolio_returns_monthly_multi <- stock_returns_monthly_multi %>% 
  tq_portfolio(assets_col = symbol,
               returns_col = ra,
               weights = weights_table,
               col_rename = 'ra'))
(rarb_multi_portfolio <- left_join(
  portfolio_returns_monthly_multi, rb,
  by = "date"
))

rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.CAPM
  )

rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = SharpeRatio
  )

# 查看所有可以使用的函数
tq_performance_fun_options()

# table.Stats
# 返回基本统计量
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = table.Stats
  )

# table.CAPM
# 返回与CAPM相关的度量
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.CAPM
  )

# table.AnnualizedReturns
# 返回年度回报、标准差和夏普比率
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = table.AnnualizedReturns
  )

# table.Correlation
# 返回与基准收益的回报率
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.Correlation
  )

# table.DownsideRisk
# 返回下行风险表，以便在多个工具或基金之间进行比较
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.DownsideRisk
  )

# table.DownsideRiskRatio
# 返回每月下行风险、年度下行风险等指标
rarb_multi_portfolio %>% 
  tq_performance(Ra = ra,
                 Rb = NULL,
                 performance_fun = table.DownsideRiskRatio)

# table.HigherMoments
# 返回分布的高阶矩和协矩
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.HigherMoments
  )

# table.InformationRatio
# 返回追踪误差表、年度追踪误差和信息比率
rarb_multi_portfolio %>% 
  tq_performance(Ra = ra,
                 Rb = rb,
                 performance_fun = table.InformationRatio)
# table.Variability
# 返回平均绝对差异、月标准差和年标准差
rarb_multi_portfolio %>% 
  tq_performance(Ra = ra,
                 Rb = NULL,
                 performance_fun = table.Variability)

# 在险值
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = VaR
  )

# 夏普比率
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = SharpeRatio
  )

# 自定义tq_portfolio
portfolio_returns_monthly %>% 
  ggplot(aes(x = date, y = ra)) + 
  geom_bar(stat = "identity", fill = palette_light()[[3]],
           width = 20) + 
  labs(title = "Portfolio Returns",
       caption = "Shows an above-zero trend meaning positive returns",
       x = "", y = "Monthly Returns") + 
  geom_smooth(method = "lm") + 
  theme_tq(base_family = enfont) + 
  scale_color_tq() + 
  scale_y_continuous(labels = scales::percent)

ggsave("tidyquantperformance1.svg")

# 使用 wealth.index = TRUE 参数，我们可以观察10000元是如何增长的：
wts <- c(0.5, 0, 0.5)
portfolio_growth_monthly <- stock_returns_monthly %>% 
  tq_portfolio(
    assets_col = symbol,
    returns_col = ra,
    weights = wts,
    col_rename = "investment.growth",
    wealth.index = TRUE
  ) %>% 
  mutate(investment.growth = investment.growth * 10000)
portfolio_growth_monthly %>% 
  ggplot(aes(x = date, y = investment.growth)) + 
  geom_line(size = 1, color = palette_light()[3]) +
  labs(x = "", y = "Portfolio Value",
       title = "Portfolio Growth",
       caption = "Now we can really visualize performance!") + 
  geom_smooth(method = "loess") + 
  theme_tq(base_family = enfont) + 
  scale_color_tq() + 
  scale_y_continuous(labels = scales::dollar)

ggsave("tidyquantperformance2.svg")

# 下面再比较三个投资组合的增长情况
portfolio_growth_monthly_multi <- stock_returns_monthly_multi %>% 
  tq_portfolio(
    assets_col = symbol,
    returns_col = ra,
    weights = weights_table,
    col_rename = "investment.growth",
    wealth.index = TRUE
  ) %>% 
  mutate(investment.growth = investment.growth * 10000)
portfolio_growth_monthly_multi %>% 
  ggplot(aes(x = date, y = investment.growth, color = factor(portfolio))) + 
  geom_line(size = 2) + 
  labs(title = "Portfolio Growth",
       subtitle = "Comparing Multiple Portfolios",
       caption = "Portfolio 3 is a Standout!",
       x = "", 
       y = "Portfolio Value",
       color = "Portfolio") + 
  geom_smooth(method = "loess") + 
  theme_tq(base_family = enfont) + 
  scale_color_tq() +
  scale_y_continuous(labels = scales::dollar)

ggsave("tidyquantperformance3.svg")
# Performance Analysis with tidyquant
library(tidyquant)
(ra <- c("000001.SZ", "000002.SZ", "000004.SZ") %>% 
  tq_get(get = "stock.prices",
         from = "2010-01-01",
         to = "2015-12-31") %>% 
  group_by(symbol) %>% 
  tq_transmute(
    select = adjusted,
    mutate_fun = periodReturn,
    period = "monthly",
    col_rename = "ra"
  ))

# 下面我们使用深证100R指数得到基准价格：
library(Tushare)
api = pro_api('fd65bf1ee56215d0ce1a95d1fa3b453d97a1779438aca26c4b7937d1')
(rb <- api(api_name = "index_daily",
    ts_code = "399004.SZ",
    start_date = "20100101",
    end_date = "20151231") %>% 
  as_tibble() %>% 
  transmute(
    date = ymd(trade_date),
    symbol = as.character(ts_code),
    close = as.numeric(close)
  ) %>% 
  tq_transmute(
    select = close,
    mutate_fun = periodReturn,
    period = "monthly",
    col_rename = "rb"
  ))

# 合并两个数据集：
(rarb <- left_join(ra, rb, by = "date"))

# 使用 tq_performance 运行 CAPM 模型
(rarb_capm <- rarb %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.CAPM
  ))

# 我们比较关心增长测度和风险测度
rarb_capm %>% 
  dplyr::select(Alpha, Beta)

# tidyquant的工作流程
# 单独资产分析
# 查看 SharpeRatio 函数的参数：
args(SharpeRatio)

# Step 1A: 获取股票数据
(stock_prices <- c("000001.SZ", "000002.SZ", "000004.SZ") %>% 
  tq_get(
    get = "stock.prices",
    from = "2010-01-01",
    to = "2015-12-31"
  ))


(stock_returns_monthly <- stock_prices %>% 
  group_by(symbol) %>% 
  tq_transmute(
    select = adjusted,
    mutate_fun = periodReturn,
    period = "monthly",
    col_rename = "ra"
  ))

# Step4: 绩效分析
stock_returns_monthly %>% 
  tq_performance(Ra = ra,
                 Rb = NULL,
                 performance_fun = SharpeRatio)

# 修改无风险利率和置信水平
stock_returns_monthly %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = SharpeRatio,
    Rf = 0.03 / 12,
    p = 0.99
  )

# 资产组合的分析
# 组合资产
# 方法1: 使用权重向量
wts <- c(0.5, 0, 0.5)
(portfolio_returns_monthly <- stock_returns_monthly %>% 
  tq_portfolio(
    assets_col = symbol,
    returns_col = ra,
    weights = wts,
    col_rename = "ra"
  ))

# 方法2: 使用资产 + 权重 数据框
(wts_map <- tibble(
  symbols = c('000001.SZ', '000004.SZ'),
  weights = c(0.5, 0.5)
))

stock_returns_monthly %>% 
  tq_portfolio(
    assets_col = symbol,
    returns_col = ra,
    weights = wts_map,
    col_rename = "ra_using_wts_map"
  )

# 合并ra和rb
(rarb_single_portfolio <- left_join(portfolio_returns_monthly,
                                   rb,
                                   by = "date"))
# 计算 CAPM 模型
rarb_single_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.CAPM
  )

# 多个资产组合
(stock_returns_monthly_multi <- stock_returns_monthly %>% 
  tq_repeat_df(n = 3))
weights <- c(
  0.5, 0.25, 0.25,
  0.25, 0.5, 0.25,
  0.25, 0.25, 0.5
)
stocks = c('000001.SZ', '000002.SZ', '000004.SZ')
(weights_table <- tibble(stocks) %>% 
  tq_repeat_df(3) %>% 
  bind_cols(tibble(weights)) %>% 
  group_by(portfolio))
(portfolio_returns_monthly_multi <- stock_returns_monthly_multi %>% 
  tq_portfolio(assets_col = symbol,
               returns_col = ra,
               weights = weights_table,
               col_rename = 'ra'))
(rarb_multi_portfolio <- left_join(
  portfolio_returns_monthly_multi, rb,
  by = "date"
))

rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.CAPM
  )

rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = SharpeRatio
  )

# 查看所有可以使用的函数
tq_performance_fun_options()

# table.Stats
# 返回基本统计量
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = table.Stats
  )

# table.CAPM
# 返回与CAPM相关的度量
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.CAPM
  )

# table.AnnualizedReturns
# 返回年度回报、标准差和夏普比率
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = table.AnnualizedReturns
  )

# table.Correlation
# 返回与基准收益的回报率
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.Correlation
  )

# table.DownsideRisk
# 返回下行风险表，以便在多个工具或基金之间进行比较
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.DownsideRisk
  )

# table.DownsideRiskRatio
# 返回每月下行风险、年度下行风险等指标
rarb_multi_portfolio %>% 
  tq_performance(Ra = ra,
                 Rb = NULL,
                 performance_fun = table.DownsideRiskRatio)

# table.HigherMoments
# 返回分布的高阶矩和协矩
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.HigherMoments
  )

# table.InformationRatio
# 返回追踪误差表、年度追踪误差和信息比率
rarb_multi_portfolio %>% 
  tq_performance(Ra = ra,
                 Rb = rb,
                 performance_fun = table.InformationRatio)
# table.Variability
# 返回平均绝对差异、月标准差和年标准差
rarb_multi_portfolio %>% 
  tq_performance(Ra = ra,
                 Rb = NULL,
                 performance_fun = table.Variability)

# 在险值
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = VaR
  )

# 夏普比率
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = SharpeRatio
  )

# 自定义tq_portfolio
portfolio_returns_monthly %>% 
  ggplot(aes(x = date, y = ra)) + 
  geom_bar(stat = "identity", fill = palette_light()[[3]],
           width = 20) + 
  labs(title = "Portfolio Returns",
       caption = "Shows an above-zero trend meaning positive returns",
       x = "", y = "Monthly Returns") + 
  geom_smooth(method = "lm") + 
  theme_tq(base_family = enfont) + 
  scale_color_tq() + 
  scale_y_continuous(labels = scales::percent)

ggsave("tidyquantperformance1.svg")

# 使用 wealth.index = TRUE 参数，我们可以观察10000元是如何增长的：
wts <- c(0.5, 0, 0.5)
portfolio_growth_monthly <- stock_returns_monthly %>% 
  tq_portfolio(
    assets_col = symbol,
    returns_col = ra,
    weights = wts,
    col_rename = "investment.growth",
    wealth.index = TRUE
  ) %>% 
  mutate(investment.growth = investment.growth * 10000)
portfolio_growth_monthly %>% 
  ggplot(aes(x = date, y = investment.growth)) + 
  geom_line(size = 1, color = palette_light()[3]) +
  labs(x = "", y = "Portfolio Value",
       title = "Portfolio Growth",
       caption = "Now we can really visualize performance!") + 
  geom_smooth(method = "loess") + 
  theme_tq(base_family = enfont) + 
  scale_color_tq() + 
  scale_y_continuous(labels = scales::dollar)

ggsave("tidyquantperformance2.svg")

# 下面再比较三个投资组合的增长情况
portfolio_growth_monthly_multi <- stock_returns_monthly_multi %>% 
  tq_portfolio(
    assets_col = symbol,
    returns_col = ra,
    weights = weights_table,
    col_rename = "investment.growth",
    wealth.index = TRUE
  ) %>% 
  mutate(investment.growth = investment.growth * 10000)
portfolio_growth_monthly_multi %>% 
  ggplot(aes(x = date, y = investment.growth, color = factor(portfolio))) + 
  geom_line(size = 2) + 
  labs(title = "Portfolio Growth",
       subtitle = "Comparing Multiple Portfolios",
       caption = "Portfolio 3 is a Standout!",
       x = "", 
       y = "Portfolio Value",
       color = "Portfolio") + 
  geom_smooth(method = "loess") + 
  theme_tq(base_family = enfont) + 
  scale_color_tq() +
  scale_y_continuous(labels = scales::dollar)

ggsave("tidyquantperformance3.svg")
# Performance Analysis with tidyquant
library(tidyquant)
(ra <- c("000001.SZ", "000002.SZ", "000004.SZ") %>% 
  tq_get(get = "stock.prices",
         from = "2010-01-01",
         to = "2015-12-31") %>% 
  group_by(symbol) %>% 
  tq_transmute(
    select = adjusted,
    mutate_fun = periodReturn,
    period = "monthly",
    col_rename = "ra"
  ))

# 下面我们使用深证100R指数得到基准价格：
library(Tushare)
api = pro_api('fd65bf1ee56215d0ce1a95d1fa3b453d97a1779438aca26c4b7937d1')
(rb <- api(api_name = "index_daily",
    ts_code = "399004.SZ",
    start_date = "20100101",
    end_date = "20151231") %>% 
  as_tibble() %>% 
  transmute(
    date = ymd(trade_date),
    symbol = as.character(ts_code),
    close = as.numeric(close)
  ) %>% 
  tq_transmute(
    select = close,
    mutate_fun = periodReturn,
    period = "monthly",
    col_rename = "rb"
  ))

# 合并两个数据集：
(rarb <- left_join(ra, rb, by = "date"))

# 使用 tq_performance 运行 CAPM 模型
(rarb_capm <- rarb %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.CAPM
  ))

# 我们比较关心增长测度和风险测度
rarb_capm %>% 
  dplyr::select(Alpha, Beta)

# tidyquant的工作流程
# 单独资产分析
# 查看 SharpeRatio 函数的参数：
args(SharpeRatio)

# Step 1A: 获取股票数据
(stock_prices <- c("000001.SZ", "000002.SZ", "000004.SZ") %>% 
  tq_get(
    get = "stock.prices",
    from = "2010-01-01",
    to = "2015-12-31"
  ))


(stock_returns_monthly <- stock_prices %>% 
  group_by(symbol) %>% 
  tq_transmute(
    select = adjusted,
    mutate_fun = periodReturn,
    period = "monthly",
    col_rename = "ra"
  ))

# Step4: 绩效分析
stock_returns_monthly %>% 
  tq_performance(Ra = ra,
                 Rb = NULL,
                 performance_fun = SharpeRatio)

# 修改无风险利率和置信水平
stock_returns_monthly %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = SharpeRatio,
    Rf = 0.03 / 12,
    p = 0.99
  )

# 资产组合的分析
# 组合资产
# 方法1: 使用权重向量
wts <- c(0.5, 0, 0.5)
(portfolio_returns_monthly <- stock_returns_monthly %>% 
  tq_portfolio(
    assets_col = symbol,
    returns_col = ra,
    weights = wts,
    col_rename = "ra"
  ))

# 方法2: 使用资产 + 权重 数据框
(wts_map <- tibble(
  symbols = c('000001.SZ', '000004.SZ'),
  weights = c(0.5, 0.5)
))

stock_returns_monthly %>% 
  tq_portfolio(
    assets_col = symbol,
    returns_col = ra,
    weights = wts_map,
    col_rename = "ra_using_wts_map"
  )

# 合并ra和rb
(rarb_single_portfolio <- left_join(portfolio_returns_monthly,
                                   rb,
                                   by = "date"))
# 计算 CAPM 模型
rarb_single_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.CAPM
  )

# 多个资产组合
(stock_returns_monthly_multi <- stock_returns_monthly %>% 
  tq_repeat_df(n = 3))
weights <- c(
  0.5, 0.25, 0.25,
  0.25, 0.5, 0.25,
  0.25, 0.25, 0.5
)
stocks = c('000001.SZ', '000002.SZ', '000004.SZ')
(weights_table <- tibble(stocks) %>% 
  tq_repeat_df(3) %>% 
  bind_cols(tibble(weights)) %>% 
  group_by(portfolio))
(portfolio_returns_monthly_multi <- stock_returns_monthly_multi %>% 
  tq_portfolio(assets_col = symbol,
               returns_col = ra,
               weights = weights_table,
               col_rename = 'ra'))
(rarb_multi_portfolio <- left_join(
  portfolio_returns_monthly_multi, rb,
  by = "date"
))

rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.CAPM
  )

rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = SharpeRatio
  )

# 查看所有可以使用的函数
tq_performance_fun_options()

# table.Stats
# 返回基本统计量
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = table.Stats
  )

# table.CAPM
# 返回与CAPM相关的度量
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.CAPM
  )

# table.AnnualizedReturns
# 返回年度回报、标准差和夏普比率
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = table.AnnualizedReturns
  )

# table.Correlation
# 返回与基准收益的回报率
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.Correlation
  )

# table.DownsideRisk
# 返回下行风险表，以便在多个工具或基金之间进行比较
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.DownsideRisk
  )

# table.DownsideRiskRatio
# 返回每月下行风险、年度下行风险等指标
rarb_multi_portfolio %>% 
  tq_performance(Ra = ra,
                 Rb = NULL,
                 performance_fun = table.DownsideRiskRatio)

# table.HigherMoments
# 返回分布的高阶矩和协矩
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.HigherMoments
  )

# table.InformationRatio
# 返回追踪误差表、年度追踪误差和信息比率
rarb_multi_portfolio %>% 
  tq_performance(Ra = ra,
                 Rb = rb,
                 performance_fun = table.InformationRatio)
# table.Variability
# 返回平均绝对差异、月标准差和年标准差
rarb_multi_portfolio %>% 
  tq_performance(Ra = ra,
                 Rb = NULL,
                 performance_fun = table.Variability)

# 在险值
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = VaR
  )

# 夏普比率
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = SharpeRatio
  )

# 自定义tq_portfolio
portfolio_returns_monthly %>% 
  ggplot(aes(x = date, y = ra)) + 
  geom_bar(stat = "identity", fill = palette_light()[[3]],
           width = 20) + 
  labs(title = "Portfolio Returns",
       caption = "Shows an above-zero trend meaning positive returns",
       x = "", y = "Monthly Returns") + 
  geom_smooth(method = "lm") + 
  theme_tq(base_family = enfont) + 
  scale_color_tq() + 
  scale_y_continuous(labels = scales::percent)

ggsave("tidyquantperformance1.svg")

# 使用 wealth.index = TRUE 参数，我们可以观察10000元是如何增长的：
wts <- c(0.5, 0, 0.5)
portfolio_growth_monthly <- stock_returns_monthly %>% 
  tq_portfolio(
    assets_col = symbol,
    returns_col = ra,
    weights = wts,
    col_rename = "investment.growth",
    wealth.index = TRUE
  ) %>% 
  mutate(investment.growth = investment.growth * 10000)
portfolio_growth_monthly %>% 
  ggplot(aes(x = date, y = investment.growth)) + 
  geom_line(size = 1, color = palette_light()[3]) +
  labs(x = "", y = "Portfolio Value",
       title = "Portfolio Growth",
       caption = "Now we can really visualize performance!") + 
  geom_smooth(method = "loess") + 
  theme_tq(base_family = enfont) + 
  scale_color_tq() + 
  scale_y_continuous(labels = scales::dollar)

ggsave("tidyquantperformance2.svg")

# 下面再比较三个投资组合的增长情况
portfolio_growth_monthly_multi <- stock_returns_monthly_multi %>% 
  tq_portfolio(
    assets_col = symbol,
    returns_col = ra,
    weights = weights_table,
    col_rename = "investment.growth",
    wealth.index = TRUE
  ) %>% 
  mutate(investment.growth = investment.growth * 10000)
portfolio_growth_monthly_multi %>% 
  ggplot(aes(x = date, y = investment.growth, color = factor(portfolio))) + 
  geom_line(size = 2) + 
  labs(title = "Portfolio Growth",
       subtitle = "Comparing Multiple Portfolios",
       caption = "Portfolio 3 is a Standout!",
       x = "", 
       y = "Portfolio Value",
       color = "Portfolio") + 
  geom_smooth(method = "loess") + 
  theme_tq(base_family = enfont) + 
  scale_color_tq() +
  scale_y_continuous(labels = scales::dollar)

ggsave("tidyquantperformance3.svg")
# Performance Analysis with tidyquant
library(tidyquant)
(ra <- c("000001.SZ", "000002.SZ", "000004.SZ") %>% 
  tq_get(get = "stock.prices",
         from = "2010-01-01",
         to = "2015-12-31") %>% 
  group_by(symbol) %>% 
  tq_transmute(
    select = adjusted,
    mutate_fun = periodReturn,
    period = "monthly",
    col_rename = "ra"
  ))

# 下面我们使用深证100R指数得到基准价格：
library(Tushare)
api = pro_api('fd65bf1ee56215d0ce1a95d1fa3b453d97a1779438aca26c4b7937d1')
(rb <- api(api_name = "index_daily",
    ts_code = "399004.SZ",
    start_date = "20100101",
    end_date = "20151231") %>% 
  as_tibble() %>% 
  transmute(
    date = ymd(trade_date),
    symbol = as.character(ts_code),
    close = as.numeric(close)
  ) %>% 
  tq_transmute(
    select = close,
    mutate_fun = periodReturn,
    period = "monthly",
    col_rename = "rb"
  ))

# 合并两个数据集：
(rarb <- left_join(ra, rb, by = "date"))

# 使用 tq_performance 运行 CAPM 模型
(rarb_capm <- rarb %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.CAPM
  ))

# 我们比较关心增长测度和风险测度
rarb_capm %>% 
  dplyr::select(Alpha, Beta)

# tidyquant的工作流程
# 单独资产分析
# 查看 SharpeRatio 函数的参数：
args(SharpeRatio)

# Step 1A: 获取股票数据
(stock_prices <- c("000001.SZ", "000002.SZ", "000004.SZ") %>% 
  tq_get(
    get = "stock.prices",
    from = "2010-01-01",
    to = "2015-12-31"
  ))


(stock_returns_monthly <- stock_prices %>% 
  group_by(symbol) %>% 
  tq_transmute(
    select = adjusted,
    mutate_fun = periodReturn,
    period = "monthly",
    col_rename = "ra"
  ))

# Step4: 绩效分析
stock_returns_monthly %>% 
  tq_performance(Ra = ra,
                 Rb = NULL,
                 performance_fun = SharpeRatio)

# 修改无风险利率和置信水平
stock_returns_monthly %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = SharpeRatio,
    Rf = 0.03 / 12,
    p = 0.99
  )

# 资产组合的分析
# 组合资产
# 方法1: 使用权重向量
wts <- c(0.5, 0, 0.5)
(portfolio_returns_monthly <- stock_returns_monthly %>% 
  tq_portfolio(
    assets_col = symbol,
    returns_col = ra,
    weights = wts,
    col_rename = "ra"
  ))

# 方法2: 使用资产 + 权重 数据框
(wts_map <- tibble(
  symbols = c('000001.SZ', '000004.SZ'),
  weights = c(0.5, 0.5)
))

stock_returns_monthly %>% 
  tq_portfolio(
    assets_col = symbol,
    returns_col = ra,
    weights = wts_map,
    col_rename = "ra_using_wts_map"
  )

# 合并ra和rb
(rarb_single_portfolio <- left_join(portfolio_returns_monthly,
                                   rb,
                                   by = "date"))
# 计算 CAPM 模型
rarb_single_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.CAPM
  )

# 多个资产组合
(stock_returns_monthly_multi <- stock_returns_monthly %>% 
  tq_repeat_df(n = 3))
weights <- c(
  0.5, 0.25, 0.25,
  0.25, 0.5, 0.25,
  0.25, 0.25, 0.5
)
stocks = c('000001.SZ', '000002.SZ', '000004.SZ')
(weights_table <- tibble(stocks) %>% 
  tq_repeat_df(3) %>% 
  bind_cols(tibble(weights)) %>% 
  group_by(portfolio))
(portfolio_returns_monthly_multi <- stock_returns_monthly_multi %>% 
  tq_portfolio(assets_col = symbol,
               returns_col = ra,
               weights = weights_table,
               col_rename = 'ra'))
(rarb_multi_portfolio <- left_join(
  portfolio_returns_monthly_multi, rb,
  by = "date"
))

rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.CAPM
  )

rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = SharpeRatio
  )

# 查看所有可以使用的函数
tq_performance_fun_options()

# table.Stats
# 返回基本统计量
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = table.Stats
  )

# table.CAPM
# 返回与CAPM相关的度量
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.CAPM
  )

# table.AnnualizedReturns
# 返回年度回报、标准差和夏普比率
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = table.AnnualizedReturns
  )

# table.Correlation
# 返回与基准收益的回报率
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.Correlation
  )

# table.DownsideRisk
# 返回下行风险表，以便在多个工具或基金之间进行比较
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.DownsideRisk
  )

# table.DownsideRiskRatio
# 返回每月下行风险、年度下行风险等指标
rarb_multi_portfolio %>% 
  tq_performance(Ra = ra,
                 Rb = NULL,
                 performance_fun = table.DownsideRiskRatio)

# table.HigherMoments
# 返回分布的高阶矩和协矩
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.HigherMoments
  )

# table.InformationRatio
# 返回追踪误差表、年度追踪误差和信息比率
rarb_multi_portfolio %>% 
  tq_performance(Ra = ra,
                 Rb = rb,
                 performance_fun = table.InformationRatio)
# table.Variability
# 返回平均绝对差异、月标准差和年标准差
rarb_multi_portfolio %>% 
  tq_performance(Ra = ra,
                 Rb = NULL,
                 performance_fun = table.Variability)

# 在险值
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = VaR
  )

# 夏普比率
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = SharpeRatio
  )

# 自定义tq_portfolio
portfolio_returns_monthly %>% 
  ggplot(aes(x = date, y = ra)) + 
  geom_bar(stat = "identity", fill = palette_light()[[3]],
           width = 20) + 
  labs(title = "Portfolio Returns",
       caption = "Shows an above-zero trend meaning positive returns",
       x = "", y = "Monthly Returns") + 
  geom_smooth(method = "lm") + 
  theme_tq(base_family = enfont) + 
  scale_color_tq() + 
  scale_y_continuous(labels = scales::percent)

ggsave("tidyquantperformance1.svg")

# 使用 wealth.index = TRUE 参数，我们可以观察10000元是如何增长的：
wts <- c(0.5, 0, 0.5)
portfolio_growth_monthly <- stock_returns_monthly %>% 
  tq_portfolio(
    assets_col = symbol,
    returns_col = ra,
    weights = wts,
    col_rename = "investment.growth",
    wealth.index = TRUE
  ) %>% 
  mutate(investment.growth = investment.growth * 10000)
portfolio_growth_monthly %>% 
  ggplot(aes(x = date, y = investment.growth)) + 
  geom_line(size = 1, color = palette_light()[3]) +
  labs(x = "", y = "Portfolio Value",
       title = "Portfolio Growth",
       caption = "Now we can really visualize performance!") + 
  geom_smooth(method = "loess") + 
  theme_tq(base_family = enfont) + 
  scale_color_tq() + 
  scale_y_continuous(labels = scales::dollar)

ggsave("tidyquantperformance2.svg")

# 下面再比较三个投资组合的增长情况
portfolio_growth_monthly_multi <- stock_returns_monthly_multi %>% 
  tq_portfolio(
    assets_col = symbol,
    returns_col = ra,
    weights = weights_table,
    col_rename = "investment.growth",
    wealth.index = TRUE
  ) %>% 
  mutate(investment.growth = investment.growth * 10000)
portfolio_growth_monthly_multi %>% 
  ggplot(aes(x = date, y = investment.growth, color = factor(portfolio))) + 
  geom_line(size = 2) + 
  labs(title = "Portfolio Growth",
       subtitle = "Comparing Multiple Portfolios",
       caption = "Portfolio 3 is a Standout!",
       x = "", 
       y = "Portfolio Value",
       color = "Portfolio") + 
  geom_smooth(method = "loess") + 
  theme_tq(base_family = enfont) + 
  scale_color_tq() +
  scale_y_continuous(labels = scales::dollar)

ggsave("tidyquantperformance3.svg")
# Performance Analysis with tidyquant
library(tidyquant)
(ra <- c("000001.SZ", "000002.SZ", "000004.SZ") %>% 
  tq_get(get = "stock.prices",
         from = "2010-01-01",
         to = "2015-12-31") %>% 
  group_by(symbol) %>% 
  tq_transmute(
    select = adjusted,
    mutate_fun = periodReturn,
    period = "monthly",
    col_rename = "ra"
  ))

# 下面我们使用深证100R指数得到基准价格：
library(Tushare)
api = pro_api('fd65bf1ee56215d0ce1a95d1fa3b453d97a1779438aca26c4b7937d1')
(rb <- api(api_name = "index_daily",
    ts_code = "399004.SZ",
    start_date = "20100101",
    end_date = "20151231") %>% 
  as_tibble() %>% 
  transmute(
    date = ymd(trade_date),
    symbol = as.character(ts_code),
    close = as.numeric(close)
  ) %>% 
  tq_transmute(
    select = close,
    mutate_fun = periodReturn,
    period = "monthly",
    col_rename = "rb"
  ))

# 合并两个数据集：
(rarb <- left_join(ra, rb, by = "date"))

# 使用 tq_performance 运行 CAPM 模型
(rarb_capm <- rarb %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.CAPM
  ))

# 我们比较关心增长测度和风险测度
rarb_capm %>% 
  dplyr::select(Alpha, Beta)

# tidyquant的工作流程
# 单独资产分析
# 查看 SharpeRatio 函数的参数：
args(SharpeRatio)

# Step 1A: 获取股票数据
(stock_prices <- c("000001.SZ", "000002.SZ", "000004.SZ") %>% 
  tq_get(
    get = "stock.prices",
    from = "2010-01-01",
    to = "2015-12-31"
  ))


(stock_returns_monthly <- stock_prices %>% 
  group_by(symbol) %>% 
  tq_transmute(
    select = adjusted,
    mutate_fun = periodReturn,
    period = "monthly",
    col_rename = "ra"
  ))

# Step4: 绩效分析
stock_returns_monthly %>% 
  tq_performance(Ra = ra,
                 Rb = NULL,
                 performance_fun = SharpeRatio)

# 修改无风险利率和置信水平
stock_returns_monthly %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = SharpeRatio,
    Rf = 0.03 / 12,
    p = 0.99
  )

# 资产组合的分析
# 组合资产
# 方法1: 使用权重向量
wts <- c(0.5, 0, 0.5)
(portfolio_returns_monthly <- stock_returns_monthly %>% 
  tq_portfolio(
    assets_col = symbol,
    returns_col = ra,
    weights = wts,
    col_rename = "ra"
  ))

# 方法2: 使用资产 + 权重 数据框
(wts_map <- tibble(
  symbols = c('000001.SZ', '000004.SZ'),
  weights = c(0.5, 0.5)
))

stock_returns_monthly %>% 
  tq_portfolio(
    assets_col = symbol,
    returns_col = ra,
    weights = wts_map,
    col_rename = "ra_using_wts_map"
  )

# 合并ra和rb
(rarb_single_portfolio <- left_join(portfolio_returns_monthly,
                                   rb,
                                   by = "date"))
# 计算 CAPM 模型
rarb_single_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.CAPM
  )

# 多个资产组合
(stock_returns_monthly_multi <- stock_returns_monthly %>% 
  tq_repeat_df(n = 3))
weights <- c(
  0.5, 0.25, 0.25,
  0.25, 0.5, 0.25,
  0.25, 0.25, 0.5
)
stocks = c('000001.SZ', '000002.SZ', '000004.SZ')
(weights_table <- tibble(stocks) %>% 
  tq_repeat_df(3) %>% 
  bind_cols(tibble(weights)) %>% 
  group_by(portfolio))
(portfolio_returns_monthly_multi <- stock_returns_monthly_multi %>% 
  tq_portfolio(assets_col = symbol,
               returns_col = ra,
               weights = weights_table,
               col_rename = 'ra'))
(rarb_multi_portfolio <- left_join(
  portfolio_returns_monthly_multi, rb,
  by = "date"
))

rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.CAPM
  )

rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = SharpeRatio
  )

# 查看所有可以使用的函数
tq_performance_fun_options()

# table.Stats
# 返回基本统计量
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = table.Stats
  )

# table.CAPM
# 返回与CAPM相关的度量
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.CAPM
  )

# table.AnnualizedReturns
# 返回年度回报、标准差和夏普比率
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = table.AnnualizedReturns
  )

# table.Correlation
# 返回与基准收益的回报率
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.Correlation
  )

# table.DownsideRisk
# 返回下行风险表，以便在多个工具或基金之间进行比较
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.DownsideRisk
  )

# table.DownsideRiskRatio
# 返回每月下行风险、年度下行风险等指标
rarb_multi_portfolio %>% 
  tq_performance(Ra = ra,
                 Rb = NULL,
                 performance_fun = table.DownsideRiskRatio)

# table.HigherMoments
# 返回分布的高阶矩和协矩
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.HigherMoments
  )

# table.InformationRatio
# 返回追踪误差表、年度追踪误差和信息比率
rarb_multi_portfolio %>% 
  tq_performance(Ra = ra,
                 Rb = rb,
                 performance_fun = table.InformationRatio)
# table.Variability
# 返回平均绝对差异、月标准差和年标准差
rarb_multi_portfolio %>% 
  tq_performance(Ra = ra,
                 Rb = NULL,
                 performance_fun = table.Variability)

# 在险值
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = VaR
  )

# 夏普比率
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = SharpeRatio
  )

# 自定义tq_portfolio
portfolio_returns_monthly %>% 
  ggplot(aes(x = date, y = ra)) + 
  geom_bar(stat = "identity", fill = palette_light()[[3]],
           width = 20) + 
  labs(title = "Portfolio Returns",
       caption = "Shows an above-zero trend meaning positive returns",
       x = "", y = "Monthly Returns") + 
  geom_smooth(method = "lm") + 
  theme_tq(base_family = enfont) + 
  scale_color_tq() + 
  scale_y_continuous(labels = scales::percent)

ggsave("tidyquantperformance1.svg")

# 使用 wealth.index = TRUE 参数，我们可以观察10000元是如何增长的：
wts <- c(0.5, 0, 0.5)
portfolio_growth_monthly <- stock_returns_monthly %>% 
  tq_portfolio(
    assets_col = symbol,
    returns_col = ra,
    weights = wts,
    col_rename = "investment.growth",
    wealth.index = TRUE
  ) %>% 
  mutate(investment.growth = investment.growth * 10000)
portfolio_growth_monthly %>% 
  ggplot(aes(x = date, y = investment.growth)) + 
  geom_line(size = 1, color = palette_light()[3]) +
  labs(x = "", y = "Portfolio Value",
       title = "Portfolio Growth",
       caption = "Now we can really visualize performance!") + 
  geom_smooth(method = "loess") + 
  theme_tq(base_family = enfont) + 
  scale_color_tq() + 
  scale_y_continuous(labels = scales::dollar)

ggsave("tidyquantperformance2.svg")

# 下面再比较三个投资组合的增长情况
portfolio_growth_monthly_multi <- stock_returns_monthly_multi %>% 
  tq_portfolio(
    assets_col = symbol,
    returns_col = ra,
    weights = weights_table,
    col_rename = "investment.growth",
    wealth.index = TRUE
  ) %>% 
  mutate(investment.growth = investment.growth * 10000)
portfolio_growth_monthly_multi %>% 
  ggplot(aes(x = date, y = investment.growth, color = factor(portfolio))) + 
  geom_line(size = 2) + 
  labs(title = "Portfolio Growth",
       subtitle = "Comparing Multiple Portfolios",
       caption = "Portfolio 3 is a Standout!",
       x = "", 
       y = "Portfolio Value",
       color = "Portfolio") + 
  geom_smooth(method = "loess") + 
  theme_tq(base_family = enfont) + 
  scale_color_tq() +
  scale_y_continuous(labels = scales::dollar)

ggsave("tidyquantperformance3.svg")
# Performance Analysis with tidyquant
library(tidyquant)
(ra <- c("000001.SZ", "000002.SZ", "000004.SZ") %>% 
  tq_get(get = "stock.prices",
         from = "2010-01-01",
         to = "2015-12-31") %>% 
  group_by(symbol) %>% 
  tq_transmute(
    select = adjusted,
    mutate_fun = periodReturn,
    period = "monthly",
    col_rename = "ra"
  ))

# 下面我们使用深证100R指数得到基准价格：
library(Tushare)
api = pro_api('fd65bf1ee56215d0ce1a95d1fa3b453d97a1779438aca26c4b7937d1')
(rb <- api(api_name = "index_daily",
    ts_code = "399004.SZ",
    start_date = "20100101",
    end_date = "20151231") %>% 
  as_tibble() %>% 
  transmute(
    date = ymd(trade_date),
    symbol = as.character(ts_code),
    close = as.numeric(close)
  ) %>% 
  tq_transmute(
    select = close,
    mutate_fun = periodReturn,
    period = "monthly",
    col_rename = "rb"
  ))

# 合并两个数据集：
(rarb <- left_join(ra, rb, by = "date"))

# 使用 tq_performance 运行 CAPM 模型
(rarb_capm <- rarb %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.CAPM
  ))

# 我们比较关心增长测度和风险测度
rarb_capm %>% 
  dplyr::select(Alpha, Beta)

# tidyquant的工作流程
# 单独资产分析
# 查看 SharpeRatio 函数的参数：
args(SharpeRatio)

# Step 1A: 获取股票数据
(stock_prices <- c("000001.SZ", "000002.SZ", "000004.SZ") %>% 
  tq_get(
    get = "stock.prices",
    from = "2010-01-01",
    to = "2015-12-31"
  ))


(stock_returns_monthly <- stock_prices %>% 
  group_by(symbol) %>% 
  tq_transmute(
    select = adjusted,
    mutate_fun = periodReturn,
    period = "monthly",
    col_rename = "ra"
  ))

# Step4: 绩效分析
stock_returns_monthly %>% 
  tq_performance(Ra = ra,
                 Rb = NULL,
                 performance_fun = SharpeRatio)

# 修改无风险利率和置信水平
stock_returns_monthly %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = SharpeRatio,
    Rf = 0.03 / 12,
    p = 0.99
  )

# 资产组合的分析
# 组合资产
# 方法1: 使用权重向量
wts <- c(0.5, 0, 0.5)
(portfolio_returns_monthly <- stock_returns_monthly %>% 
  tq_portfolio(
    assets_col = symbol,
    returns_col = ra,
    weights = wts,
    col_rename = "ra"
  ))

# 方法2: 使用资产 + 权重 数据框
(wts_map <- tibble(
  symbols = c('000001.SZ', '000004.SZ'),
  weights = c(0.5, 0.5)
))

stock_returns_monthly %>% 
  tq_portfolio(
    assets_col = symbol,
    returns_col = ra,
    weights = wts_map,
    col_rename = "ra_using_wts_map"
  )

# 合并ra和rb
(rarb_single_portfolio <- left_join(portfolio_returns_monthly,
                                   rb,
                                   by = "date"))
# 计算 CAPM 模型
rarb_single_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.CAPM
  )

# 多个资产组合
(stock_returns_monthly_multi <- stock_returns_monthly %>% 
  tq_repeat_df(n = 3))
weights <- c(
  0.5, 0.25, 0.25,
  0.25, 0.5, 0.25,
  0.25, 0.25, 0.5
)
stocks = c('000001.SZ', '000002.SZ', '000004.SZ')
(weights_table <- tibble(stocks) %>% 
  tq_repeat_df(3) %>% 
  bind_cols(tibble(weights)) %>% 
  group_by(portfolio))
(portfolio_returns_monthly_multi <- stock_returns_monthly_multi %>% 
  tq_portfolio(assets_col = symbol,
               returns_col = ra,
               weights = weights_table,
               col_rename = 'ra'))
(rarb_multi_portfolio <- left_join(
  portfolio_returns_monthly_multi, rb,
  by = "date"
))

rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.CAPM
  )

rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = SharpeRatio
  )

# 查看所有可以使用的函数
tq_performance_fun_options()

# table.Stats
# 返回基本统计量
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = table.Stats
  )

# table.CAPM
# 返回与CAPM相关的度量
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.CAPM
  )

# table.AnnualizedReturns
# 返回年度回报、标准差和夏普比率
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = table.AnnualizedReturns
  )

# table.Correlation
# 返回与基准收益的回报率
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.Correlation
  )

# table.DownsideRisk
# 返回下行风险表，以便在多个工具或基金之间进行比较
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.DownsideRisk
  )

# table.DownsideRiskRatio
# 返回每月下行风险、年度下行风险等指标
rarb_multi_portfolio %>% 
  tq_performance(Ra = ra,
                 Rb = NULL,
                 performance_fun = table.DownsideRiskRatio)

# table.HigherMoments
# 返回分布的高阶矩和协矩
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.HigherMoments
  )

# table.InformationRatio
# 返回追踪误差表、年度追踪误差和信息比率
rarb_multi_portfolio %>% 
  tq_performance(Ra = ra,
                 Rb = rb,
                 performance_fun = table.InformationRatio)
# table.Variability
# 返回平均绝对差异、月标准差和年标准差
rarb_multi_portfolio %>% 
  tq_performance(Ra = ra,
                 Rb = NULL,
                 performance_fun = table.Variability)

# 在险值
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = VaR
  )

# 夏普比率
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = SharpeRatio
  )

# 自定义tq_portfolio
portfolio_returns_monthly %>% 
  ggplot(aes(x = date, y = ra)) + 
  geom_bar(stat = "identity", fill = palette_light()[[3]],
           width = 20) + 
  labs(title = "Portfolio Returns",
       caption = "Shows an above-zero trend meaning positive returns",
       x = "", y = "Monthly Returns") + 
  geom_smooth(method = "lm") + 
  theme_tq(base_family = enfont) + 
  scale_color_tq() + 
  scale_y_continuous(labels = scales::percent)

ggsave("tidyquantperformance1.svg")

# 使用 wealth.index = TRUE 参数，我们可以观察10000元是如何增长的：
wts <- c(0.5, 0, 0.5)
portfolio_growth_monthly <- stock_returns_monthly %>% 
  tq_portfolio(
    assets_col = symbol,
    returns_col = ra,
    weights = wts,
    col_rename = "investment.growth",
    wealth.index = TRUE
  ) %>% 
  mutate(investment.growth = investment.growth * 10000)
portfolio_growth_monthly %>% 
  ggplot(aes(x = date, y = investment.growth)) + 
  geom_line(size = 1, color = palette_light()[3]) +
  labs(x = "", y = "Portfolio Value",
       title = "Portfolio Growth",
       caption = "Now we can really visualize performance!") + 
  geom_smooth(method = "loess") + 
  theme_tq(base_family = enfont) + 
  scale_color_tq() + 
  scale_y_continuous(labels = scales::dollar)

ggsave("tidyquantperformance2.svg")

# 下面再比较三个投资组合的增长情况
portfolio_growth_monthly_multi <- stock_returns_monthly_multi %>% 
  tq_portfolio(
    assets_col = symbol,
    returns_col = ra,
    weights = weights_table,
    col_rename = "investment.growth",
    wealth.index = TRUE
  ) %>% 
  mutate(investment.growth = investment.growth * 10000)
portfolio_growth_monthly_multi %>% 
  ggplot(aes(x = date, y = investment.growth, color = factor(portfolio))) + 
  geom_line(size = 2) + 
  labs(title = "Portfolio Growth",
       subtitle = "Comparing Multiple Portfolios",
       caption = "Portfolio 3 is a Standout!",
       x = "", 
       y = "Portfolio Value",
       color = "Portfolio") + 
  geom_smooth(method = "loess") + 
  theme_tq(base_family = enfont) + 
  scale_color_tq() +
  scale_y_continuous(labels = scales::dollar)

ggsave("tidyquantperformance3.svg")
# Performance Analysis with tidyquant
library(tidyquant)
(ra <- c("000001.SZ", "000002.SZ", "000004.SZ") %>% 
  tq_get(get = "stock.prices",
         from = "2010-01-01",
         to = "2015-12-31") %>% 
  group_by(symbol) %>% 
  tq_transmute(
    select = adjusted,
    mutate_fun = periodReturn,
    period = "monthly",
    col_rename = "ra"
  ))

# 下面我们使用深证100R指数得到基准价格：
library(Tushare)
api = pro_api('fd65bf1ee56215d0ce1a95d1fa3b453d97a1779438aca26c4b7937d1')
(rb <- api(api_name = "index_daily",
    ts_code = "399004.SZ",
    start_date = "20100101",
    end_date = "20151231") %>% 
  as_tibble() %>% 
  transmute(
    date = ymd(trade_date),
    symbol = as.character(ts_code),
    close = as.numeric(close)
  ) %>% 
  tq_transmute(
    select = close,
    mutate_fun = periodReturn,
    period = "monthly",
    col_rename = "rb"
  ))

# 合并两个数据集：
(rarb <- left_join(ra, rb, by = "date"))

# 使用 tq_performance 运行 CAPM 模型
(rarb_capm <- rarb %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.CAPM
  ))

# 我们比较关心增长测度和风险测度
rarb_capm %>% 
  dplyr::select(Alpha, Beta)

# tidyquant的工作流程
# 单独资产分析
# 查看 SharpeRatio 函数的参数：
args(SharpeRatio)

# Step 1A: 获取股票数据
(stock_prices <- c("000001.SZ", "000002.SZ", "000004.SZ") %>% 
  tq_get(
    get = "stock.prices",
    from = "2010-01-01",
    to = "2015-12-31"
  ))


(stock_returns_monthly <- stock_prices %>% 
  group_by(symbol) %>% 
  tq_transmute(
    select = adjusted,
    mutate_fun = periodReturn,
    period = "monthly",
    col_rename = "ra"
  ))

# Step4: 绩效分析
stock_returns_monthly %>% 
  tq_performance(Ra = ra,
                 Rb = NULL,
                 performance_fun = SharpeRatio)

# 修改无风险利率和置信水平
stock_returns_monthly %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = SharpeRatio,
    Rf = 0.03 / 12,
    p = 0.99
  )

# 资产组合的分析
# 组合资产
# 方法1: 使用权重向量
wts <- c(0.5, 0, 0.5)
(portfolio_returns_monthly <- stock_returns_monthly %>% 
  tq_portfolio(
    assets_col = symbol,
    returns_col = ra,
    weights = wts,
    col_rename = "ra"
  ))

# 方法2: 使用资产 + 权重 数据框
(wts_map <- tibble(
  symbols = c('000001.SZ', '000004.SZ'),
  weights = c(0.5, 0.5)
))

stock_returns_monthly %>% 
  tq_portfolio(
    assets_col = symbol,
    returns_col = ra,
    weights = wts_map,
    col_rename = "ra_using_wts_map"
  )

# 合并ra和rb
(rarb_single_portfolio <- left_join(portfolio_returns_monthly,
                                   rb,
                                   by = "date"))
# 计算 CAPM 模型
rarb_single_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.CAPM
  )

# 多个资产组合
(stock_returns_monthly_multi <- stock_returns_monthly %>% 
  tq_repeat_df(n = 3))
weights <- c(
  0.5, 0.25, 0.25,
  0.25, 0.5, 0.25,
  0.25, 0.25, 0.5
)
stocks = c('000001.SZ', '000002.SZ', '000004.SZ')
(weights_table <- tibble(stocks) %>% 
  tq_repeat_df(3) %>% 
  bind_cols(tibble(weights)) %>% 
  group_by(portfolio))
(portfolio_returns_monthly_multi <- stock_returns_monthly_multi %>% 
  tq_portfolio(assets_col = symbol,
               returns_col = ra,
               weights = weights_table,
               col_rename = 'ra'))
(rarb_multi_portfolio <- left_join(
  portfolio_returns_monthly_multi, rb,
  by = "date"
))

rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.CAPM
  )

rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = SharpeRatio
  )

# 查看所有可以使用的函数
tq_performance_fun_options()

# table.Stats
# 返回基本统计量
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = table.Stats
  )

# table.CAPM
# 返回与CAPM相关的度量
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.CAPM
  )

# table.AnnualizedReturns
# 返回年度回报、标准差和夏普比率
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = table.AnnualizedReturns
  )

# table.Correlation
# 返回与基准收益的回报率
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.Correlation
  )

# table.DownsideRisk
# 返回下行风险表，以便在多个工具或基金之间进行比较
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.DownsideRisk
  )

# table.DownsideRiskRatio
# 返回每月下行风险、年度下行风险等指标
rarb_multi_portfolio %>% 
  tq_performance(Ra = ra,
                 Rb = NULL,
                 performance_fun = table.DownsideRiskRatio)

# table.HigherMoments
# 返回分布的高阶矩和协矩
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.HigherMoments
  )

# table.InformationRatio
# 返回追踪误差表、年度追踪误差和信息比率
rarb_multi_portfolio %>% 
  tq_performance(Ra = ra,
                 Rb = rb,
                 performance_fun = table.InformationRatio)
# table.Variability
# 返回平均绝对差异、月标准差和年标准差
rarb_multi_portfolio %>% 
  tq_performance(Ra = ra,
                 Rb = NULL,
                 performance_fun = table.Variability)

# 在险值
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = VaR
  )

# 夏普比率
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = SharpeRatio
  )

# 自定义tq_portfolio
portfolio_returns_monthly %>% 
  ggplot(aes(x = date, y = ra)) + 
  geom_bar(stat = "identity", fill = palette_light()[[3]],
           width = 20) + 
  labs(title = "Portfolio Returns",
       caption = "Shows an above-zero trend meaning positive returns",
       x = "", y = "Monthly Returns") + 
  geom_smooth(method = "lm") + 
  theme_tq(base_family = enfont) + 
  scale_color_tq() + 
  scale_y_continuous(labels = scales::percent)

ggsave("tidyquantperformance1.svg")

# 使用 wealth.index = TRUE 参数，我们可以观察10000元是如何增长的：
wts <- c(0.5, 0, 0.5)
portfolio_growth_monthly <- stock_returns_monthly %>% 
  tq_portfolio(
    assets_col = symbol,
    returns_col = ra,
    weights = wts,
    col_rename = "investment.growth",
    wealth.index = TRUE
  ) %>% 
  mutate(investment.growth = investment.growth * 10000)
portfolio_growth_monthly %>% 
  ggplot(aes(x = date, y = investment.growth)) + 
  geom_line(size = 1, color = palette_light()[3]) +
  labs(x = "", y = "Portfolio Value",
       title = "Portfolio Growth",
       caption = "Now we can really visualize performance!") + 
  geom_smooth(method = "loess") + 
  theme_tq(base_family = enfont) + 
  scale_color_tq() + 
  scale_y_continuous(labels = scales::dollar)

ggsave("tidyquantperformance2.svg")

# 下面再比较三个投资组合的增长情况
portfolio_growth_monthly_multi <- stock_returns_monthly_multi %>% 
  tq_portfolio(
    assets_col = symbol,
    returns_col = ra,
    weights = weights_table,
    col_rename = "investment.growth",
    wealth.index = TRUE
  ) %>% 
  mutate(investment.growth = investment.growth * 10000)
portfolio_growth_monthly_multi %>% 
  ggplot(aes(x = date, y = investment.growth, color = factor(portfolio))) + 
  geom_line(size = 2) + 
  labs(title = "Portfolio Growth",
       subtitle = "Comparing Multiple Portfolios",
       caption = "Portfolio 3 is a Standout!",
       x = "", 
       y = "Portfolio Value",
       color = "Portfolio") + 
  geom_smooth(method = "loess") + 
  theme_tq(base_family = enfont) + 
  scale_color_tq() +
  scale_y_continuous(labels = scales::dollar)

ggsave("tidyquantperformance3.svg")
# Performance Analysis with tidyquant
library(tidyquant)
(ra <- c("000001.SZ", "000002.SZ", "000004.SZ") %>% 
  tq_get(get = "stock.prices",
         from = "2010-01-01",
         to = "2015-12-31") %>% 
  group_by(symbol) %>% 
  tq_transmute(
    select = adjusted,
    mutate_fun = periodReturn,
    period = "monthly",
    col_rename = "ra"
  ))

# 下面我们使用深证100R指数得到基准价格：
library(Tushare)
api = pro_api('fd65bf1ee56215d0ce1a95d1fa3b453d97a1779438aca26c4b7937d1')
(rb <- api(api_name = "index_daily",
    ts_code = "399004.SZ",
    start_date = "20100101",
    end_date = "20151231") %>% 
  as_tibble() %>% 
  transmute(
    date = ymd(trade_date),
    symbol = as.character(ts_code),
    close = as.numeric(close)
  ) %>% 
  tq_transmute(
    select = close,
    mutate_fun = periodReturn,
    period = "monthly",
    col_rename = "rb"
  ))

# 合并两个数据集：
(rarb <- left_join(ra, rb, by = "date"))

# 使用 tq_performance 运行 CAPM 模型
(rarb_capm <- rarb %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.CAPM
  ))

# 我们比较关心增长测度和风险测度
rarb_capm %>% 
  dplyr::select(Alpha, Beta)

# tidyquant的工作流程
# 单独资产分析
# 查看 SharpeRatio 函数的参数：
args(SharpeRatio)

# Step 1A: 获取股票数据
(stock_prices <- c("000001.SZ", "000002.SZ", "000004.SZ") %>% 
  tq_get(
    get = "stock.prices",
    from = "2010-01-01",
    to = "2015-12-31"
  ))


(stock_returns_monthly <- stock_prices %>% 
  group_by(symbol) %>% 
  tq_transmute(
    select = adjusted,
    mutate_fun = periodReturn,
    period = "monthly",
    col_rename = "ra"
  ))

# Step4: 绩效分析
stock_returns_monthly %>% 
  tq_performance(Ra = ra,
                 Rb = NULL,
                 performance_fun = SharpeRatio)

# 修改无风险利率和置信水平
stock_returns_monthly %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = SharpeRatio,
    Rf = 0.03 / 12,
    p = 0.99
  )

# 资产组合的分析
# 组合资产
# 方法1: 使用权重向量
wts <- c(0.5, 0, 0.5)
(portfolio_returns_monthly <- stock_returns_monthly %>% 
  tq_portfolio(
    assets_col = symbol,
    returns_col = ra,
    weights = wts,
    col_rename = "ra"
  ))

# 方法2: 使用资产 + 权重 数据框
(wts_map <- tibble(
  symbols = c('000001.SZ', '000004.SZ'),
  weights = c(0.5, 0.5)
))

stock_returns_monthly %>% 
  tq_portfolio(
    assets_col = symbol,
    returns_col = ra,
    weights = wts_map,
    col_rename = "ra_using_wts_map"
  )

# 合并ra和rb
(rarb_single_portfolio <- left_join(portfolio_returns_monthly,
                                   rb,
                                   by = "date"))
# 计算 CAPM 模型
rarb_single_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.CAPM
  )

# 多个资产组合
(stock_returns_monthly_multi <- stock_returns_monthly %>% 
  tq_repeat_df(n = 3))
weights <- c(
  0.5, 0.25, 0.25,
  0.25, 0.5, 0.25,
  0.25, 0.25, 0.5
)
stocks = c('000001.SZ', '000002.SZ', '000004.SZ')
(weights_table <- tibble(stocks) %>% 
  tq_repeat_df(3) %>% 
  bind_cols(tibble(weights)) %>% 
  group_by(portfolio))
(portfolio_returns_monthly_multi <- stock_returns_monthly_multi %>% 
  tq_portfolio(assets_col = symbol,
               returns_col = ra,
               weights = weights_table,
               col_rename = 'ra'))
(rarb_multi_portfolio <- left_join(
  portfolio_returns_monthly_multi, rb,
  by = "date"
))

rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.CAPM
  )

rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = SharpeRatio
  )

# 查看所有可以使用的函数
tq_performance_fun_options()

# table.Stats
# 返回基本统计量
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = table.Stats
  )

# table.CAPM
# 返回与CAPM相关的度量
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.CAPM
  )

# table.AnnualizedReturns
# 返回年度回报、标准差和夏普比率
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = table.AnnualizedReturns
  )

# table.Correlation
# 返回与基准收益的回报率
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.Correlation
  )

# table.DownsideRisk
# 返回下行风险表，以便在多个工具或基金之间进行比较
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.DownsideRisk
  )

# table.DownsideRiskRatio
# 返回每月下行风险、年度下行风险等指标
rarb_multi_portfolio %>% 
  tq_performance(Ra = ra,
                 Rb = NULL,
                 performance_fun = table.DownsideRiskRatio)

# table.HigherMoments
# 返回分布的高阶矩和协矩
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.HigherMoments
  )

# table.InformationRatio
# 返回追踪误差表、年度追踪误差和信息比率
rarb_multi_portfolio %>% 
  tq_performance(Ra = ra,
                 Rb = rb,
                 performance_fun = table.InformationRatio)
# table.Variability
# 返回平均绝对差异、月标准差和年标准差
rarb_multi_portfolio %>% 
  tq_performance(Ra = ra,
                 Rb = NULL,
                 performance_fun = table.Variability)

# 在险值
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = VaR
  )

# 夏普比率
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = SharpeRatio
  )

# 自定义tq_portfolio
portfolio_returns_monthly %>% 
  ggplot(aes(x = date, y = ra)) + 
  geom_bar(stat = "identity", fill = palette_light()[[3]],
           width = 20) + 
  labs(title = "Portfolio Returns",
       caption = "Shows an above-zero trend meaning positive returns",
       x = "", y = "Monthly Returns") + 
  geom_smooth(method = "lm") + 
  theme_tq(base_family = enfont) + 
  scale_color_tq() + 
  scale_y_continuous(labels = scales::percent)

ggsave("tidyquantperformance1.svg")

# 使用 wealth.index = TRUE 参数，我们可以观察10000元是如何增长的：
wts <- c(0.5, 0, 0.5)
portfolio_growth_monthly <- stock_returns_monthly %>% 
  tq_portfolio(
    assets_col = symbol,
    returns_col = ra,
    weights = wts,
    col_rename = "investment.growth",
    wealth.index = TRUE
  ) %>% 
  mutate(investment.growth = investment.growth * 10000)
portfolio_growth_monthly %>% 
  ggplot(aes(x = date, y = investment.growth)) + 
  geom_line(size = 1, color = palette_light()[3]) +
  labs(x = "", y = "Portfolio Value",
       title = "Portfolio Growth",
       caption = "Now we can really visualize performance!") + 
  geom_smooth(method = "loess") + 
  theme_tq(base_family = enfont) + 
  scale_color_tq() + 
  scale_y_continuous(labels = scales::dollar)

ggsave("tidyquantperformance2.svg")

# 下面再比较三个投资组合的增长情况
portfolio_growth_monthly_multi <- stock_returns_monthly_multi %>% 
  tq_portfolio(
    assets_col = symbol,
    returns_col = ra,
    weights = weights_table,
    col_rename = "investment.growth",
    wealth.index = TRUE
  ) %>% 
  mutate(investment.growth = investment.growth * 10000)
portfolio_growth_monthly_multi %>% 
  ggplot(aes(x = date, y = investment.growth, color = factor(portfolio))) + 
  geom_line(size = 2) + 
  labs(title = "Portfolio Growth",
       subtitle = "Comparing Multiple Portfolios",
       caption = "Portfolio 3 is a Standout!",
       x = "", 
       y = "Portfolio Value",
       color = "Portfolio") + 
  geom_smooth(method = "loess") + 
  theme_tq(base_family = enfont) + 
  scale_color_tq() +
  scale_y_continuous(labels = scales::dollar)

ggsave("tidyquantperformance3.svg")
# Performance Analysis with tidyquant
library(tidyquant)
(ra <- c("000001.SZ", "000002.SZ", "000004.SZ") %>% 
  tq_get(get = "stock.prices",
         from = "2010-01-01",
         to = "2015-12-31") %>% 
  group_by(symbol) %>% 
  tq_transmute(
    select = adjusted,
    mutate_fun = periodReturn,
    period = "monthly",
    col_rename = "ra"
  ))

# 下面我们使用深证100R指数得到基准价格：
library(Tushare)
api = pro_api('fd65bf1ee56215d0ce1a95d1fa3b453d97a1779438aca26c4b7937d1')
(rb <- api(api_name = "index_daily",
    ts_code = "399004.SZ",
    start_date = "20100101",
    end_date = "20151231") %>% 
  as_tibble() %>% 
  transmute(
    date = ymd(trade_date),
    symbol = as.character(ts_code),
    close = as.numeric(close)
  ) %>% 
  tq_transmute(
    select = close,
    mutate_fun = periodReturn,
    period = "monthly",
    col_rename = "rb"
  ))

# 合并两个数据集：
(rarb <- left_join(ra, rb, by = "date"))

# 使用 tq_performance 运行 CAPM 模型
(rarb_capm <- rarb %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.CAPM
  ))

# 我们比较关心增长测度和风险测度
rarb_capm %>% 
  dplyr::select(Alpha, Beta)

# tidyquant的工作流程
# 单独资产分析
# 查看 SharpeRatio 函数的参数：
args(SharpeRatio)

# Step 1A: 获取股票数据
(stock_prices <- c("000001.SZ", "000002.SZ", "000004.SZ") %>% 
  tq_get(
    get = "stock.prices",
    from = "2010-01-01",
    to = "2015-12-31"
  ))


(stock_returns_monthly <- stock_prices %>% 
  group_by(symbol) %>% 
  tq_transmute(
    select = adjusted,
    mutate_fun = periodReturn,
    period = "monthly",
    col_rename = "ra"
  ))

# Step4: 绩效分析
stock_returns_monthly %>% 
  tq_performance(Ra = ra,
                 Rb = NULL,
                 performance_fun = SharpeRatio)

# 修改无风险利率和置信水平
stock_returns_monthly %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = SharpeRatio,
    Rf = 0.03 / 12,
    p = 0.99
  )

# 资产组合的分析
# 组合资产
# 方法1: 使用权重向量
wts <- c(0.5, 0, 0.5)
(portfolio_returns_monthly <- stock_returns_monthly %>% 
  tq_portfolio(
    assets_col = symbol,
    returns_col = ra,
    weights = wts,
    col_rename = "ra"
  ))

# 方法2: 使用资产 + 权重 数据框
(wts_map <- tibble(
  symbols = c('000001.SZ', '000004.SZ'),
  weights = c(0.5, 0.5)
))

stock_returns_monthly %>% 
  tq_portfolio(
    assets_col = symbol,
    returns_col = ra,
    weights = wts_map,
    col_rename = "ra_using_wts_map"
  )

# 合并ra和rb
(rarb_single_portfolio <- left_join(portfolio_returns_monthly,
                                   rb,
                                   by = "date"))
# 计算 CAPM 模型
rarb_single_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.CAPM
  )

# 多个资产组合
(stock_returns_monthly_multi <- stock_returns_monthly %>% 
  tq_repeat_df(n = 3))
weights <- c(
  0.5, 0.25, 0.25,
  0.25, 0.5, 0.25,
  0.25, 0.25, 0.5
)
stocks = c('000001.SZ', '000002.SZ', '000004.SZ')
(weights_table <- tibble(stocks) %>% 
  tq_repeat_df(3) %>% 
  bind_cols(tibble(weights)) %>% 
  group_by(portfolio))
(portfolio_returns_monthly_multi <- stock_returns_monthly_multi %>% 
  tq_portfolio(assets_col = symbol,
               returns_col = ra,
               weights = weights_table,
               col_rename = 'ra'))
(rarb_multi_portfolio <- left_join(
  portfolio_returns_monthly_multi, rb,
  by = "date"
))

rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.CAPM
  )

rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = SharpeRatio
  )

# 查看所有可以使用的函数
tq_performance_fun_options()

# table.Stats
# 返回基本统计量
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = table.Stats
  )

# table.CAPM
# 返回与CAPM相关的度量
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.CAPM
  )

# table.AnnualizedReturns
# 返回年度回报、标准差和夏普比率
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = table.AnnualizedReturns
  )

# table.Correlation
# 返回与基准收益的回报率
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.Correlation
  )

# table.DownsideRisk
# 返回下行风险表，以便在多个工具或基金之间进行比较
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.DownsideRisk
  )

# table.DownsideRiskRatio
# 返回每月下行风险、年度下行风险等指标
rarb_multi_portfolio %>% 
  tq_performance(Ra = ra,
                 Rb = NULL,
                 performance_fun = table.DownsideRiskRatio)

# table.HigherMoments
# 返回分布的高阶矩和协矩
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.HigherMoments
  )

# table.InformationRatio
# 返回追踪误差表、年度追踪误差和信息比率
rarb_multi_portfolio %>% 
  tq_performance(Ra = ra,
                 Rb = rb,
                 performance_fun = table.InformationRatio)
# table.Variability
# 返回平均绝对差异、月标准差和年标准差
rarb_multi_portfolio %>% 
  tq_performance(Ra = ra,
                 Rb = NULL,
                 performance_fun = table.Variability)

# 在险值
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = VaR
  )

# 夏普比率
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = SharpeRatio
  )

# 自定义tq_portfolio
portfolio_returns_monthly %>% 
  ggplot(aes(x = date, y = ra)) + 
  geom_bar(stat = "identity", fill = palette_light()[[3]],
           width = 20) + 
  labs(title = "Portfolio Returns",
       caption = "Shows an above-zero trend meaning positive returns",
       x = "", y = "Monthly Returns") + 
  geom_smooth(method = "lm") + 
  theme_tq(base_family = enfont) + 
  scale_color_tq() + 
  scale_y_continuous(labels = scales::percent)

ggsave("tidyquantperformance1.svg")

# 使用 wealth.index = TRUE 参数，我们可以观察10000元是如何增长的：
wts <- c(0.5, 0, 0.5)
portfolio_growth_monthly <- stock_returns_monthly %>% 
  tq_portfolio(
    assets_col = symbol,
    returns_col = ra,
    weights = wts,
    col_rename = "investment.growth",
    wealth.index = TRUE
  ) %>% 
  mutate(investment.growth = investment.growth * 10000)
portfolio_growth_monthly %>% 
  ggplot(aes(x = date, y = investment.growth)) + 
  geom_line(size = 1, color = palette_light()[3]) +
  labs(x = "", y = "Portfolio Value",
       title = "Portfolio Growth",
       caption = "Now we can really visualize performance!") + 
  geom_smooth(method = "loess") + 
  theme_tq(base_family = enfont) + 
  scale_color_tq() + 
  scale_y_continuous(labels = scales::dollar)

ggsave("tidyquantperformance2.svg")

# 下面再比较三个投资组合的增长情况
portfolio_growth_monthly_multi <- stock_returns_monthly_multi %>% 
  tq_portfolio(
    assets_col = symbol,
    returns_col = ra,
    weights = weights_table,
    col_rename = "investment.growth",
    wealth.index = TRUE
  ) %>% 
  mutate(investment.growth = investment.growth * 10000)
portfolio_growth_monthly_multi %>% 
  ggplot(aes(x = date, y = investment.growth, color = factor(portfolio))) + 
  geom_line(size = 2) + 
  labs(title = "Portfolio Growth",
       subtitle = "Comparing Multiple Portfolios",
       caption = "Portfolio 3 is a Standout!",
       x = "", 
       y = "Portfolio Value",
       color = "Portfolio") + 
  geom_smooth(method = "loess") + 
  theme_tq(base_family = enfont) + 
  scale_color_tq() +
  scale_y_continuous(labels = scales::dollar)

ggsave("tidyquantperformance3.svg")
# Performance Analysis with tidyquant
library(tidyquant)
(ra <- c("000001.SZ", "000002.SZ", "000004.SZ") %>% 
  tq_get(get = "stock.prices",
         from = "2010-01-01",
         to = "2015-12-31") %>% 
  group_by(symbol) %>% 
  tq_transmute(
    select = adjusted,
    mutate_fun = periodReturn,
    period = "monthly",
    col_rename = "ra"
  ))

# 下面我们使用深证100R指数得到基准价格：
library(Tushare)
api = pro_api('fd65bf1ee56215d0ce1a95d1fa3b453d97a1779438aca26c4b7937d1')
(rb <- api(api_name = "index_daily",
    ts_code = "399004.SZ",
    start_date = "20100101",
    end_date = "20151231") %>% 
  as_tibble() %>% 
  transmute(
    date = ymd(trade_date),
    symbol = as.character(ts_code),
    close = as.numeric(close)
  ) %>% 
  tq_transmute(
    select = close,
    mutate_fun = periodReturn,
    period = "monthly",
    col_rename = "rb"
  ))

# 合并两个数据集：
(rarb <- left_join(ra, rb, by = "date"))

# 使用 tq_performance 运行 CAPM 模型
(rarb_capm <- rarb %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.CAPM
  ))

# 我们比较关心增长测度和风险测度
rarb_capm %>% 
  dplyr::select(Alpha, Beta)

# tidyquant的工作流程
# 单独资产分析
# 查看 SharpeRatio 函数的参数：
args(SharpeRatio)

# Step 1A: 获取股票数据
(stock_prices <- c("000001.SZ", "000002.SZ", "000004.SZ") %>% 
  tq_get(
    get = "stock.prices",
    from = "2010-01-01",
    to = "2015-12-31"
  ))


(stock_returns_monthly <- stock_prices %>% 
  group_by(symbol) %>% 
  tq_transmute(
    select = adjusted,
    mutate_fun = periodReturn,
    period = "monthly",
    col_rename = "ra"
  ))

# Step4: 绩效分析
stock_returns_monthly %>% 
  tq_performance(Ra = ra,
                 Rb = NULL,
                 performance_fun = SharpeRatio)

# 修改无风险利率和置信水平
stock_returns_monthly %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = SharpeRatio,
    Rf = 0.03 / 12,
    p = 0.99
  )

# 资产组合的分析
# 组合资产
# 方法1: 使用权重向量
wts <- c(0.5, 0, 0.5)
(portfolio_returns_monthly <- stock_returns_monthly %>% 
  tq_portfolio(
    assets_col = symbol,
    returns_col = ra,
    weights = wts,
    col_rename = "ra"
  ))

# 方法2: 使用资产 + 权重 数据框
(wts_map <- tibble(
  symbols = c('000001.SZ', '000004.SZ'),
  weights = c(0.5, 0.5)
))

stock_returns_monthly %>% 
  tq_portfolio(
    assets_col = symbol,
    returns_col = ra,
    weights = wts_map,
    col_rename = "ra_using_wts_map"
  )

# 合并ra和rb
(rarb_single_portfolio <- left_join(portfolio_returns_monthly,
                                   rb,
                                   by = "date"))
# 计算 CAPM 模型
rarb_single_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.CAPM
  )

# 多个资产组合
(stock_returns_monthly_multi <- stock_returns_monthly %>% 
  tq_repeat_df(n = 3))
weights <- c(
  0.5, 0.25, 0.25,
  0.25, 0.5, 0.25,
  0.25, 0.25, 0.5
)
stocks = c('000001.SZ', '000002.SZ', '000004.SZ')
(weights_table <- tibble(stocks) %>% 
  tq_repeat_df(3) %>% 
  bind_cols(tibble(weights)) %>% 
  group_by(portfolio))
(portfolio_returns_monthly_multi <- stock_returns_monthly_multi %>% 
  tq_portfolio(assets_col = symbol,
               returns_col = ra,
               weights = weights_table,
               col_rename = 'ra'))
(rarb_multi_portfolio <- left_join(
  portfolio_returns_monthly_multi, rb,
  by = "date"
))

rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.CAPM
  )

rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = SharpeRatio
  )

# 查看所有可以使用的函数
tq_performance_fun_options()

# table.Stats
# 返回基本统计量
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = table.Stats
  )

# table.CAPM
# 返回与CAPM相关的度量
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.CAPM
  )

# table.AnnualizedReturns
# 返回年度回报、标准差和夏普比率
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = table.AnnualizedReturns
  )

# table.Correlation
# 返回与基准收益的回报率
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.Correlation
  )

# table.DownsideRisk
# 返回下行风险表，以便在多个工具或基金之间进行比较
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.DownsideRisk
  )

# table.DownsideRiskRatio
# 返回每月下行风险、年度下行风险等指标
rarb_multi_portfolio %>% 
  tq_performance(Ra = ra,
                 Rb = NULL,
                 performance_fun = table.DownsideRiskRatio)

# table.HigherMoments
# 返回分布的高阶矩和协矩
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.HigherMoments
  )

# table.InformationRatio
# 返回追踪误差表、年度追踪误差和信息比率
rarb_multi_portfolio %>% 
  tq_performance(Ra = ra,
                 Rb = rb,
                 performance_fun = table.InformationRatio)
# table.Variability
# 返回平均绝对差异、月标准差和年标准差
rarb_multi_portfolio %>% 
  tq_performance(Ra = ra,
                 Rb = NULL,
                 performance_fun = table.Variability)

# 在险值
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = VaR
  )

# 夏普比率
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = SharpeRatio
  )

# 自定义tq_portfolio
portfolio_returns_monthly %>% 
  ggplot(aes(x = date, y = ra)) + 
  geom_bar(stat = "identity", fill = palette_light()[[3]],
           width = 20) + 
  labs(title = "Portfolio Returns",
       caption = "Shows an above-zero trend meaning positive returns",
       x = "", y = "Monthly Returns") + 
  geom_smooth(method = "lm") + 
  theme_tq(base_family = enfont) + 
  scale_color_tq() + 
  scale_y_continuous(labels = scales::percent)

ggsave("tidyquantperformance1.svg")

# 使用 wealth.index = TRUE 参数，我们可以观察10000元是如何增长的：
wts <- c(0.5, 0, 0.5)
portfolio_growth_monthly <- stock_returns_monthly %>% 
  tq_portfolio(
    assets_col = symbol,
    returns_col = ra,
    weights = wts,
    col_rename = "investment.growth",
    wealth.index = TRUE
  ) %>% 
  mutate(investment.growth = investment.growth * 10000)
portfolio_growth_monthly %>% 
  ggplot(aes(x = date, y = investment.growth)) + 
  geom_line(size = 1, color = palette_light()[3]) +
  labs(x = "", y = "Portfolio Value",
       title = "Portfolio Growth",
       caption = "Now we can really visualize performance!") + 
  geom_smooth(method = "loess") + 
  theme_tq(base_family = enfont) + 
  scale_color_tq() + 
  scale_y_continuous(labels = scales::dollar)

ggsave("tidyquantperformance2.svg")

# 下面再比较三个投资组合的增长情况
portfolio_growth_monthly_multi <- stock_returns_monthly_multi %>% 
  tq_portfolio(
    assets_col = symbol,
    returns_col = ra,
    weights = weights_table,
    col_rename = "investment.growth",
    wealth.index = TRUE
  ) %>% 
  mutate(investment.growth = investment.growth * 10000)
portfolio_growth_monthly_multi %>% 
  ggplot(aes(x = date, y = investment.growth, color = factor(portfolio))) + 
  geom_line(size = 2) + 
  labs(title = "Portfolio Growth",
       subtitle = "Comparing Multiple Portfolios",
       caption = "Portfolio 3 is a Standout!",
       x = "", 
       y = "Portfolio Value",
       color = "Portfolio") + 
  geom_smooth(method = "loess") + 
  theme_tq(base_family = enfont) + 
  scale_color_tq() +
  scale_y_continuous(labels = scales::dollar)

ggsave("tidyquantperformance3.svg")
# Performance Analysis with tidyquant
library(tidyquant)
(ra <- c("000001.SZ", "000002.SZ", "000004.SZ") %>% 
  tq_get(get = "stock.prices",
         from = "2010-01-01",
         to = "2015-12-31") %>% 
  group_by(symbol) %>% 
  tq_transmute(
    select = adjusted,
    mutate_fun = periodReturn,
    period = "monthly",
    col_rename = "ra"
  ))

# 下面我们使用深证100R指数得到基准价格：
library(Tushare)
api = pro_api('fd65bf1ee56215d0ce1a95d1fa3b453d97a1779438aca26c4b7937d1')
(rb <- api(api_name = "index_daily",
    ts_code = "399004.SZ",
    start_date = "20100101",
    end_date = "20151231") %>% 
  as_tibble() %>% 
  transmute(
    date = ymd(trade_date),
    symbol = as.character(ts_code),
    close = as.numeric(close)
  ) %>% 
  tq_transmute(
    select = close,
    mutate_fun = periodReturn,
    period = "monthly",
    col_rename = "rb"
  ))

# 合并两个数据集：
(rarb <- left_join(ra, rb, by = "date"))

# 使用 tq_performance 运行 CAPM 模型
(rarb_capm <- rarb %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.CAPM
  ))

# 我们比较关心增长测度和风险测度
rarb_capm %>% 
  dplyr::select(Alpha, Beta)

# tidyquant的工作流程
# 单独资产分析
# 查看 SharpeRatio 函数的参数：
args(SharpeRatio)

# Step 1A: 获取股票数据
(stock_prices <- c("000001.SZ", "000002.SZ", "000004.SZ") %>% 
  tq_get(
    get = "stock.prices",
    from = "2010-01-01",
    to = "2015-12-31"
  ))


(stock_returns_monthly <- stock_prices %>% 
  group_by(symbol) %>% 
  tq_transmute(
    select = adjusted,
    mutate_fun = periodReturn,
    period = "monthly",
    col_rename = "ra"
  ))

# Step4: 绩效分析
stock_returns_monthly %>% 
  tq_performance(Ra = ra,
                 Rb = NULL,
                 performance_fun = SharpeRatio)

# 修改无风险利率和置信水平
stock_returns_monthly %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = SharpeRatio,
    Rf = 0.03 / 12,
    p = 0.99
  )

# 资产组合的分析
# 组合资产
# 方法1: 使用权重向量
wts <- c(0.5, 0, 0.5)
(portfolio_returns_monthly <- stock_returns_monthly %>% 
  tq_portfolio(
    assets_col = symbol,
    returns_col = ra,
    weights = wts,
    col_rename = "ra"
  ))

# 方法2: 使用资产 + 权重 数据框
(wts_map <- tibble(
  symbols = c('000001.SZ', '000004.SZ'),
  weights = c(0.5, 0.5)
))

stock_returns_monthly %>% 
  tq_portfolio(
    assets_col = symbol,
    returns_col = ra,
    weights = wts_map,
    col_rename = "ra_using_wts_map"
  )

# 合并ra和rb
(rarb_single_portfolio <- left_join(portfolio_returns_monthly,
                                   rb,
                                   by = "date"))
# 计算 CAPM 模型
rarb_single_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.CAPM
  )

# 多个资产组合
(stock_returns_monthly_multi <- stock_returns_monthly %>% 
  tq_repeat_df(n = 3))
weights <- c(
  0.5, 0.25, 0.25,
  0.25, 0.5, 0.25,
  0.25, 0.25, 0.5
)
stocks = c('000001.SZ', '000002.SZ', '000004.SZ')
(weights_table <- tibble(stocks) %>% 
  tq_repeat_df(3) %>% 
  bind_cols(tibble(weights)) %>% 
  group_by(portfolio))
(portfolio_returns_monthly_multi <- stock_returns_monthly_multi %>% 
  tq_portfolio(assets_col = symbol,
               returns_col = ra,
               weights = weights_table,
               col_rename = 'ra'))
(rarb_multi_portfolio <- left_join(
  portfolio_returns_monthly_multi, rb,
  by = "date"
))

rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.CAPM
  )

rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = SharpeRatio
  )

# 查看所有可以使用的函数
tq_performance_fun_options()

# table.Stats
# 返回基本统计量
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = table.Stats
  )

# table.CAPM
# 返回与CAPM相关的度量
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.CAPM
  )

# table.AnnualizedReturns
# 返回年度回报、标准差和夏普比率
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = table.AnnualizedReturns
  )

# table.Correlation
# 返回与基准收益的回报率
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.Correlation
  )

# table.DownsideRisk
# 返回下行风险表，以便在多个工具或基金之间进行比较
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.DownsideRisk
  )

# table.DownsideRiskRatio
# 返回每月下行风险、年度下行风险等指标
rarb_multi_portfolio %>% 
  tq_performance(Ra = ra,
                 Rb = NULL,
                 performance_fun = table.DownsideRiskRatio)

# table.HigherMoments
# 返回分布的高阶矩和协矩
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.HigherMoments
  )

# table.InformationRatio
# 返回追踪误差表、年度追踪误差和信息比率
rarb_multi_portfolio %>% 
  tq_performance(Ra = ra,
                 Rb = rb,
                 performance_fun = table.InformationRatio)
# table.Variability
# 返回平均绝对差异、月标准差和年标准差
rarb_multi_portfolio %>% 
  tq_performance(Ra = ra,
                 Rb = NULL,
                 performance_fun = table.Variability)

# 在险值
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = VaR
  )

# 夏普比率
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = SharpeRatio
  )

# 自定义tq_portfolio
portfolio_returns_monthly %>% 
  ggplot(aes(x = date, y = ra)) + 
  geom_bar(stat = "identity", fill = palette_light()[[3]],
           width = 20) + 
  labs(title = "Portfolio Returns",
       caption = "Shows an above-zero trend meaning positive returns",
       x = "", y = "Monthly Returns") + 
  geom_smooth(method = "lm") + 
  theme_tq(base_family = enfont) + 
  scale_color_tq() + 
  scale_y_continuous(labels = scales::percent)

ggsave("tidyquantperformance1.svg")

# 使用 wealth.index = TRUE 参数，我们可以观察10000元是如何增长的：
wts <- c(0.5, 0, 0.5)
portfolio_growth_monthly <- stock_returns_monthly %>% 
  tq_portfolio(
    assets_col = symbol,
    returns_col = ra,
    weights = wts,
    col_rename = "investment.growth",
    wealth.index = TRUE
  ) %>% 
  mutate(investment.growth = investment.growth * 10000)
portfolio_growth_monthly %>% 
  ggplot(aes(x = date, y = investment.growth)) + 
  geom_line(size = 1, color = palette_light()[3]) +
  labs(x = "", y = "Portfolio Value",
       title = "Portfolio Growth",
       caption = "Now we can really visualize performance!") + 
  geom_smooth(method = "loess") + 
  theme_tq(base_family = enfont) + 
  scale_color_tq() + 
  scale_y_continuous(labels = scales::dollar)

ggsave("tidyquantperformance2.svg")

# 下面再比较三个投资组合的增长情况
portfolio_growth_monthly_multi <- stock_returns_monthly_multi %>% 
  tq_portfolio(
    assets_col = symbol,
    returns_col = ra,
    weights = weights_table,
    col_rename = "investment.growth",
    wealth.index = TRUE
  ) %>% 
  mutate(investment.growth = investment.growth * 10000)
portfolio_growth_monthly_multi %>% 
  ggplot(aes(x = date, y = investment.growth, color = factor(portfolio))) + 
  geom_line(size = 2) + 
  labs(title = "Portfolio Growth",
       subtitle = "Comparing Multiple Portfolios",
       caption = "Portfolio 3 is a Standout!",
       x = "", 
       y = "Portfolio Value",
       color = "Portfolio") + 
  geom_smooth(method = "loess") + 
  theme_tq(base_family = enfont) + 
  scale_color_tq() +
  scale_y_continuous(labels = scales::dollar)

ggsave("tidyquantperformance3.svg")
# Performance Analysis with tidyquant
library(tidyquant)
(ra <- c("000001.SZ", "000002.SZ", "000004.SZ") %>% 
  tq_get(get = "stock.prices",
         from = "2010-01-01",
         to = "2015-12-31") %>% 
  group_by(symbol) %>% 
  tq_transmute(
    select = adjusted,
    mutate_fun = periodReturn,
    period = "monthly",
    col_rename = "ra"
  ))

# 下面我们使用深证100R指数得到基准价格：
library(Tushare)
api = pro_api('fd65bf1ee56215d0ce1a95d1fa3b453d97a1779438aca26c4b7937d1')
(rb <- api(api_name = "index_daily",
    ts_code = "399004.SZ",
    start_date = "20100101",
    end_date = "20151231") %>% 
  as_tibble() %>% 
  transmute(
    date = ymd(trade_date),
    symbol = as.character(ts_code),
    close = as.numeric(close)
  ) %>% 
  tq_transmute(
    select = close,
    mutate_fun = periodReturn,
    period = "monthly",
    col_rename = "rb"
  ))

# 合并两个数据集：
(rarb <- left_join(ra, rb, by = "date"))

# 使用 tq_performance 运行 CAPM 模型
(rarb_capm <- rarb %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.CAPM
  ))

# 我们比较关心增长测度和风险测度
rarb_capm %>% 
  dplyr::select(Alpha, Beta)

# tidyquant的工作流程
# 单独资产分析
# 查看 SharpeRatio 函数的参数：
args(SharpeRatio)

# Step 1A: 获取股票数据
(stock_prices <- c("000001.SZ", "000002.SZ", "000004.SZ") %>% 
  tq_get(
    get = "stock.prices",
    from = "2010-01-01",
    to = "2015-12-31"
  ))


(stock_returns_monthly <- stock_prices %>% 
  group_by(symbol) %>% 
  tq_transmute(
    select = adjusted,
    mutate_fun = periodReturn,
    period = "monthly",
    col_rename = "ra"
  ))

# Step4: 绩效分析
stock_returns_monthly %>% 
  tq_performance(Ra = ra,
                 Rb = NULL,
                 performance_fun = SharpeRatio)

# 修改无风险利率和置信水平
stock_returns_monthly %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = SharpeRatio,
    Rf = 0.03 / 12,
    p = 0.99
  )

# 资产组合的分析
# 组合资产
# 方法1: 使用权重向量
wts <- c(0.5, 0, 0.5)
(portfolio_returns_monthly <- stock_returns_monthly %>% 
  tq_portfolio(
    assets_col = symbol,
    returns_col = ra,
    weights = wts,
    col_rename = "ra"
  ))

# 方法2: 使用资产 + 权重 数据框
(wts_map <- tibble(
  symbols = c('000001.SZ', '000004.SZ'),
  weights = c(0.5, 0.5)
))

stock_returns_monthly %>% 
  tq_portfolio(
    assets_col = symbol,
    returns_col = ra,
    weights = wts_map,
    col_rename = "ra_using_wts_map"
  )

# 合并ra和rb
(rarb_single_portfolio <- left_join(portfolio_returns_monthly,
                                   rb,
                                   by = "date"))
# 计算 CAPM 模型
rarb_single_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.CAPM
  )

# 多个资产组合
(stock_returns_monthly_multi <- stock_returns_monthly %>% 
  tq_repeat_df(n = 3))
weights <- c(
  0.5, 0.25, 0.25,
  0.25, 0.5, 0.25,
  0.25, 0.25, 0.5
)
stocks = c('000001.SZ', '000002.SZ', '000004.SZ')
(weights_table <- tibble(stocks) %>% 
  tq_repeat_df(3) %>% 
  bind_cols(tibble(weights)) %>% 
  group_by(portfolio))
(portfolio_returns_monthly_multi <- stock_returns_monthly_multi %>% 
  tq_portfolio(assets_col = symbol,
               returns_col = ra,
               weights = weights_table,
               col_rename = 'ra'))
(rarb_multi_portfolio <- left_join(
  portfolio_returns_monthly_multi, rb,
  by = "date"
))

rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.CAPM
  )

rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = SharpeRatio
  )

# 查看所有可以使用的函数
tq_performance_fun_options()

# table.Stats
# 返回基本统计量
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = table.Stats
  )

# table.CAPM
# 返回与CAPM相关的度量
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.CAPM
  )

# table.AnnualizedReturns
# 返回年度回报、标准差和夏普比率
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = table.AnnualizedReturns
  )

# table.Correlation
# 返回与基准收益的回报率
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.Correlation
  )

# table.DownsideRisk
# 返回下行风险表，以便在多个工具或基金之间进行比较
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.DownsideRisk
  )

# table.DownsideRiskRatio
# 返回每月下行风险、年度下行风险等指标
rarb_multi_portfolio %>% 
  tq_performance(Ra = ra,
                 Rb = NULL,
                 performance_fun = table.DownsideRiskRatio)

# table.HigherMoments
# 返回分布的高阶矩和协矩
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = rb,
    performance_fun = table.HigherMoments
  )

# table.InformationRatio
# 返回追踪误差表、年度追踪误差和信息比率
rarb_multi_portfolio %>% 
  tq_performance(Ra = ra,
                 Rb = rb,
                 performance_fun = table.InformationRatio)
# table.Variability
# 返回平均绝对差异、月标准差和年标准差
rarb_multi_portfolio %>% 
  tq_performance(Ra = ra,
                 Rb = NULL,
                 performance_fun = table.Variability)

# 在险值
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = VaR
  )

# 夏普比率
rarb_multi_portfolio %>% 
  tq_performance(
    Ra = ra,
    Rb = NULL,
    performance_fun = SharpeRatio
  )

# 自定义tq_portfolio
portfolio_returns_monthly %>% 
  ggplot(aes(x = date, y = ra)) + 
  geom_bar(stat = "identity", fill = palette_light()[[3]],
           width = 20) + 
  labs(title = "Portfolio Returns",
       caption = "Shows an above-zero trend meaning positive returns",
       x = "", y = "Monthly Returns") + 
  geom_smooth(method = "lm") + 
  theme_tq(base_family = enfont) + 
  scale_color_tq() + 
  scale_y_continuous(labels = scales::percent)

ggsave("tidyquantperformance1.svg")

# 使用 wealth.index = TRUE 参数，我们可以观察10000元是如何增长的：
wts <- c(0.5, 0, 0.5)
portfolio_growth_monthly <- stock_returns_monthly %>% 
  tq_portfolio(
    assets_col = symbol,
    returns_col = ra,
    weights = wts,
    col_rename = "investment.growth",
    wealth.index = TRUE
  ) %>% 
  mutate(investment.growth = investment.growth * 10000)
portfolio_growth_monthly %>% 
  ggplot(aes(x = date, y = investment.growth)) + 
  geom_line(size = 1, color = palette_light()[3]) +
  labs(x = "", y = "Portfolio Value",
       title = "Portfolio Growth",
       caption = "Now we can really visualize performance!") + 
  geom_smooth(method = "loess") + 
  theme_tq(base_family = enfont) + 
  scale_color_tq() + 
  scale_y_continuous(labels = scales::dollar)

ggsave("tidyquantperformance2.svg")

# 下面再比较三个投资组合的增长情况
portfolio_growth_monthly_multi <- stock_returns_monthly_multi %>% 
  tq_portfolio(
    assets_col = symbol,
    returns_col = ra,
    weights = weights_table,
    col_rename = "investment.growth",
    wealth.index = TRUE
  ) %>% 
  mutate(investment.growth = investment.growth * 10000)
portfolio_growth_monthly_multi %>% 
  ggplot(aes(x = date, y = investment.growth, color = factor(portfolio))) + 
  geom_line(size = 2) + 
  labs(title = "Portfolio Growth",
       subtitle = "Comparing Multiple Portfolios",
       caption = "Portfolio 3 is a Standout!",
       x = "", 
       y = "Portfolio Value",
       color = "Portfolio") + 
  geom_smooth(method = "loess") + 
  theme_tq(base_family = enfont) + 
  scale_color_tq() +
  scale_y_continuous(labels = scales::dollar)

ggsave("tidyquantperformance3.svg")
