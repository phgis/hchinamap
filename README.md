# hchinamap：使用R + Highmaps绘制中国以及各个省市自治区地图

> 爲了把這個 R 提交給CRAN，我把region參數修改爲英語，因此，在CRAN上的那個版本中 region 參數爲英文，具體可以參考我提供的 vignettes。
<!-- badges: start -->
 [![Rdoc](http://www.rdocumentation.org/badges/version/hchinamap)](http://www.rdocumentation.org/packages/hchinamap) ![](http://cranlogs.r-pkg.org/badges/grand-total/hchinamap?color=green) ![](http://cranlogs.r-pkg.org/badges/hchinamap?color=green) ![](http://cranlogs.r-pkg.org/badges/last-week/hchinamap?color=green) [![Launch binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/czxa/hchinamap/master) [![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental) ![](https://visitor-count-badge.herokuapp.com/total.svg?repo_id=czxa.hchinamap) [![HitCount](http://hits.dwyl.com/czxa/hchinamap.svg)](http://hits.dwyl.com/czxa/hchinamap)
<!-- badges: end -->


## 引言

很久之前，我就发现[简数科技](https://www.highcharts.com.cn/mapdata)提供的中国地图示例非常不错，一直有想把它封装成R函数或者Stata命令的想法，今天这个想法终于在R上实现了，使用该包可以非常方便的创建中国地图以及各个省市自治区的地图。由于该包是基于htmlwidgets开发，所以你也可以在R Markdown和shiny中使用。

## 安装

你可以從 `CRAN` 上安装这个这个包：

```r
install.packages("hchinamap", build_vignettes = TRUE)
```

你也可以从 `github` 上安装这个包:

```r
devtools::install_github('hchinamap', build_vignettes = TRUE)
# 或者使用git
devtools::install_git("https://github.com/czxa/hchinamap.git", build_vignettes = TRUE)
```

## 概览

中国地图 | 省份地图
:-: | :-:
![](https://czxb.github.io/br/chinamap.png) | ![](https://czxb.github.io/br/anhuimao.png)

## 许可证

hchinamap包依赖于[highcharts](https://www.highcharts.com.cn/)，`highcharts`是一个商业的JS图表库，它同时提供了商业许可和非商业许可。这意味着如果你是非商业使用，你可以免费自由的使用，但是如果你是用于商业和政府，你需要联系[highcharts](https://www.highcharts.com.cn/)以获得许可。motherland包没有从[highcharts](https://www.highcharts.com.cn/)获取任何许可，因此在你使用该包以及[highcharts](https://www.highcharts.com.cn/)前，请谨慎阅读相关许可。

## 获取vignettes

```r
vignette("hchinamap")
```
