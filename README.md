# hchinamap：使用R + Highmaps绘制中国以及各个省市自治区地图

> 由于含有不可替代的中文，这个包是肯定不能提交给 CRAN 了。

## 引言

很久之前，我就发现[简数科技](https://www.highcharts.com.cn/mapdata)提供的中国地图示例非常不错，一直有想把它封装成R函数或者Stata命令的想法，今天这个想法终于在R上实现了，使用该包可以非常方便的创建中国地图以及各个省市自治区的地图。由于该包是基于htmlwidgets开发，所以你也可以在R Markdown和shiny中使用。

## 安装

你可以从 `github` 上安装这个包:

```r
devtools::install_github('hchinamap')
# 或者使用git
devtools::install_git("https://github.com/czxa/hchinamap.git")
```

## 概览

中国地图 | 省份地图
:-: | :-:
![](https://czxb.github.io/br/chinamap.png) | ![](https://czxb.github.io/br/anhuimao.png)

## 许可证

motherland包依赖于[highcharts](https://www.highcharts.com.cn/)，`highcharts`是一个商业的JS图表库，它同时提供了商业许可和非商业许可。这意味着如果你是非商业使用，你可以免费自由的使用，但是如果你是用于商业和政府，你需要联系[highcharts](https://www.highcharts.com.cn/)以获得许可。motherland包没有从[highcharts](https://www.highcharts.com.cn/)获取任何许可，因此在你使用该包以及[highcharts](https://www.highcharts.com.cn/)前，请谨慎阅读相关许可。

## 获取vignettes

```r
vignette("hchinamap")
```
