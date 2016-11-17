
<!-- This is the project specific website template -->
<!-- It can be changed as liked or replaced by other content -->

<?php

$domain=ereg_replace('[^\.]*\.(.*)$','\1',$_SERVER['HTTP_HOST']);
$group_name=ereg_replace('([^\.]*)\..*$','\1',$_SERVER['HTTP_HOST']);
$themeroot='r-forge.r-project.org/themes/rforge/';

echo '<?xml version="1.0" encoding="UTF-8"?>';
?>
<!DOCTYPE html
	PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"
	"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en   ">

  <head>
	<meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
	<title><?php echo $group_name; ?></title>
	<link href="http://<?php echo $themeroot; ?>styles/estilo1.css" rel="stylesheet" type="text/css" />
	<link rel="stylesheet" href="styles.css" type="text/css">
  </head>

<body>

<!-- R-Forge Logo -->
<table border="0" width="100%" cellspacing="0" cellpadding="0">
<tr><td>
<a href="http://r-forge.r-project.org/"><img src="http://<?php echo $themeroot; ?>/imagesrf/logo.png" border="0" alt="R-Forge Logo" /> </a> </td> </tr>
</table>

<h1>greenbrown - land surface phenology and trend analysis</h1>

<!-- menu -->
<hr>
<h2>Contents</h2>
<ul>
	<li><a href="index.php">greenbrown: Introduction</a></li> 
	<li><a href="install.php">Installation and loading</a></li> 
	<li>Using greenbrown - applications and examples:</li>
	<ul>
		<li><a href="trends.php">Trends and breakpoints</a></li>
		<li><a href="phenology.php">Phenology analysis</a></li>
	</ul>
	<li><a href="http://<?php echo $domain; ?>/projects/<?php echo $group_name; ?>/">Developer summary page</a> </li>
</ul>
<hr>
<!-- end of menu -->

<h2>Using greenbrown: Trends and breakpoints</h2>

<h3>Trend and breakpoint estimation on time series: Trend</h3>

<p>The main function to calculate trends and breakpoints on single time series is <b><a href="man/Trend.html">Trend</a></b>. This function offers a common access to different methods for trend analysis as assessed in Forkel et al. (2013):</p>

<p><ul>Forkel, M., Carvalhais, N., Verbesselt, J., Mahecha, M., Neigh, C., Reichstein, M., 2013.  <a href="http://www.mdpi.com/2072-4292/5/5/2113">Trend Change Detection in NDVI Time Series: Effects of Inter-Annual Variability and Methodology.</a> Remote Sensing 5, 2113–2144. doi:10.3390/rs5052113</ul></p>

<p>The following methods are tested and compared in this paper based on NDVI time series:</p>

<ul>
<li><b><a href="man/TrendAAT.html">TrendAAT</a></b> computes trends on annual aggregated time series (method AAT in Forkel et al. 2013).</li> 
<li><b><a href="man/TrendSTM.html">TrendSTM</a></b> computes trends based on a season-trend model (similar to the time series analysis approach used in <a href="http://cran.r-project.org/web/packages/bfast/index.html" target="_blank">bfast</a>) (method STM in Forkel et al. 2013).</li>
<li><b><a href="man/TrendSeasonalAdjusted.html">TrendSeasonalAdjusted</a></b> removes first the seasonal cycle from a time series and computes then the trend on the seasonal-adjusted time series. To remove the seasonal cycle two functions are currently implemented to estimate the seasonal component: <b><a href="man/MeanSeasonalCycle.html">MeanSeasonalCycle</a></b> (method MAC in Forkel et al. 2013) and <b><a href="man/SSASeasonalCycle.html">SSASeasonalCycle</a></b> (method SSA in Forkel et al. 2013).</li>
</ul>


<blockquote>
> # load the package and example data<br>
> library(greenbrown)<br>
> data(ndvi) # load the time series<br>
> plot(ndvi) # plot the time series<br>
<br>
> # calculate trend (default method: TrendAAT)<br>
> trd <- Trend(ndvi)<br>
> trd<br>
--- Trend --------------------------------------- <br>
Calculate trends and trend changes on time series <br>
------------------------------------------------- <br>
<br>
Time series start   :  1982 1 <br>
Time series end     :  2008 1 <br>
Time series length  :  27 <br>
<br>
Test for structural change <br>
  OLS-based MOSUM test for structural change <br>
    statistic       : 0.8876267 <br>
    p-value         : 0.2874284 <br>
  Breakpoints       : Breakpoints were not detected. <br>
<br>
Trend method        : AAT <br>
<br>
Trends in segments of the time series <br>
                       slope        %  p-value      tau <br>
     segment   1 : -0.000988   -0.343    0.182   -0.185 <br>

<br>
> # calculate trend but consider breakpoints<br>
> trd <- Trend(ndvi, mosum.pval=1)<br>
> plot(trd) <br>
</blockquote>

<p><img src="figure_Trend.png"><br>
Fig. 1: Trend in mean annual NDVI from an example grid cell in central Alaska.</p>

<p>Other functions exist to calculate non-linear trends on time series based on different methods such as <b><a href="man/TrendPoly.html">TrendPoly</a></b> (4th-order polynomial), <b><a href="man/TrendRunmed.html">TrendRunmed</a></b> (running median), <b><a href="man/TrendSSA.html">TrendSSA</a></b> (low frequency component from singular spectrum analysis), <b><a href="man/TrendSTL.html">TrendSTL</a></b> (trend component from STL). </p> 

<p>The package implements further methods that have been used in Forkel et al. (2013) to estimate properties of NDVI time series and to simulate artificial time series. Statistical properties of time series can be estimated based on the time series decomposition approach as described in Forkel et al. (2013) and implemented in <b><a href="man/Decompose.html">Decompose</a></b> which is then applied to raster datasets with <b><a href="man/GetTsStatisticsRaster.html">GetTsStatisticsRaster</a></b>. <b><a href="man/SimTs.html">SimTs</a></b> can be used to simulate artificial time series.</p>


<h3>Trend and breakpoint estimation on raster data sets: TrendRaster and TrendGradient</h3>

<p>The function <b><a href="man/TrendRaster.html">TrendRaster</a></b> is used to apply trend and breakpoint estimation methods as implemented in <b><a href="man/Trend.html">Trend</a></b> to gridded raster datasets. </p>

<blockquote>
> data(ndvimap) # load the example raster data<br>
> ?ndvimap # information about the data<br>
> plot(ndvimap, 8, col=brgr.colors(50))<br>
<br>
> # calculate trend on the raster dataset using annual maximum NDVI<br>
> trendmap <- TrendRaster(ndvimap, start=c(1982, 1), freq=12, method="AAT", breaks=1, funAnnual=max)<br>
> plot(trendmap, col=brgr.colors(20), legend.width=2) # this line will produce figure 2:<br>
</blockquote>

<p><img src="figure2.png"><br>
Fig. 2: Trends on annual maximum NDVI with detected breakpoints (BP, year of breakpoint), length of the time series segments (LengthSEG, in years), slope of the trend in each segment (SlopeSEG) and p-value of the trend in each segment (Pval).</p>

<p>Results of this trend and breakpoint analysis can be classified into maps of significant positive or negative trends with <b><a href="man/TrendClassification.html">TrendClassification</a></b>.

<blockquote>
> trendclassmap <- TrendClassification(trendmap, min.length=8, max.pval=0.05)<br>
> plot(trendclassmap, col=brgr.colors(3), legend.width=2) <br>
</blockquote>

<p><img src="figure3.png"><br>
Fig. 3: Significant greening and browning trends on annual maximum NDVI in each time series segment.</p>


<p>The function <a href="man/TrendGradient.html"><b>TrendGradient</b></a> computes trends along a spatial gradient (e.g. along latitudes). This function extracts along a spatial gradient (e.g. along latitude) time series from a raster brick and computes for each position a temporal trend.</p>

<blockquote>
> data(ndvimap)<br>
> # compute a latitudinal gradient of trends (by default the method 'AAT' is used):<br>
> gradient <- TrendGradient(ndvimap, start=c(1982, 1), freq=12)<br>
> plot(gradient)<br> 
</blockquote>

<p><img src="figure_TrendGradient.png"><br>
Fig. 4: Latitudinal gradient of NDVI trends. The bold line with significance flags is the estimated trend slope. The gray-colored area and the thin line are the 95% confidence interval and the median, respectively of the estimated trend depending on a sampling of the time series length.</p>

 
<h3>Comparison of classified maps: CompareClassification and AccuracyAssessment</h3>

<p>greenbrown offers also some function to compare classifications and maps with classified values. The function <b><a href="man/AccuracyAssessment.html">AccuracyAssessment</a></b> computes an accuracy assessment (user accuracy, producer accuracy, total accuracy) from two classifications. <b><a href="man/CompareClassification.html">CompareClassification</a></b> compares two classified maps. These functions can be for example used to compare two maps of classified trend (greening/browning/no trend) that were for example calculated with two different methods or from two NDVI data sets. </p>

<blockquote>
> data(ndvimap)<br>
<br>
> # calculate trends with two different methods<br>
> AATmap <- TrendRaster(ndvimap, start=c(1982, 1), freq=12, method="AAT", breaks=0)<br>
> STMmap <- TrendRaster(ndvimap, start=c(1982, 1), freq=12, method="STM", breaks=0)<br>
<br>
> # classify the trend estimates into significant positive, negative and no trend<br>
> AATmap.cl <- TrendClassification(AATmap)<br>
> STMmap.cl <- TrendClassification(STMmap)<br>
<br>
> # compare the classified maps<br>
> compare <- CompareClassification(AATmap.cl, STMmap.cl, names=list('AAT'=c("Br", "No", "Gr"), 'STM'=c("Br", "No", "Gr")))<br>
> compare 
<br>
> # plot the comparison<br>
> plot(compare) 
</blockquote>

<p><img src="fig_CompareClassification.png"><br>
Fig. 5: Comparsion of greening and browning trends on annual mean NDVI from two different methods. Gr/Gr indicates pixels where both maps show greening trends. No cases exist where the two methods have opposite trends (Gr/Br or Br/Gr).</p>

<br>
<hr>
<p><a href="index.php">greenbrown</a>, Matthias Forkel, mforkel [at] bgc-jena.mpg.de, 2015-04-15</p>

</body>
</html>
