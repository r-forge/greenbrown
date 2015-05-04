
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

<p>The R package greenbrown is a collection of functions to analyse trends, trend changes and phenology events in gridded time series like from satellite observations or climate model simulations.</p>

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


<h4>Introduction</h4>

<p>Satellite observations are used to monitor temporal changes of the terrestrial vegetation. Therefore time series of vegetation indices such as Normalized Difference Vegetation Index (NDVI) are derived from satellite measurements. Vegetation index time series are indicative of the coverage of green vegetation, photosynthetic activity and green biomass. However, the analysis of vegetation index time series is often dependent on the used analysis methods. The package provides access to different methods for 1) trend and breakpoint analysis, 2) time series smoothing and interpolation, and 3) analysis of land surface phenology. </p>

<p>The package has been developed at the Max Planck Institute for Biogeochemistry, Jena, Germany in order to distribute the code of the following publications: </p>

<ul>
<li>Forkel M, Carvalhais N, Verbesselt J, Mahecha M, Neigh C, Reichstein M (2013) <a href="http://www.mdpi.com/2072-4292/5/5/2113">Trend Change Detection in NDVI Time Series: Effects of Inter-Annual Variability and Methodology.</a> Remote Sensing, 5, 2113-2144.</li> 
<li>Forkel M, Migliavacca M, Thonicke K, Reichstein M, Schaphoff S, Weber U, Carvalhais N (2015) <a href="http://onlinelibrary.wiley.com/doi/10.1111/gcb.12950/abstract">Co-dominant water control on global inter-annual variability and trends in land surface phenology and greenness.</a> Global Change Biology, accepted.</li>
</ul>


<h4>Usage and citation</h4>

<p>The greenbrown package is published under the GPL-2 license (GNU Public License) which guarantees users the freedoms to use, study, share (copy), and modify the software. In case you are using the software or parts of it within scientific publications, we recommend to cite Forkel et al. (2013) in the context of trend analysis and/or Forkel et al. (2015) in the context of phenology analyses. To cite the R package or this website please use:</p>

<ul>
<li>Forkel M, Wutzler T (2015) greenbrown - land surface phenology and trend analysis. A package for the R software. Version 2.2, 2015-04-15, <a href="http://greenbrown.r-forge.r-project.org/">http://greenbrown.r-forge.r-project.org/</a>.</li>
</ul>


<h4>Developers and contact</h4>

<p>The package has been developed by Matthias Forkel with technical support by Thomas Wutzler. Nuno Carvalhais, Miguel Mahecha, Mirco Migliavacca and Jan Verbesselt gave advice or provided code snippets to some of the functions. </p>

<p>Matthias Forkel, mforkel [at] bgc-jena.mpg.de <br>
Thomas Wutzler, twutz [at] bgc-jena.mpg.de<br>
Max Planck Institute for Biogeochemistry Jena, Germany<br>
Hans-Knoell-Str. 10<br>
07745 Jena, Germany</p>

<br>
<hr>
<p><a href="index.php">greenbrown</a>, Matthias Forkel, mforkel [at] bgc-jena.mpg.de, 2015-04-15</p>

</body>
</html>
