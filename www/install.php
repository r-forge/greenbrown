
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

<h2>Installation and loading</h2>

<p>The package greenbrown was developed since the R version 2.15.3. To install the package directly from Rforge you need the most recent R version.</p>

<p>To install the most recent version of greenbrown type directly within R: <br><b>install.packages("greenbrown", repos="http://R-Forge.R-project.org")</b></p>

<p>If you want to use greenbrown, you have to load the package at the beginning of each R session with: <br><b>library(greenbrown)</b></p> 

<p>Alternatively, you can receive all greenbrown source code files by using an anonymous SVN checout: <br><b>
svn checkout svn://r-forge.r-project.org/svnroot/greenbrown/</b></p>

<br>
<hr>
<p><a href="index.php">greenbrown</a>, Matthias Forkel, mforkel [at] bgc-jena.mpg.de, 2015-04-15</p>

</body>
</html>
