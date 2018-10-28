#!/usr/bin/perl

# new picture of the day
# script that selects a random picture from a
#  photo collection (directories containing images and associated .xml files)

use XML::XPath;
use XML::XPath::XMLParser;
use File::Find;

$gVersion="2018-10-28-0";
$gDebug=0;
$gPhotoCollectionPath = $ENV{'HOME'} . "/var/alternatives/colectie-foto";
$gPodPath = $ENV{'HOME'} . "/var/run/pod/";
$gFlagsDir = $ENV{'HOME'} . "/var/settings/colectie-foto-flags";
$gImageFnameRegexp="\.(jpe?g|tiff?|png)\$";

sub debugMsg {
	$gDebug && print ":debug: $_[0]\n";
}


sub convertImage
{
	my $skipResize=1;
	$photoFile=$_[0];
	$destinationFile=$_[1];
	$maxHeight=780; # :fixme: - cmd line param.
	$maxWidht=950; # :fixme: - cmd line param.

	my $currentHeight=qx/identify -ping -format "%h" "$photoFile"/;
	die "identify error" if($?);
	$gDebug && print "  :debug: Height: $currentHeight \n";
	if($currentHeight<$maxHeight) {
		$gDebug && print "  :debug: Skipping resize \n";
		$resizePercent1=100;
	}
	else {
		$resizePercent1=$maxHeight*100/$currentHeight;
		$gDebug && print "  :debug: Percent 1: $resizePercent1 \n";
		$skipResize=0;
	}

	my $currentWidth=qx/identify -ping -format "%w" "$photoFile"/;
	die "identify error" if($?);
	$gDebug && print "  :debug: Width: $currentWidth \n";
	if($currentWidth<$maxWidht) {
		$gDebug && print "  :debug: Skipping resize \n";
		$resizePercent2=100;
	}
	else {
		$resizePercent2=$maxWidht*100/$currentWidth;
		$gDebug && print "  :debug: Percent 2: $resizePercent2 \n";
		$skipResize=0;
	}

	if($resizePercent1<$resizePercent2) {
		$resizePercent=$resizePercent1;
	}
	else {
		$resizePercent=$resizePercent2;
	}
	if(!$skipResize) {
		qx/convert -resize $resizePercent% "$photoFile" "$destinationFile"/;
	}
	else {
		qx/convert "$photoFile" "$destinationFile"/;
	}
}


# @return 0 on error
# $_[5]=1 if nsfw
sub getXmlData
{
	$gDebug && print "  :debug: trying to load xml file $_[0] \n";
	# If the file does not exist, the program dies.
	# Not a good idea, XML::XPath should be replaced with something better.
	my $xp = XML::XPath->new(filename => "$_[0]");
    my $nodeset;

    $nodeset = $xp->find('/photo/filename'); # find all filenames
	$_[1] = $nodeset->string_value(); # ret first filename

    $nodeset = $xp->find('/photo/author'); # find all authors
	$_[2] = $nodeset->string_value(); # :fixme: - multiple authors?

    $nodeset = $xp->find('/photo/source');
	$_[3] = $nodeset->string_value(); # :fixme: - multiple sources?

    $nodeset = $xp->find('/photo/title');
	$_[4] = $nodeset->string_value();

	$nodeset = $xp->find('/photo/tag');
	my $nodesetText=$nodeset->to_literal();
	if ($nodesetText =~ /safety-moderate/) {
		$gDebug && print "  :debug: xml contains 'safety-moderate' tag \n";
		$_[5]=1;
	}
	else {
		$_[5]=0;
	}
	return 1;
}


sub printHtml
{
	$gDebug && print "  :debug: writing to html file $_[0] \n";
	open(HTMLFILE, ">$_[0]");
	binmode HTMLFILE, ":utf8";
	print HTMLFILE '<html>
<head>
<meta http-equiv="Content-Type" content="text/html; charset=UTF-8">
<title>Photo</title>
<style type="text/css">
		 html { background-color: black; color: white; }
		 img { border: 1px solid white; margin-top: 2px; }

div.details {
  background-color: transparent;
  color: #ccc;
  /*font-weight: bold;*/
  /*width: 32%;*/
  width: 75%;
  margin: 0.5em;
  border: 1px solid #ccc;
  /* border-style: solid none; */
  border-style: none;
  /*
  position: fixed;
  top: auto;
  right: 5%;
  bottom: 10%;
  */
  font-size: small;
  font-family: "Consolas",sans;
  left: auto;
}

span.author { font-weight: bold; }

</style>
</head>
<body>
<div align="center" width="100%">';

	print HTMLFILE "<img src=\"$_[1]\">\n";
	print HTMLFILE "<div class=\"details\">\n";
	print HTMLFILE "$_[4]";
    print HTMLFILE " ; <span class=\"author\">$_[2]</span>" if ($_[2]);
	print HTMLFILE "<br>\n";
	#print HTMLFILE "Sursa: $_[3]<br>\n";
	print HTMLFILE '
</div>
</div>
</body>
</html>';
	close(HTMLFILE);
}


sub displayUsage {
	print "photoCollection-pod.pl ver. $gVersion\n";
	print "usage: photoCollection-pod.pl ";
	print "[ -c <collectionPath> ] [ -d <destination> ] [ -v ]\n";
	print "  -v = verbose on\n";
}


sub replaceExtension {
	$_[0] =~ /(.*)\..*/;
	return "$1.$_[1]";
}


sub isImageFile {
	#return ($_[0] =~ /$gImageFnameRegexp/i); # :debug:
	return (-f "$_[0]" && $_[0] =~ m/$gImageFnameRegexp/i);
}


@gImgFileList=();

sub processFile {
	#debugMsg("Analyzing file $File::Find::name"); # :debug:
	if (isImageFile($File::Find::name)) {
		my $flagFile;
		my $probability=2;

		debugMsg("Analyzing image file $File::Find::name");

		# check x2 flag
		$flagFile=$gFlagsDir."/".replaceExtension($_, "flag_x2");
		if (-e $flagFile) {
			$probability=4;
		}

		# check x-2 flag
		$flagFile=$gFlagsDir."/".replaceExtension($_, "flag_x-2");
		if (-e $flagFile) {
			$probability=1;
		}

		for ($i=0; $i<$probability; $i++) {
			push(@gImgFileList, $File::Find::name);
		}
	}
}


# $_[0] - collection path
# $_[1] - flags dir
# returns the image filename (e.g. photo.jpg)
# to obtain the xml filename, use replaceExtension()
sub selectAFile {
	find({ wanted => \&processFile, follow => 1 } , ( $_[0] ) );
	debugMsg("No. of files in list: ".($#gImgFileList+1));

	# :debug:
	#for ($i=0; $i<=$#gImgFileList; $i++) {
	#	debugMsg($gImgFileList[$i]);
	#}

	if ($#gImgFileList > 0) {
		my $selectedDirNum=int(rand($#gImgFileList+1));
		debugMsg("Selected image no. $selectedDirNum from list; filename=$gImgFileList[$selectedDirNum]");
		return $gImgFileList[$selectedDirNum];
	}
	else {
		return 0;
	}
}

# main

# test area
#$gDebug=1;
#debugMsg(replaceExtension("bibi.jpg", "xml"));
#exit(0);

while(scalar @ARGV > 0) {
	if($ARGV[0] eq "-h") {
		displayUsage;
		exit 1;
	}
	elsif($ARGV[0] eq "-c") {
		if($ARGV[1]) {
			$gPhotoCollectionPath=$ARGV[1];
			if (! -d "$gPhotoCollectionPath") {
				print STDERR "Error: $gPhotoCollectionPath is not a directory.\n";
				exit 2;
			}
		}
		else {
			print STDERR "Error: Invalid path (-c param).\n";
			exit 1;
		}
		shift(@ARGV);
		shift(@ARGV);
	}
	elsif($ARGV[0] eq "-d") {
		if($ARGV[1]) {
			$gPodPath=$ARGV[1];
			if (! -d "$gPodPath") {
				print STDERR "Error: $gPodPath is not a directory.\n";
				exit 2;
			}
		}
		else {
			print STDERR "Error: Invalid path (-d param).\n";
			exit 1;
		}
		shift(@ARGV);
		shift(@ARGV);
	}
	elsif($ARGV[0] eq "-v") {
		$gDebug=1;
		shift(@ARGV);
	}
	else {
		print STDERR "Invalid parameter: $ARGV[0] \n";
		exit 1;
	}
}

$gDebug && print("  :debug: photo collection path: $gPhotoCollectionPath\n");
$gDebug && print("  :debug: pod path: $gPodPath\n");

# basic checks
-d "$gFlagsDir" && print "\nWARNING: flags dir $gFlagsDir does not exist.\n";
-d "$gPhotoCollectionPath" || die "Photo collection dir does not exist.";
-d "$gPodPath" || die "POD path does not exist.";

my $tries=0;

while($tries<5) {
	my $selectedPhoto=selectAFile($gPhotoCollectionPath);
	my $nsfw;
	if($selectedPhoto) {
		binmode STDOUT, ":utf8";
		my $xmlFile=replaceExtension($selectedPhoto, "xml");
        if (! -e $xmlFile) {
            debugMsg("No xml file for image $selectedPhoto");
            $filename="";
            $author="";
            $source="";
            $title = substr($selectedPhoto, length($gPhotoCollectionPath) + 1);
            $nsfw=0; # :fixme:
        } elsif (getXmlData($xmlFile, $filename, $author, $source, $title, $nsfw)<=0) {
            print STDERR "Error parsing XML file $fileToLoad.\n";
			exit(3);
		}
		if ($nsfw) {
			$tries++;
			next;
		}
		else {
			convertImage($selectedPhoto, $gPodPath."/"."photo-pod.png");
			printHtml($gPodPath."/"."photo-pod.html", "photo-pod.png", $author, $source, $title);
			last;
		}
	}
	else {
		print STDERR "Cannot choose a file from collection (invalid collection?)\n";
		exit(2);
	}
}

if ($tries>=5) {
	print STDERR "Too many tries. Something is wrong! \n";
	exit(4);
}
