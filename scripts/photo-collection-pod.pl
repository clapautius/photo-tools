#!/usr/bin/perl

# new picture of the day
# script that selects a random picture from a
#  photo collection (directories containing images and associated .xml files)

# E.g. config file (strip first 2 chars)
# # comment
# fpath old/ -2
# tags safety-moderate 0

use XML::XPath;
use XML::XPath::XMLParser;
use File::Find;

$gVersion = "2018-10-31-0";
$gDebug = 0;
$gHeavyDebug = 0;
$gPhotoCollectionPath = $ENV{'HOME'} . "/var/alternatives/colectie-foto";
$gPodPath = $ENV{'HOME'} . "/var/run/pod/";
$gFlagsDir = $ENV{'HOME'} . "/var/settings/colectie-foto-flags";
$gImageFnameRegexp="\.(jpe?g|tiff?|png)\$";

# :fixme: replace with a struct or smth.
@gProbabilitiesAlterType = ();
@gProbabilitiesAlterValue = ();
@gProbabilitiesalterFactor = ();

sub debugMsg {
	$gDebug && print ":debug: $_[0]\n";
}


sub debugHeavyMsg {
	$gHeavyDebug && print ":debug: $_[0]\n";
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
# $_[5] : tags
sub getXmlData
{
    debugMsg("  :debug: trying to load xml file $_[0]");
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
	$_[5] = $nodeset->to_literal();
    debugHeavyMsg("tags: $_[5]");

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
	print "photo-collection-pod.pl ver. $gVersion\n";
	print "usage: photo-collection-pod.pl [ -t ] [ -c <collectionPath> ] [ -d <destination> ] [ -v ]\n";
	print "  -v = verbose on\n";
    print "  -t : run tests\n";
}


sub fpathReplaceExtension {
	$_[0] =~ /(.*)\..*/;
	return "$1.$_[1]";
}


# returns an array (path, filename, extension)
sub fpathSplit {
    debugHeavyMsg("fpathSplit: splitting $_[0]");
    my @arr;
    if ($_[0] =~ m|(.*)/+(.*)\.(.*)$|) {
        @arr = ($1, $2, $3);
    } elsif ($_[0] =~ /(.*)\.(.*)$/) {
        @arr = ("", $1, $2);
    } else {
        @arr = ("", $_[0], "");
    }
    debugHeavyMsg("fpathSplit: $arr[0], $arr[1], $arr[2]");
    return @arr;
}


sub isImageFile {
	#return ($_[0] =~ /$gImageFnameRegexp/i); # :debug:
    my $rc = (-f "$_[0]" && $_[0] =~ m/$gImageFnameRegexp/i);
	debugHeavyMsg("isImageFile: result for $_[0] is $rc");
    return $rc;
}


# images (with full path)
@gImgFileList = ();

# images (only filename, without path or extension)
@gImgFileOnlyList = ();

# $_[0] : old probability
# $_[1] : index in gProbabilitiesAlterFactor array
sub alterProbability {
    my $ret = $_[0];
    if ($gProbabilitiesAlterFactor[$_[1]] eq "-1") {
        $ret = $_[0] - 1;
    } elsif ($gProbabilitiesAlterFactor[$_[1]] eq "-2") {
        $ret = $_[0] - 2;
    } elsif ($gProbabilitiesAlterFactor[$_[1]] eq "+2") {
        $ret = $_[0] + 2;
    } elsif ($gProbabilitiesAlterFactor[$_[1]] eq "*10") {
        $ret = $_[0] * 10;
    } elsif ($gProbabilitiesAlterFactor[$_[1]] eq "0") {
        $ret = 0;
    };
    return $ret;
}


sub processFile {
	debugHeavyMsg("Analyzing file / dir $File::Find::name"); # :debug:
	if (isImageFile($File::Find::name)) {
        $imgPath = $File::Find::name;
		debugMsg("Analyzing image file $File::Find::name");
        ($fpath, $fname, $fext) = fpathSplit($imgPath);

        # check if not already present
        foreach(@gImgFileOnlyList) {
            if ($_ eq $fname) {
                print "\n\nERROR: Duplicate image: $_\n\n";
                exit(3);
            }
        }
        push(@gImgFileOnlyList, $fname);

		my $flagFile;
		my $probability=3;

        # read data from xml (if available)
        my $xmlFile = fpathReplaceExtension($imgPath, "xml");
        if (! -e $xmlFile) {
            debugMsg("No xml file for image $imgPath");
            $filename = "";
            $author = "";
            $source = "";
            $title = $fname;
            $tags = "";
        } elsif (getXmlData($xmlFile, $filename, $author, $source, $title, $tags)<=0) {
            print STDERR "Error parsing XML file $fileToLoad.\n";
			exit(3);
		}

        # alter probabilities
        for my $i (0 .. $#gProbabilitiesAlterType) {
            debugHeavyMsg("checking prob for index $i for $imgPath; type=$gProbabilitiesAlterType[$i], value=$gProbabilitiesAlterValue[$i], factor=$gProbabilitiesAlterFactor[$i]");

            if ($gProbabilitiesAlterType[$i] eq 'fpath') {
                my $probregex = $gProbabilitiesAlterValue[$i];
                if ($imgPath =~ m|$probregex|) {
                    debugMsg("$img matches $probregex");
                    $probability = alterProbability($probability, $i);
                    debugMsg("New probability is $probability");
                }
            }

            if ($gProbabilitiesAlterType[$i] eq 'tags') {
                my $probregex = $gProbabilitiesAlterValue[$i];
                if ($tags =~ m|$probregex|) {
                    debugMsg("tags $tags (for image $imgPath) match $probregex");
                    $probability = alterProbability($probability, $i);
                    debugMsg("New probability is $probability");
                }
            }

        }

		for ($i=0; $i<$probability; $i++) {
			push(@gImgFileList, $File::Find::name);
		}
	}
}


sub readProbabilities {
    open(my $fh, '<:encoding(UTF-8)', "$ENV{HOME}/.photo-collection-pod")
        or die "Could not open file $ENV{HOME}/.photo-collection-pod $!";

    while (my $row = <$fh>) {
        chomp $row;
        if ($row =~ "^#") {
            debugMsg("continue (next)");
            next;
        }
        @rowElts = split /\s+/, $row;
        # :fixme: validate data
        push(@gProbabilitiesAlterType, $rowElts[0]);
        push(@gProbabilitiesAlterValue, $rowElts[1]);
        push(@gProbabilitiesAlterFactor, $rowElts[2]);

    }
    debugMsg("probabilities regex: @gProbabilitiesFactor");
}


sub runTests {
    debugMsg("Running tests");

    $testIn="file";
    @arr = fpathSplit($testIn);
    print "$testIn : @arr\n";

    $testIn="file.txt";
    @arr = fpathSplit($testIn);
    print "$testIn : @arr\n";

    $testIn="/file.txt";
    @arr = fpathSplit($testIn);
    print "$testIn : @arr\n";

    $testIn = "/home/file.txt";
    @arr = fpathSplit($testIn);
    print "$testIn : @arr\n";
}


# $_[0] - collection path
# $_[1] - flags dir
# returns the image filename (e.g. photo.jpg)
# to obtain the xml filename, use fpathReplaceExtension()
sub selectAFile {
	find({ wanted => \&processFile, follow => 1, no_chdir => 1 } , ( $_[0] ) );
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
#debugMsg(fpathReplaceExtension("bibi.jpg", "xml"));
#exit(0);

while(scalar @ARGV > 0) {
	if($ARGV[0] eq "-h") {
		displayUsage;
		exit 1;
	}
	elsif($ARGV[0] eq "-t") {
        runTests;
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
readProbabilities;
binmode STDOUT, ":utf8";

my $selectedPhoto=selectAFile($gPhotoCollectionPath);
if($selectedPhoto) {
    my $xmlFile=fpathReplaceExtension($selectedPhoto, "xml");
    # :fixme: don't read the xml twice - try to keep the old data in memory
    # (or not?, maybe that's too much info? to check)
    if (! -e $xmlFile) {
        debugMsg("No xml file for image $selectedPhoto");
        $filename = "";
        $author = "";
        $source = "";
        $title = substr($selectedPhoto, length($gPhotoCollectionPath) + 1);
        $tags = "";
    } elsif (getXmlData($xmlFile, $filename, $author, $source, $title, $tags)<=0) {
        print STDERR "Error parsing XML file $fileToLoad.\n";
        exit(3);
    }
    convertImage($selectedPhoto, $gPodPath."/"."photo-pod.png");
    printHtml($gPodPath."/"."photo-pod.html", "photo-pod.png", $author, $source, $title);
}
else {
    print STDERR "Cannot choose a file from collection (invalid collection?)\n";
    exit(2);
}
