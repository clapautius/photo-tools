#!/usr/bin/perl

# new picture of the day
# script that selects a random picture from a
#  photo collection - directories containing images (with or without associated .xml files)

# Config file format
# cmd-match-type cmd-match-value commands (comma separated)
# cmds: +<num>, -<num>, =<num>, !
#   +<num> : increase probability
#   -<num> : decrease probability
#   =<num> : set probability
#   ! : stop processing other commands for this file
# E.g.
# # comment
# fpath old/ -2
# tags safety-moderate =0,!

use XML::XPath;
use XML::XPath::XMLParser;
use File::Find;

$gVersion = "2018-12-10-0";
$gDebug = 0;
$gHeavyDebug = 0;
$gPhotoCollectionPath = $ENV{'HOME'} . "/var/alternatives/colectie-foto";
$gPodPath = $ENV{'HOME'} . "/var/run/pod/";
$gFlagsDir = $ENV{'HOME'} . "/var/settings/colectie-foto-flags";
$gImageFnameRegexp="\.(jpe?g|tiff?|png)\$";

@gConfigCmds = ();

sub debugMsg {
	$gDebug && print ":debug: $_[0]\n";
}


sub debugHeavyMsg {
	$gHeavyDebug && print ":debug: :heavy: $_[0]\n";
}


# $_[0] : input image
# $_[1] : output image
# $_[2] : max width
# $_[3] : max height
sub convertImage
{
	my $skipResize = 1;
	$photoFile = $_[0];
	$destinationFile = $_[1];
	$maxWidth = $_[2];
	$maxHeight = $_[3];

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
	if($currentWidth<$maxWidth) {
		$gDebug && print "  :debug: Skipping resize \n";
		$resizePercent2=100;
	}
	else {
		$resizePercent2=$maxWidth*100/$currentWidth;
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
    debugMsg("  trying to load xml file $_[0]");
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


# $_[0] - output html
# $_[1] - image src
# $_[2] - author
# $_[3] - source (or path without filename)
# $_[4] - title (or filename)
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
  img {
    /*border: 1px solid white;*/
    margin-top: 2px;
  }

div.details {
  background-color: #191919;
  color: #dcc;
  /*font-weight: bold;*/
  /*width: 32%;*/
  width: 80%;
  margin: 0.5em;
  margin-top: 15px;
  border: 2px solid #111;
  /* border-style: none; */
  /*
  position: fixed;
  top: auto;
  right: 5%;
  bottom: 10%;
  */
  /*font-size: small;*/
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
	print HTMLFILE "<br/>\n";
    if ($_[3]) {
        print HTMLFILE "($_[3])<br>\n";
    }
	print HTMLFILE '
</div>
</div><br/> <!-- one BR is space for KDE icons -->
</body>
</html>';
	close(HTMLFILE);
}


sub displayUsage {
	print "photo-collection-pod.pl ver. $gVersion\n";
	print "usage: photo-collection-pod.pl [ -t ] [ -c <collectionPath> ] [ -d <destination> ] [ -v ] [ -m <max_width>x<max_height> ]\n";
	print "  -v : verbose on\n";
    print "  -t : run tests\n";
    print "  -m : max dimensions (e.g. 500x500)\n";
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


# The function returns ($continue, $newProbability)
# $_[0] : cmd
# $_[1] : old probability
sub executeOneCommand {
    my $newVal = $_[1];
    my $cont = 1;
    if ($_[0] =~ /^\s*\+\s*(\d+)\s*$/) {
        $newVal += $1;
    }
    if ($_[0] =~ /^\s*-\s*(\d+)\s*$/) {
        $newVal -= $1;
    }
    if ($_[0] =~ /^\s*=\s*(\d+)\s*$/) {
        $newVal = $1;
    }
    if ($_[0] =~ /^\s*!\s*$/) {
        $cont = 0;
    }
    return ($cont, $newVal);
}


# The function returns new probability (0 <= $probability <= 20)
# $_[0] - image full path
# $_[1] - image tags
# $_[2] - original probability
sub executeConfigCmds {
    my $imgPath = $_[0];
    my $tags = $_[1];
    my @result = (1, 0);
    my $cont = 1;
    my $probability = $_[2];

    debugHeavyMsg("");
    debugHeavyMsg("checking filters for $imgPath");
    # alter probabilities
    ALL_CMDS: foreach(@gConfigCmds) {
        debugHeavyMsg("cmd-match-type : " . $$_{'cmd-match-type'} .
                      "; cmd-match-value : " . $$_{'cmd-match-value'} .
                      "; commands : " . $$_{'commands'});
        if ($$_{'cmd-match-type'} eq 'fpath') {
            my $probregex = $$_{'cmd-match-value'};
            if ($imgPath =~ m|$probregex|) {
                debugMsg("$imgPath matches $probregex");
                my @commands = split(',', $$_{'commands'});
                debugHeavyMsg("executing commands: @commands");
                foreach (@commands) {
                    ($cont, $probability) = executeOneCommand($_, $probability);
                    debugMsg("New probability (after command $_) is $probability");
                    if ($cont == 0) {
                        debugMsg("  stop processing filters");
                        last ALL_CMDS;
                    }
                }
            }
        }
        if ($$_{'cmd-match-type'} eq 'tags') {
            my $probregex = $$_{'cmd-match-value'};
            if ($tags =~ m|$probregex|) {
                debugMsg("tags $tags (for image $imgPath) match $probregex");
                my @commands = split(',', $$_{'commands'});
                debugHeavyMsg("executing commands: @commands");
                foreach (@commands) {
                    ($cont, $probability) = executeOneCommand($_, $probability);
                    debugMsg("New probability (after command $_) is $probability");
                    if ($cont == 0) {
                        debugMsg("  stop processing filters");
                        last ALL_CMDS;
                    }
                }
            }
        }
    }
    $probability = 0 if ($probability < 0);
    $probability = 20 if ($probability > 20);
    debugMsg("Final probability is $probability");
    return $probability;
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

        $probability = executeConfigCmds($imgPath, $tags, $probability);

		for ($i=0; $i<$probability; $i++) {
			push(@gImgFileList, $File::Find::name);
		}
	}
}


sub readProbabilities {
    return 0 if (! -e "$ENV{HOME}/.photo-collection-pod");
    open(my $fh, '<:encoding(UTF-8)', "$ENV{HOME}/.photo-collection-pod")
        or die "Could not open file $ENV{HOME}/.photo-collection-pod $!";

    while (my $row = <$fh>) {
        chomp $row;
        if ($row =~ "^#") {
            debugMsg("continue (next)");
            next;
        }
        @rowElts = split /\s+/, $row;
        my %configCmd = ('cmd-match-type' => $rowElts[0],
                         'cmd-match-value' => $rowElts[1],
                         'commands' => $rowElts[2]);
        push(@gConfigCmds, \%configCmd);
    }
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

    my %elt = ( 'cmd-match-type' => 'fpath',
                'cmd-match-value' => 'match',
                'commands' => '+2' );
    push(@gConfigCmds, \%elt);
    my ($cont, $factor) = executeConfigCmds("fpath-that-matches", "no-tags");
    print "cont=$cont, factor=$factor\n (expected: 1, 2)\n";

    ($cont, $factor) = executeOneCommand("+52", -2);
    print "cont=$cont, factor=$factor\n (expected: 1, 50)\n";

    ($cont, $factor) = executeOneCommand("=32", 0);
    print "cont=$cont, factor=$factor\n (expected: 1, 32)\n";

    ($cont, $factor) = executeOneCommand(" ! ", 0);
    print "cont=$cont, factor=$factor\n (expected: 0, 0)\n";

    %elt = ( 'cmd-match-type' => 'fpath',
             'cmd-match-value' => 'match',
             'commands' => '-7,!' );
    push(@gConfigCmds, \%elt);
    my ($cont, $factor) = executeConfigCmds("fpath-that-matches", "no-tags");
    print "cont=$cont, factor=$factor\n (expected: 0, -7)\n";

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
my $maxWidth = 950;
my $maxHeight = 780;
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
	elsif($ARGV[0] eq "-m") {
		if($ARGV[1] && $ARGV[1] =~ /(\d+)x(\d+)/) {
			$maxWidth = $1;
            $maxHeight = $2;
		}
		else {
			print STDERR "Error: Invalid dimensions (-m param).\n";
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

debugMsg("photo collection path: $gPhotoCollectionPath");
debugMsg("pod path: $gPodPath");
debugMsg("max width: $maxWidth, max height: $maxHeight");

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
        my $filepath = substr($selectedPhoto, length($gPhotoCollectionPath) + 1);
        my ($a, $b, $c) = fpathSplit($filepath);
        $title = $b . "." . $c;
        $source = $a;
        $tags = "";
    } elsif (getXmlData($xmlFile, $filename, $author, $source, $title, $tags)<=0) {
        print STDERR "Error parsing XML file $fileToLoad.\n";
        exit(3);
    }
    convertImage($selectedPhoto, $gPodPath."/"."photo-pod.png",
                 $maxWidth, $maxHeight);
    $source =~ s|^https?://||;
    printHtml($gPodPath."/"."photo-pod.html", "photo-pod.png", $author, $source, $title);
}
else {
    print STDERR "Cannot choose a file from collection (invalid collection?)\n";
    exit(2);
}
