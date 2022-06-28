#!/usr/bin/env perl

#use strict;
use Image::ExifTool;

#use Date::Parse;
#use Data::Dumper;

$gVersion="2022-06-07-0"; # :release:

#my @list = `ls -1 *JPG *jpg *jpeg *JPEG *.NEF *.nef`;
my @list = `find . -maxdepth 1 \\( \\( -iname '*.nef' -o -iname '*.jpg' \\) -o -iname '*.jpeg' \\) -printf "%f\n"`;

$gDebug=0;
$gDontRename=0;
$gFujiIsMyFuji=0;
$gSkipXmp=0;
$gSkipRename=0;
$gSkipReadOnly=0;

$gTagLocation=0;
$gTagCountryCode=0;
$gTagAuthor=0;
$gTagUrl=0;

sub execCmd {
	my $cmd=join('',@_);
	$cmd.=" > /dev/null ";
	$gDebug && print "  :debug: executing cmd: $cmd\n";
	my $status=system($cmd);
	if($status) {
		return 0;
	}
	else {
		return 1;
	}
}


sub dumpEveryTag {
	my $exifImg = new Image::ExifTool;
	print "EXIF data for file $_[0]\n";
	my $exifInfo = $exifImg->ImageInfo($_[0]);

	if($exifImg->GetValue("Error")) {
		return 0;
	}
	else {
		foreach my $family (0, 1, 2) {
			my @groupList=Image::ExifTool::GetAllGroups($family);
			foreach my $group (@groupList) {
				my @tagList=Image::ExifTool::GetAllTags($group);
				foreach my $tag (@tagList) {
					my $value=$exifImg->GetValue($tag, 'PrintConv');
					print "$family / $group / $tag = $value \n" if($value);
				}
			}
		}
		return 1;
	}
}


# $_[0] - filename
# $_[1] - prefix string (Cx_yymdd_)
sub processExifTags {
	my $exifImg = new Image::ExifTool;
	$gDebug && print "  :debug: extract exif from file $_[0]\n";
	my $exifInfo = $exifImg->ImageInfo($_[0]);

	if($exifImg->GetValue("Error")) {
		return 0;
	}

        # :debug:
        #foreach (sort keys %$exifInfo) {
        #    print "$_ => $$exifInfo{$_}\n";
        #}

	$_[1]="i";

	# get time
	my $dateTime=$exifImg->GetValue("DateTimeOriginal");
	$gDebug && print "  :debug: dateTime in EXIF = $dateTime\n";
	if($dateTime =~ /^([0-9]{4}):([0-9]{2}):([0-9]{2}) [0-9:]*$/) {
		$year=$1;
		$month=$2;
		$day=$3;
		$gDebug && print "  :debug: date=$year-$month-$day\n";
		$_[1].=sprintf("%02d", $year%100);
		if($month == 10) { $_[1].="a"; }
		elsif($month == 11) { $_[1].="b"; }
		elsif($month == 12) { $_[1].="c"; }
		else {
			$_[1].=$month%10;
		}
		$_[1].=sprintf("%02d", $day);
	}
	else {
		print STDERR "Invalid date/time in EXIF info!\n";
		return 0;
	}

	# get camera data
	my $cameraModel=$exifImg->GetValue("Model");
	$cameraModel =~ s/\s+$//; # trim spaces at the end
	my $cameraSn=$exifImg->GetValue("SerialNumber");
	$cameraSn =~ s/\s+$//; # trim spaces at the end
	my $cameraProd=$exifImg->GetValue("Make");
	$cameraProd =~ s/\s+$//; # trim spaces at the end
	$gDebug && print "\n  :debug: camera model: $cameraModel; serialNum: $cameraSn; producer: $cameraProd\n";
	my $cameraDetected=0;
	if($cameraModel eq "NIKON D80" && substr($cameraSn, -3) eq "942") {
		$_[1].="_c2";
		$cameraDetected=1;
	}
	elsif($cameraModel eq "NIKON D300" && substr($cameraSn, -3) eq "283") {
		$_[1].="_c3";
		$cameraDetected=1;
	}
	elsif($cameraModel eq "NIKON D600" && substr($cameraSn, -3) eq "196") {
		$_[1].="_c4";
		$cameraDetected=1;
	}
	elsif($cameraModel eq "NIKON Z 5" && substr($cameraSn, -2) eq "38") {
		$_[1].="_c5";
		$cameraDetected=1;
	}
	elsif($cameraModel eq "D5503") {
        # sony xperia z1 compact - assume it's mine
		$_[1].="_cz"; # this was c5 for a while
		$cameraDetected=1;
	}
	elsif($cameraProd eq "Sony" && $cameraModel eq "G8441") {
        # sony xperia xz1 compact - assume it's mine
		$_[1].="_c6";
		$cameraDetected=1;
	}
	elsif($cameraModel eq "FinePix S5600") {
		if($gFujiIsMyFuji) {
			$_[1].="_c1";
			$cameraDetected=1;
		}
		else {
			print STDERR "\nFuji FinePix S5600 camera detected. Assume it's my camera? ([Y]es/[a]ll/[n]o) ";
			$answer=<STDIN>;
			chomp($answer);
			$gDebug && print "\n  :debug: answer is: $answer\n";
			if($answer eq "y" || $answer eq "Y" || !$answer) {
				$_[1].="_c1";
				$cameraDetected=1;
			}
			elsif($answer eq "a" || $answer eq "A") {
				$gFujiIsMyFuji=1;
				$_[1].="_c1";
				$cameraDetected=1;
			}
		}
	}

	if(!$cameraDetected) {
		print STDERR "Unknown camera (model=$cameraModel, sn=$cameraSn)\n";
		$_[1].="_cx";
	}
	return 1;
}


# Return: the new name or 0 on error.
# Warning: the file might not exist if the -n option is specified (to create only a script).
sub renameFile {
	my $srcName=$_[0];
	my $dstName="";
	my $extension;

	# already renamed
	if ($_[0] =~ /^i[0-9aAbBcC]{5}_c[0-9]+_.*/) {
		print STDERR "$_[0] appears to be already renamed. Skipping.\n";
		return $_[0];
	}

	if($_[0] =~ /.*([0-9]{4})\.((nef)|(jpe?g)|(NEF)|(JPE?G))$/) {
		$gDebug && print "  :debug: number part: $1\n";
		$extension=lc($2);
		$gDebug && print "  :debug: extension: $extension\n";
		if(processExifTags($srcName, $dstName)) {
			$dstName.=$1;
			$dstName.="_co.$extension";
		}
		else {
			print STDERR "Error processing EXIF data from file $_[0]\n";
			return 0;
		}
	}
	else {
		print STDERR "Error: invalid filename for image: $_[0]\n";
		return 0;
	}
	if(-e "$dstName") {
		print STDERR "Filename $dstName exists. Aborting!\n";
		return 0;
	}
	if($gDontRename) {
		print "mv -i \"$srcName\" \"$dstName\" || echo \"Error renaming file $srcName to $dstName\"\n";
	}
	else {
		print "- $srcName -> $dstName\n";
		rename($srcName, $dstName) || return 0;
	}
	return $dstName;
}


sub readOnlyFile {
	$gDebug && print "  :debug: making file read-only: $_[0]\n";
	my $origPerm=((stat($_[0]))[2] & 07777) || return 0;
	my $newPerm=($origPerm & 07555);
	$gDebug && printf("  :debug: old perm: %o, new perm: %o\n", $origPerm, $newPerm);
	chmod($newPerm, $_[0]) || return 0;
}


# ret: 0 = error, 1 = ok
sub tagFile {
	$gDebug && print "  :debug: tagging file $_[0]\n";
	$fnameImg=$_[0];

    # remove extension from filename - not needed anymore,
    # sidecar files should have the same name + .xmp extension
	#$fnameImg =~ s/\.[^\.]+//;

	if (-e "$fnameImg.xmp") {
		print STDERR "$fnameImg.xmp already exists. Skipping.\n";
		return 0;
	}
	my $exifCmd="exiftool -o \"$fnameImg.xmp\" ";
	$exifCmd.=" -TagsFromFile @ ";
    $exifCmd.=" -Iptc:CodedCharacterSet=\"UTF8\" ";
    $exifCmd.=" -xmp:Location=\"$gTagLocation\" ";
    $exifCmd.=" -Iptc:Sub-location=\"$gTagLocation\" ";
	$exifCmd.=" -xmp:CountryCode=\"$gTagCountryCode\" ";
	$exifCmd.=" -xmp:Creator=\"$gTagAuthor\" ";
	#$exifCmd.=" -xmp:CreatorContactInfoCiUrlWork=\"$gTagUrl\" ";
	$exifCmd.=" -xmp:CreatorWorkURL=\"$gTagUrl\" ";
	$exifCmd.=$_[0];
	$gDebug && print "  :debug: exiftool cmd: $exifCmd\n";
	if(!execCmd($exifCmd)) {
		print STDERR "Error executing exiftool cmd $exifCmd\n";
		return 0;
	}
	else {
		return 1;
	}
}


sub processFiles {
	my $newName;
	for my $fname (@list) {
        chomp $fname;
		#if(!dumpEveryTag($fname)) {
		if(!$gSkipRename) {
			if(!($newName=renameFile($fname))) {
				print STDERR "Error renaming file $fname\n";
			}
		}
		$newName=$fname if($gDontRename || $gSkipRename);
		tagFile($newName) if(!$gSkipXmp);
		readOnlyFile($newName) if(!$gSkipReadOnly);
	}
}


sub readTags {
	print "Enter location (city, country, etc.): ";
	$gTagLocation=<STDIN>;
	chomp($gTagLocation);
	print "Enter country code (default=RO): ";
	$gTagCountryCode=<STDIN>;
	chomp($gTagCountryCode);
	$gTagCountryCode="RO" if(!$gTagCountryCode);
	print "Enter author (default='Tudor M. Pristavu, http://imago.sfd.ro'): ";
	$gTagAuthor=<STDIN>;
	chomp($gTagAuthor);
	$gTagAuthor="Tudor M. Pristavu, http://imago.sfd.ro" if(!$gTagAuthor);
	print "Enter URL (default='http://imago.sfd.ro'): ";
	$gTagUrl=<STDIN>;
	chomp($gTagUrl);
	$gTagUrl="http://imago.sfd.ro" if(!$gTagUrl);
	if($gDebug) {
		print "Using the following XMP tags:\n";
		print "  location: $gTagLocation\n";
		print "  country code: $gTagCountryCode\n";
		print "  author (creator): $gTagAuthor\n";
		print "  URL: $gTagUrl\n";
	}
}


sub displayUsage {
    print "photoRenameTool.pl ver. $gVersion\n\n";
	print "Usage: photoRenameTool.pl ";
	print "[ -d ] [ -n ] [ --no-rename ] [ --no-xmp ] [ --no-readonly ] [ -i <file> ]\n";
	print "  -d = debug on\n";
	print "  -n = don't actually rename, just generate a shell script for that\n";
	print "  -i = display every info for the specified file (only one file)\n";
	print "  --no-xmp = skip tagging with XMP metadata\n";
	print "  --no-rename = skip renaming\n";
	print "  --no-readonly = skip making image files read-only\n";
}


while(scalar @ARGV > 0) {
	if($ARGV[0] eq "-h") {
		displayUsage;
		exit 1;
	}
	elsif($ARGV[0] eq "-d") {
		$gDebug=1;
		shift(@ARGV);
	}
	elsif($ARGV[0] eq "-n") {
		$gDontRename=1;
		shift(@ARGV);
	}
	elsif($ARGV[0] eq "--no-xmp") {
		$gSkipXmp=1;
		shift(@ARGV);
	}
	elsif($ARGV[0] eq "--no-rename") {
		$gSkipRename=1;
		shift(@ARGV);
	}
	elsif($ARGV[0] eq "--no-readonly") {
		$gSkipReadOnly=1;
		shift(@ARGV);
	}
	elsif($ARGV[0] eq "-i") { # display info
		if($ARGV[1]) {
			dumpEveryTag($ARGV[1]);
			exit 0;
		}
		else {
			print STDERR "Error: Invalid filename\n";
			exit 1;
		}
	}

	else {
		print STDERR "Error: invalid argument\n";
		exit(1);
	}
}

if($gDontRename) {
	if($gDebug) {
		print STDERR "You should not use -d (debug) and -n (don't run, generate script) together.\n";
		exit 2;
	}
	if(!$gSkipXmp) {
		print STDERR "Warning: the generated script will NOT rename the .xmp files that will be created!\n";
	}
	print "#!/bin/sh\n\n";
}


if(!$gSkipXmp) {
	readTags;
}

processFiles;

if (!$gSkipXmp) {
    print STDERR "\n\033[0;31m* Reread metadata in digikam !\033[0m\n\n";
}
