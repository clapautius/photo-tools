#!/usr/bin/perl

use Cwd;

$gVersion="0.2"; # :release:

$gDebug=0;

$gTagsTrash=0;

sub lastComponentOfPath {
	$_[0] || return 0;
	my $path=$_[0];
	my $idx=rindex($path, "/");
	if($idx) {
		return substr($path, $idx+1, length($path)-$idx-1);
	}
	else {
		return $path;
	}
}

sub splitNameAndExtension {
	$_[0] || return 0;
	my $fileName=$_[0];
	my $idx=rindex($fileName, ".");
	if($idx) {
		$_[1]=substr($fileName, 0, $idx);
		$_[2]=substr($fileName, $idx+1, length($fileName)-$idx-1);
	}
	else {
		$_[1]=$path;
		$_[2]="";
	}
	return 1;
}


sub recDoStuff {
	$gDebug && print "  :debug: recDoStuff(): dir=$_[1]\n";
	my $curDir=getcwd();
	my $lastComp=lastComponentOfPath($_[1]);
	chdir($lastComp) || return 0;
	$_[0]->($_[1]);

	opendir(DIR, ".") || return 0;
	my @dirContent=readdir(DIR);
	closedir(DIR);

	foreach $entryInDir (@dirContent) {
		if("$entryInDir" ne "." && "$entryInDir" ne ".." && -d "$entryInDir") {
			recDoStuff($_[0], "$_[1]"."/"."$entryInDir");
		}
	}
	chdir($curDir);
}


sub printContentOfDir {
	system("ls");
}

sub checkOrphan {
	$gDebug && print ("  :debug: Checking orphans in ".getcwd()."\n");
	opendir(DIR, ".") || return 0;
	my @dirContent=readdir(DIR);
	closedir(DIR);
	my $name, $extension, $imgName, $imgExtension;

	foreach $entryInDir (@dirContent) {
		if (-f "$entryInDir") {
			splitNameAndExtension($entryInDir, $name, $extension);
			#$gDebug && print ("  :debug: analyzing file $entryInDir (name=$name, ext=$extension)\n");
			if ($extension eq "nef" || $extension eq "NEF" ||
			   $extension eq "jpg" || $extension eq "JPG" ||
			   extension eq "jpeg" || extension eq "JPEG") {
				if(! (-f "$entryInDir".".xmp" || -f "$entryInDir".".XMP") ) {
					print "Image file $entryInDir has no .xmp sidecar file.\n";
				}
			}
			elsif ($extension eq "xmp" || $extension eq "XMP") {
                splitNameAndExtension($name, $imgName, $imgExtension);
				if(! (-f "$imgName".".nef" || -f "$imgName".".NEF" ||
					  -f "$imgName".".jpg" || -f "$imgName".".JPG" ||
					  -f "$imgName".".jpeg" || -f "$imgName".".JPEG")) {
					print "Tags file $entryInDir has no image file.";
					if ($gTagsTrash) {
						qx/mv -i "$entryInDir" "$gTagsTrash"/;
						if ($? != 0) {
							print "\n";
							print STDERR "Error moving file $entryInDir to $gTagsTrash\n";
						}
						else {
							print " Moved to $gTagsTrash.\n";
						}
					}
					else {
						print "\n";
					}
				}
			}
		}
	}
}


sub displayUsage {
	print "Usage: photoAdminTool.pl ";
	print "[ -d ] [ --tags-trash | -t <dir> ] <cmd>\n";
	print "<cmd> : co = check orphans;\n";
	print "  -d = debug on\n";
	print "  --tags-trash = move orphan tags files to this dir\n\n";
}


while(scalar @ARGV > 1) {
	if($ARGV[0] eq "-h") {
		displayUsage;
		exit 1;
	}
	elsif($ARGV[0] eq "--tags-trash" || $ARGV[0] eq "-t") {
		if($ARGV[1]) {
			$gTagsTrash=$ARGV[1];
			if (! -d "$gTagsTrash") {
				print STDERR "Error: $gTagsTrash is not a directory.\n";
				exit 2;
			}
		}
		else {
			print STDERR "Error: Invalid path (--tags-trash/-t param).\n";
			exit 1;
		}
		shift(@ARGV);
		shift(@ARGV);
	}
}

if ($ARGV[0] eq "-h") {
	displayUsage;
	exit 1;
}
elsif ($ARGV[0] eq "co") {
	recDoStuff(\&checkOrphan, ".");
}
else {
	print STDERR "Error: unknown command!\n\n";
	displayUsage;
	exit 1;
}
