#!/usr/bin/perl -w

### ----------------------------------------------------------------------
### Copyright 2005 Alexey Radul and Rebecca Frankel.
### ----------------------------------------------------------------------
### This file is part of The Symmetriad.
### 
### The Symmetriad is free software; you can redistribute it and/or modify
### it under the terms of the GNU General Public License as published by
### the Free Software Foundation; either version 2 of the License, or
### (at your option) any later version.
### 
### The Symmetriad is distributed in the hope that it will be useful,
### but WITHOUT ANY WARRANTY; without even the implied warranty of
### MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
### GNU General Public License for more details.
### 
### You should have received a copy of the GNU General Public License
### along with The Symmetriad; if not, write to the Free Software
### Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
### ----------------------------------------------------------------------

use strict;

### PARAMETERS ###

# Command line controllable options
my %opt = 
    ("outfile", "playout/bar.ps",
     "infile", "playout/H3-play-2.skel",
     "basedir", ".",
     "format", "ps",
     "fullxform", "",
     "4xform", "",
     "3xform", "",
     "axes", "0 1 2",
     );

# Their help lines
my %optHelp =
    ("outfile", "  Filename where to put output",
     "infile", "   Filename whence to take input",
     "basedir", "  Base directory for relative filenames",
     "format", "   Output format",
     "fullxform", "5x5 specified transform for the object",
     "4xform", "   4x4 specified transform for the object",
     "3xform", "   3x3 specified transform for the object",
     "axes", "  Which dimensions to show in perspective",
     );

### GLOBAL STATE ###

my $my_name = "render.pl";
my $xform_string = "";

### CODE ###

parseOptions();
die("Input file $opt{infile} does not exist") unless -e $opt{infile};
die("Input file $opt{infile} is unreadable") unless -r $opt{infile};
computeXForm();
createImage();

sub parseOptions {
    while(@ARGV > 0) {
	my $opt = shift @ARGV;
	if($opt eq "-help") {
	    usage();
	    exit(0);
	}
	if($opt =~ /^-(.*)$/ && @ARGV != 0) { # Really is an option
	    if(defined($opt{$1})) { # A real option, too
		$opt{$1} = shift @ARGV;
	    } else {
		print STDERR "Unrecognized option -$1.\n";
		print STDERR "For options list, run $my_name -help\n";
		exit(1);
	    }
	} else { # Something's wrong
	    print STDERR "Invalid argument format.\n";
	    print STDERR "For options list, run $my_name -help\n";
	    exit(1);
	}
    }
}

sub usage {
    print "$my_name, written by Alexey Radul for The Symmetriad, Novemeber 2004.\n";
    print "Usage:  $my_name [option argument]...\n";
    print "Function:  Uses Geomview to convert .skel files produced by The Symmetriad proper into image files.\n";
    print "Accepted options are:\n";
    print "  -help:           Print this help and exit.\n";
    foreach my $option (sort keys %opt) {
	print "  -$option:  $optHelp{$option}  Default:  $opt{$option}\n";
    }
}

sub increase_dimension {
    my $orig_dim = shift;
    my @orig_array = @_;

    my @newelts = ();
    for(my $i = 0; $i < $orig_dim; $i++) {
	for(my $j = 0; $j < $orig_dim; $j++) {
	    $newelts[($orig_dim+1)*$i + $j] = $orig_array[$orig_dim*$i + $j];
	}
	$newelts[($orig_dim+1)*$i + $orig_dim] = 0;
	$newelts[($orig_dim+1)*$orig_dim + $i] = 0;
    }
    $newelts[($orig_dim+1)*($orig_dim+1)-1] = 1;
    return @newelts;
}    

sub computeXForm {
    if(!($opt{"3xform"} eq "") && $opt{"4xform"} eq "") {
	my @elts = split(/\s+/, $opt{"3xform"});
	my @newelts = increase_dimension(3, @elts);
	$opt{"4xform"} = join(" ", @newelts);
    }

    print $opt{"4xform"} . "\n";

    if(!($opt{"4xform"} eq "") && $opt{fullxform} eq "") {
	my @elts = split(/\s+/, $opt{"4xform"});
	my @newelts = increase_dimension(4, @elts);
	$opt{fullxform} = join(" ", @newelts);
    }

    if($opt{fullxform} eq "") {
	$xform_string = "ntransform { 5 5  1 0 0 0 0  0 1 0 0 0  0 0 1 0 0  0 0 0 1 0  0 0 0 0 1 }";
    } else {
	$xform_string = "ntransform { 5 5 $opt{fullxform} }";
    }
}
    
sub createImage {
    my @geomview_commands = 
	('(delete allcams)',
	 '(dimension 4)',
	 '(new-camera "autocam")',
	 '(ND-axes "autocam" "default" ' . "$opt{axes})",
	 "(load $opt{infile} geometry)",
	 "(ND-xform g1 $xform_string)",
	 '(backcolor "autocam" 1 1 1)',
	 '(bbox-draw "g1" off)',
	 '(look World)',
	 '(snapshot "autocam" ' . "$opt{outfile} $opt{format})",
	 '(quit)',
	 );

    my $shell_command = "echo '" . join(" ", @geomview_commands) . " ' | geomview -c - -wins 0 -nopanels";
    #print $shell_command . "\n";
    print `$shell_command` . "\n";
}
