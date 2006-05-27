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
use English '-no_match_vars';

## It's not clear how well this script actually works.

my $scheme_dir = "/usr/local/scmutils";
my $scheme = "$scheme_dir/mit-scheme/bin/scheme -library $scheme_dir/mit-scheme/lib";
my @bands = `find /usr/local/scmutils/ -name "edwin-mechanics.com"`; # Oh, the hacks we pull...
my $band = $bands[0];
chomp($band);
my $command = "echo '$ARGV[0]' | $scheme -heap 6000 -constant 2000 -band $band -eval '(load \"compile.scm\")' '(load \"load\")'";
print $command . "\n";
my $result = `$command`;
if(defined $ARGV[1]) {
    print $result . "\n";
}
if($CHILD_ERROR != 0) {
    print $result . "\n";
    exit($CHILD_ERROR);
}
