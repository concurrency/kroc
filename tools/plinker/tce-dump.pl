#!/usr/bin/perl
#
#  Perl code for linking and assembling bytecode for the TVM
#  Copyright (C) 2008 Carl Ritson <cgr@kent.ac.uk>
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with this program; if not, write to the Free Software
#  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
#

use strict;
use Data::Dumper;

require 'ETC.pm';
require 'TCOFF.pm';

# Constants
my $instruct_h	= "/Users/cgr/src/kroc-svn/tools/occ21/include/instruct.h";
my $tcoff_h	= "/Users/cgr/src/kroc-svn/tools/occ21/include/tcoff.h";

# ETC Decoder
my $etc		= new Transputer::ETC ($instruct_h);

# TCOFF Decoder
my $tcoff	= new Transputer::TCOFF ($tcoff_h);

# Command Line Parsing
my $file = shift;
if (!$file) {
	print "tce-dump.pl <file>\n";
	exit 1;
}

# Decode
my $data = $tcoff->read_file ($file);
die "Failed to read $file" if !$data;

# Textualise
foreach my $section (@{$data->{'LOAD_TEXT'}}) {
	my @text	= $etc->decode_load_text ($section->{'data'});
	my $indent	= "";

	if (!@text) {
		print STDERR "Failed to decode a text segment...\n";
		next;
	}

	foreach my $op (@text) {
		my $name = $op->{'name'};

		print_op ($indent, $op);
		
		if ($name eq '.PROC') {
			$indent = "\t";
		} elsif ($name eq '.GLOBALEND' || $name eq '.SECTIONLAB') {
			$indent = "";
		}
	}
}

exit 0;

sub print_op ($$) {
	my ($indent, $op)	= @_;
	my %indent		= (
		'.PROC'		=> "",
		'.STUBNAME'	=> "",
		'.GLOBALEND'	=> ""
	);

	my $name	= $op->{'name'};
	my $arg		= $op->{'arg'};

	if (exists ($indent{$name})) {
		$indent = $indent{$name};
	};

	if ($name eq '.OCCCOMMENT') {
		$arg =~ s/[\r\n]/ /gs;
		print "-- $arg\n";
	} elsif ($name =~ /^\.(SET|SECTION)LAB$/) {
		print "\n" if $name eq '.SECTIONLAB';
		print ".L$arg:\n";
	} elsif ($name =~ /\..*BYTES$/) {
		my $bytes	= unpack ('h*', $arg);
		my @bytes	= ($bytes =~ /(..)/g);
		my $text	= join (' ', @bytes) . ' ';
		my @lines	= ($text =~ /(.{0,48})/g);
		print "$name ", length ($arg), "\n";
		foreach my $line (@lines) {
			print "\t$line\n" if $line;
		}
	} elsif (exists ($op->{'arg'})) {
		if (ref ($arg)) {
			my @params = ref ($arg) =~ /^ARRAY/ ? @$arg : ( $arg );
			my @tparam;
			foreach my $p (@params) {
				if (ref ($p)) {
					print_op ($indent, $p);
				} else {
					push (@tparam, $p);
				}
			}
			if (@tparam) {
				print $indent, "$name\t", join (', ', @tparam), "\n";
			} else {
				#print $indent, $name, "\n";
			}
		} else {
			print $indent, "$name\t$arg\n";
		}
	} else {
		print $indent, "$name\n";
	}
}
