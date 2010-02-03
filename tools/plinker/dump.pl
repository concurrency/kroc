#!/usr/bin/env perl
#
#  Perl code for linking and assembling bytecode for the TVM
#  Copyright (C) 2008, 2009 Carl Ritson <cgr@kent.ac.uk>
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

require 'ETC.pm';
require 'TCOFF.pm';

package main;

use strict;
use Data::Dumper;

# ETC Decoder
my $etc		= new Transputer::ETC ();

# TCOFF Decoder
my $tcoff	= new Transputer::TCOFF ();

# Command Line Parsing
my ($first, @files) = @ARGV;
my $comments;
if ($first eq '-C') {
	$comments = 1;
} elsif ($first) {
	unshift (@files, $first) if $first ne '--';
}

if (!@files) {
	print "tce-dump.pl [-C] <file> [<file> ...]\n";
	exit 1;
}

foreach my $file (@files) {
	# Decode
	my $data = $tcoff->read_file ($file);
	die "Failed to read $file" if !$data;

	if ($comments) {
		foreach my $comment (@{$data->{'COMMENT'}}) {
			my $data = $etc->decode_str ($comment->{'data'});
			print "$data\n";
		}
	} else {
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
		my $bytes	= unpack ('H*', $arg);
		my @bytes	= ($bytes =~ /(..)/g);
		my $text	= join (' ', @bytes) . ' ';
		my @lines	= ($text =~ /(.{0,48})/g);
		print "$name ", length ($arg), "\n";
		foreach my $line (@lines) {
			print "\t$line\n" if $line;
		}
	} elsif ($name eq '.LABEL' && ref ($arg) =~ /^ARRAY/) {
		my $l1 = $arg->[0];
		my $l2 = $arg->[1];
		if ($l2->{'arg'} < 0) {
			print_op ($indent, $l1);
		} else {
			print $indent, $l1->{'name'}, "\t", $l1->{'arg'}, "-L", $l2->{'arg'}, "\n";
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
