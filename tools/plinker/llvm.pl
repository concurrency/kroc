#!/usr/bin/env perl
#
#  Perl code for translating ETC to LLVM assembly
#  Copyright (C) 2009 Carl Ritson <cgr@kent.ac.uk>
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
require 'LLVM.pm';
require 'TCOFF.pm';
require 'TEncode.pm';

package main;

use strict;
use Data::Dumper;

# ETC Decoder
my $etc		= new Transputer::ETC ();

# LLVM Coder
my $llvm	= new Transputer::LLVM ();

# TCOFF Decoder
my $tcoff	= new Transputer::TCOFF ();

# Options
my $output;
my $verbose;
my @files;

# Command Line Parsing
my @args	= @ARGV;
my $standalone	= 0;
my $options 	= 1;
while (my $arg = shift @args) {
	if ($options && $arg eq '--') {
		$options 		= 0;
	} elsif ($options && $arg eq '-v') {
		$verbose		= 1;
		$etc->{'verbose'} 	= 1;
		$tcoff->{'verbose'}	= 1;
	} elsif ($options && $arg eq '-s') {
		$standalone		= 1;
	} elsif ($options && $arg eq '-o') {
		$output 		= shift @args;
	} else {
		push (@files, $arg);
	}
}

if (!$output) {
	$output = $files[-1];
	$output =~ s/\.tce$/.llvm/i;
}

if (!$output || !@files) {
	print "llvm-coder.pl [-v] [-s] [-o <name>] <file>\n";
	exit 1;
}

# Load ETC
my $last_file;
my $endian;
my %etc_file;
my @etc;
my $objects = [];

foreach my $file (@files) {
	my $data = $tcoff->read_file ($file);
	die "Failed to read $file" if !$data;

	# Check endian
	if ($data->{'.ENDIAN'}) {
		my $file_endian = $data->{'.ENDIAN'}->[0];
		die "Inconsistent endian settings, $file is $file_endian"
			if defined ($endian) && $file_endian ne $endian;
		$endian = $file_endian;
	}

	# Decode text sections
	my @texts;
	foreach my $section (@{$data->{'LOAD_TEXT'}}) {
		my @text = $etc->decode_load_text ($section->{'data'});

		if (!@text) {
			print STDERR "Failed to decode a text section in $file...\n";
		} else {
			my $ref = { 'file' => $file, 'etc' => \@text };
			push (@texts, $ref);
			$etc_file{\@text} = $data;
		}
	}

	push (@etc, @texts) if @texts;
	
	$last_file = $data;
}

# Check we have some ETC to work with
die "No valid data loaded (invalid ETC files?)" if !@etc;

my $entry_point;
if ($standalone) {
	# Pick Entry Point
	my $symbols 	= $last_file->{'symbols'};
	my $last_text	= $etc[@etc - 1]->{'etc'};
	my $jentry;
	foreach my $op (@$last_text) {
		if ($op->{'name'} eq '.JUMPENTRY') {
			$jentry = $op->{'arg'};
		}
	}

	die "No jump entry in the final text section: don't know which process to link"
		if !defined ($jentry);

	$entry_point = $symbols->{$jentry};

	die "Unable to find symbol definition for entry point $jentry"
		if !defined ($entry_point) || !exists ($entry_point->{'definition'});

	if ($verbose) {
		print 	"Target:\n",
			format_symbol_definition ($entry_point->{'definition'}, "  ");
	}
}

my @asm = $llvm->generate (@etc);

if ($entry_point) {
	push (@asm, $llvm->entry_point ($entry_point));
}

foreach my $line (@asm) {
	print $line, "\n";
}

# FIXME: do more stuff!

exit 0;

sub format_symbol_definition {
	my ($def, $prefix) 	= @_;
	my @lines		= split (/\r?\n/, $def);
	my $indent		= "";

	foreach my $line (@lines) {
		$line =~ s/PROC\s+(\S+)\s*\(/PROC \1 (/;
		$line =~ s/(--|,)(\S)/\1 \2/g;
		$line =~ s/(\S)--/\1 --/g;

		if ($line =~ /^(PROC|SEQ|PAR)/) {
			$line = $indent . $line;
			$indent = "$indent  ";
		} elsif ($line =~ /^:/) {
			$indent = "";
		} else {
			$line = $indent . $line;
		}
		
		$line = $prefix . $line . "\n";
	}

	return join ('', @lines);
}
