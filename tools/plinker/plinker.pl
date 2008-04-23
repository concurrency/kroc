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
require 'Linker.pm';
require 'TCOFF.pm';
require 'TEncode.pm';
require 'TVM.pm';

# Constants
my $instruct_h	= "/Users/cgr/src/kroc-svn/tools/occ21/include/instruct.h";
my $tcoff_h	= "/Users/cgr/src/kroc-svn/tools/occ21/include/tcoff.h";

# ETC Decoder
my $etc		= new Transputer::ETC ($instruct_h);

# Linker
my $linker	= new Transterpreter::Linker ($instruct_h);

# TCOFF Decoder
my $tcoff	= new Transputer::TCOFF ($tcoff_h);

# TVM helper
my $tvm		= new Transterpreter::VM ();

# Command Line Parsing
my ($first, @files) = @ARGV;
my $output;
if ($first eq '-o') {
	$output = shift @files;
} else {
	unshift (@files, $first) if $first ne '--';
	$output = $files[-1];
	$output =~ s/\.tce$/.tbc/i;
}

if (!$output || !@files) {
	print "plinker.pl [-o <name>] <file> [<file>]\n";
	exit 1;
}

# Load ETC
my $last_file;
my $endian;
my @etc;
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
	foreach my $section (@{$data->{'LOAD_TEXT'}}) {
		my @text = $etc->decode_load_text ($section->{'data'});

		if (!@text) {
			print STDERR "Failed to decode a text section in $file...\n";
		} else {
			push (@etc, \@text);
		}
	}

	$last_file = $data;
}

# Pick Entry Point
my $symbols 	= $last_file->{'symbols'};
my $jentry;
foreach my $op (@{$etc[@etc - 1]}) {
	if ($op->{'name'} eq '.JUMPENTRY') {
		$jentry = $op->{'arg'};
	}
}

die "No jump entry in the final text section: don't know which process to link"
	if !defined ($jentry);

my $entry_point;
foreach my $sym_id (keys (%$symbols)) {
	my $sym = $symbols->{$sym_id};
	if (exists ($sym->{'definition'}) && ($sym->{'string'} eq $jentry)) {
		$entry_point = $sym;
	}
}

die "Unable to find symbol definition for entry point $jentry"
	if !defined ($entry_point);

print 	"Target:\n",
	format_symbol_definition ($entry_point->{'definition'}, "  ");

# Link
my @labels = $linker->link ($entry_point->{'string'}, @etc);

# TEncode
my ($bytecode, $debug)	= $tvm->assemble (@labels);
my $tlp			= new Transterpreter::TEncode ();
my $ffi			= new Transterpreter::TEncode ();
my $lni			= new Transterpreter::TEncode ();
my $tbc			= new Transterpreter::TEncode (
	'endU'	=> $endian eq 'big' ? 1 : 0,
	'ws U'	=> $entry_point->{'ws'},
	'vs U'	=> $entry_point->{'vs'},
	'ms U'	=> $entry_point->{'ms'},
	'bc B'	=> join ('', @$bytecode),
	'tlpL'	=> $tlp,
	'ffiL'	=> $ffi,
	'lniL'	=> $lni
);
my $tenc		= new Transterpreter::TEncode (
	'tbcL'	=> $tbc
);

# Top-Level-Process

$tlp->add ('fmtS', '?!!'); # FIXME
$tlp->add ('tlpS', $entry_point->{'definition'});

# Line numbering information
my $lni_fn 	= new Transterpreter::TEncode ();
my %files;

foreach my $entry (@$debug) {
	$files{$entry->{'file'}}++;
}

my @files = sort { $files{$a} <=> $files{$b} } (keys (%files));

for (my $i = 0; $i < @files; ++$i) {
	my $file	= $files[$i];
	$files{$file}	= $i;
	$lni_fn->add ('fn S' => $file );
}

$lni->add ('fn L' => $lni_fn);

foreach my $entry (@$debug) {
	$lni->add (
		'lndL'	=> new Transterpreter::TEncode (
			'offU'	=> $entry->{'pos'},
			'fniU'	=> $files{$entry->{'file'}},
			'ln U'	=> $entry->{'line'}
		)
	);
}

# Output 
my $fh;
open ($fh, ">$output") || die $!;
binmode ($fh);
print $fh $tenc->encode ('TEnc');
close ($fh);

# Debug labels
if (0) {
	foreach my $label (@labels) {
		print $label->{'name'}, " ";
		if ($label->{'data'}) {
			print "data";
		} elsif ($label->{'stub'}) {
			print "stub ", $label->{'stub'};
		} else {
			print $label->{'symbol'};
		}
		print "\n";
	}
}

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
