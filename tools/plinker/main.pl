#!/usr/bin/env perl
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

require 'ETC.pm';
require 'Linker.pm';
require 'TCOFF.pm';
require 'TEncode.pm';
require 'TVM.pm';

package main;

use strict;
use Data::Dumper;

# ETC Decoder
my $etc		= new Transputer::ETC ();

# Linker
my $linker	= new Transterpreter::Linker ();

# TCOFF Decoder
my $tcoff	= new Transputer::TCOFF ();

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
my %etc_file;
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
			my $ref = \@text;
			push (@etc, $ref);
			$etc_file{$ref} = $data;
		}
	}

	$last_file = $data;
}

# Check we have some ETC to work with
die "No valid data loaded (invalid ETC files?)" if !@etc;

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

my $entry_point = $symbols->{$jentry};

die "Unable to find symbol definition for entry point $jentry"
	if !defined ($entry_point) || !exists ($entry_point->{'definition'});

print 	"Target:\n",
	format_symbol_definition ($entry_point->{'definition'}, "  ");

# Link
my @labels = $linker->link ($entry_point->{'string'}, @etc);

# TEncode
my ($bytecode, $debug)	= $tvm->assemble (@labels);
my $tlp			= new Transterpreter::TEncode ();
my $ffi			= new Transterpreter::TEncode ();
my $stb			= new Transterpreter::TEncode ();
my $dbg			= new Transterpreter::TEncode ();
my $tbc			= new Transterpreter::TEncode (
	'endU'	=> $endian eq 'big' ? 1 : 0,
	'ws U'	=> $entry_point->{'ws'},
	'vs U'	=> $entry_point->{'vs'},
	'padB'	=> "\0\0\0\0",
	'ms U'	=> $entry_point->{'ms'},
	'bc B'	=> join ('', @$bytecode),
	'tlpL'	=> $tlp,
	'ffiL'	=> $ffi,
	'stbL'	=> $stb,
	'dbgL'	=> $dbg
);
my $tenc		= new Transterpreter::TEncode (
	'tbcL'	=> $tbc
);

# Top-Level-Process
$tlp->add ('fmtS', build_format_string ($entry_point->{'definition'}));
$tlp->add ('symS', $entry_point->{'string'});

# Symbol Table
foreach my $label (@labels) {
	next if !exists ($label->{'symbol'});
	die if !$label->{'source'};
	my $name	= $label->{'symbol'};
	my $symbols 	= $etc_file{$label->{'source'}}->{'symbols'};
	my $symbol	= $symbols->{$name};
	my $sym		= new Transterpreter::TEncode (
		'offU'	=> $label->{'pos'},
		'symS'	=> $name
	);

	if (exists ($symbol->{'definition'})) {
		$sym->add ('defS' => $symbol->{'definition'});
		$sym->add ('ws U' => $symbol->{'ws'});
		$sym->add ('vs U' => $symbol->{'vs'});
	}

	$stb->add ('symL' => $sym);
}

# Line numbering information
my $fns 	= new Transterpreter::TEncode ();
my %files;

foreach my $entry (@$debug) {
	$files{$entry->{'file'}}++;
}

my @files = sort { $files{$a} <=> $files{$b} } (keys (%files));

for (my $i = 0; $i < @files; ++$i) {
	my $file	= $files[$i];
	$files{$file}	= $i;
	$fns->add ('fn S' => $file );
}
$dbg->add ('fn L' => $fns);

my $lnd;
foreach my $entry (@$debug) {
	$lnd .= pack ('NNN', 
		$entry->{'pos'},
		$files{$entry->{'file'}},
		$entry->{'line'}
	);
}
$dbg->add ('lndB' => $lnd);

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

sub build_format_string {
	my ($def)	= @_;
	my ($params)	= ($def =~ m/PROC\s+\S+\(([^\)]+)\)/s);
	my @params	= split (/,/, $params);
	my @fmt;

	foreach my $param (@params) {
		if ($param =~ /^FIXED/) {
			if ($param =~ /\?/) {
				push (@fmt, 'S');
			} elsif ($param =~ /!/) {
				push (@fmt, 'C');
			} else {
				die;
			}
		} elsif ($param =~ m/^CHAN OF (\S+)\s+(\S+)/) {
			my ($type, $name)	= ($1, $2);
			my ($dir)		= ($name =~ m/([\?!])$/);
			if (!$dir) {
				($dir) = ($def =~ m/$name([\?!])/gs);
				$dir = '.' if !$dir;
			}
			push (@fmt, $dir);
		} elsif ($param =~ /^VAL/) {
			push (@fmt, 'V');
		} else {
			push (@fmt, '_');
		}
	}

	push (@fmt, 'F') if $def =~ /PROC.*\(.*\).*FORK/;

	return join ('', @fmt);
}

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
