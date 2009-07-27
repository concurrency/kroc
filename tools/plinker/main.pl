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

# Options
my $bits	= 32;
my $output;
my %sections	= (
	'debug'		=> 1,
	'ffi'		=> 1,
	'symbols'	=> 1,
	'tlp'		=> 1
);
my $verbose;
my @files;

# Command Line Parsing
my @args	= @ARGV;
my $options 	= 1;
while (my $arg = shift @args) {
	if ($options && $arg eq '--') {
		$options 		= 0;
	} elsif ($options && $arg eq '-v') {
		$verbose		= 1;
		$etc->{'verbose'} 	= 1;
		$linker->{'verbose'} 	= 1;
		$tcoff->{'verbose'}	= 1;
		$tvm->{'verbose'} 	= 1;
	} elsif ($options && $arg eq '-o') {
		$output 		= shift @args;
	} elsif ($options && $arg eq '-s') {
		$bits			= 16;
	} elsif ($options && $arg =~ /^-[ed]$/) {
		my $section		= shift @args;
		$sections{$section}	= $arg eq '-e';
	} else {
		push (@files, $arg);
	}
}

if (!$output) {
	$output = $files[-1];
	$output =~ s/\.tce$/.tbc/i;
}

if (!$output || !@files) {
	print <<END;
Usage:
  plinker [OPTIONS] <file> [<file> ...]
    Link one or more ETC input files as a TEncode bytecode.

Options:
  -d <section>   Disable output of a section
  -e <section>   Enable output of a section
  -o <name>      Specific output file name
  -s             Small 16-bit output (default is 32-bit)
  -v             Verbose mode

Valid sections are:
  debug          Line numbering information   (default: enabled)
  ffi            Foreign Function Interface   (default: enabled)
  symbols        Symbol information           (default: enabled)
  tlp            Top Level Process descriptor (default: enabled)

Report bugs to <kroc-bugs\@kent.ac.uk>.
END
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

	# Separate objects and libraries
	if ($file =~ /\.tce$/i) {
		push (@$objects, @texts);
	} elsif (@texts) {
		if (@$objects) {
			push (@etc, $objects);
			$objects = [];
		}
		push (@etc, \@texts);
	}

	$last_file = $data;
}

push (@etc, $objects) if @$objects;
$objects = [];

# Check we have some ETC to work with
die "No valid data loaded (invalid ETC files?)" if !@etc;

# Pick Entry Point
my $symbols 	= $last_file->{'symbols'};
my $jentry;
my $last_texts	= $etc[@etc - 1];
my $last_text	= $last_texts->[@$last_texts - 1]->{'etc'};
foreach my $op (@$last_text) {
	if (!$jentry && ($op->{'name'} eq '.JUMPENTRY')) {
		$jentry = $op->{'arg'};
	}
}

die "No jump entry in the final text section: don't know which process to link"
	if !defined ($jentry);

my $entry_point = $symbols->{$jentry};

die "Unable to find symbol definition for entry point $jentry"
	if !defined ($entry_point) || !exists ($entry_point->{'definition'});

if ($verbose) {
	print 	"Target:\n",
		format_symbol_definition ($entry_point->{'definition'}, "  ");
}

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
	'bc B'	=> join ('', @$bytecode)
);
$tbc->add ('tlpL', $tlp) if $sections{'tlp'};
$tbc->add ('ffiL', $ffi) if $sections{'ffi'};
$tbc->add ('stbL', $stb) if $sections{'symbols'};
$tbc->add ('dbgL', $dbg) if $sections{'debug'};

my $tenc		= new Transterpreter::TEncode (
	'tbcL'	=> $tbc
);

# Top-Level-Process
$tlp->add ('fmtS', build_format_string ($entry_point->{'definition'}));
$tlp->add ('symS', $entry_point->{'string'});

# FFI
my %ffi_libs;
my @ffi_symbols;
foreach my $label (@labels) {
	if (exists ($label->{'ffi_index'})) {
		$ffi_symbols[$label->{'ffi_index'}] = '_' . $label->{'ffi_symbol'};
	}
	if (exists ($label->{'source'})) {
		my $comments = $etc_file{$label->{'source'}}->{'COMMENT'};

		# Search comments
		foreach my $comment (@$comments) {
			my $data = $comment->{'data'};
			if ($data =~ /\(spragma\s+\(dynlib\s+(.*)\)\)/) {
				my $library = $1;
				$ffi_libs{$library}++;
			}
		}
	}
}

if (@ffi_symbols) {
	my $libs = new Transterpreter::TEncode ();
	my $syms = new Transterpreter::TEncode ();
	my $map	= new Transterpreter::TEncode ();

	$ffi->add ('libL' => $libs);
	$ffi->add ('symL' => $syms);
	$ffi->add ('mapL' => $map);

	foreach my $lib (sort (keys (%ffi_libs))) {
		$libs->add ('libS' => $lib);
	}
	foreach my $sym (@ffi_symbols) {
		$syms->add ('symS' => $sym);
	}
	foreach my $sym (@ffi_symbols) {
		$map->add ('idxI' => -1);
	}
}

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
	$lnd .= pack (($bits == 32 ? 'NNN' : 'nnn'), 
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
print $fh $tenc->encode ($bits == 32 ? 'TEnc' : 'tenc');
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
		$param =~ s/^\s*//s;
		$param =~ s/\s*$//s;

		if ($param =~ /^FIXED/) {
			if ($param =~ /\?/) {
				push (@fmt, 'S');
			} elsif ($param =~ /!/) {
				push (@fmt, 'C');
			} else {
				die;
			}
		} elsif ($param =~ m/^CHAN\s+OF\s+(\S+)\s+(\S+)/s) {
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

	push (@fmt, 'F') if $def =~ m/PROC.*\(.*\).*FORK/s;

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
