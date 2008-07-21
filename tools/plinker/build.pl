#!/usr/bin/perl

require 'Instructions.pm';
require 'TCOFF.pm';

package main;

use Data::Dumper;
use strict;

my $in = 'main.pl';
my $out = 'plinker.pl';
my %included;

write_file ('store.pl', compile_store ());
write_file ($out, include_file ($in));

exit 0;

sub write_file ($$) {
	my ($fn, $data) = @_;
	my $fh;
	open ($fh, '>', $fn) || die $!;
	print $fh $data;
	close ($fh);
}

sub compile_store {
	my $store = {};
	Transputer::Instructions::compile (
		$store, "../occ21/include/instruct.h"
	);
	Transputer::TCOFF::compile (
		$store, "../occ21/include/tcoff.h"
	);
	Transterpreter::Instructions::compile (
		$store, "../../runtime/libtvm/"
	);
	return Data::Dumper->new ([$store], ['STORE'])->Dump;
}

sub include_file ($) {
	my $file = shift;
	my $data = "";

	return $data if exists $included{$file};

	$included{$file}++;

	my $fh;
	open ($fh, $file) || die $!;
	while (my $line = <$fh>) {
		if ($line =~ m/^\s*require '(.*?)'/) {
			my $required = $1;
			$data .= include_file ($required);
		} elsif ($line =~ /^\s*1\s*;\s*/) {
			# ignore
		} else {
			$data .= $line;
		}
	}
	close ($fh);

	return $data;
}
