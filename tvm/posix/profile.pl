#!/usr/bin/perl

use strict;

my %count;
my %names;
my %sec;

my $on = 0;
my $total = 0;

while (my $line = <STDIN>) {
	if ($line =~ /^--.*user/i) {
		$on = 1;
	} elsif ($line =~ /^--/) {
		$on = 0;
	}
	if ($on && $line =~ /^(pri|sec)\s+(0x[a-f0-9]+)\s+([A-Z_]+)\s+=\s+(\d+)/) {
		my ($type, $opcode, $name, $count) = ($1, $2, $3, $4);

		$names{$type . $opcode} = $name;
		$count{$type . $opcode} += $count;
		$total += $count;
	}
}

my @keys = sort { return $count{$b} <=> $count{$a}; } (keys (%names));

foreach my $k (@keys) {
	my ($type, $opcode) = ($k =~ m/(pri|sec)(.*)/);
	my $name = $names{$k};
	my $count = $count{$k};
	my $p = $count / ($total / 100.0);
	my $p_i = int ($p);
	my $p_f = int ((($p - $p_i) * 100.0) + 0.5);

	printf (
		"$type  $opcode  %-12s = %2d.%02d%% (%d)\n", 
		$name, 
		$p_i, $p_f,
		$count
	);
}

