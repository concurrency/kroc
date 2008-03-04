#!/usr/bin/perl

use strict;

my %configs;
my %defaults;
my %names;

my $current;
while (my $line = <STDIN>) {
	if ($line =~ m/.*ov9655_(.*)\[\]/) {
		my $name = $1;
		$current = [];
		$configs{$name} = $current;
	} elsif ($line =~ m/.*0x([0-9a-f]{2}).*0x([0-9a-f]{2})(.*)/i) {
		my ($reg, $val, $rest) = ($1, $2, $3);
		my $name;

		if ($rest =~ m/\/\*\s*([A-Z]\S+)(.*)/) {
			my $rest;
			
			($name, $rest) = ($1, $2);

			if ($rest =~ m/\s*-\s*([0-9a-f]{2})/i) {
				$defaults{$name} = hex ($1);
			}
		}

		$reg = hex ($reg);
		$val = hex ($val);

		if (!$name) {
			$name = sprintf('REG%02X', $reg);
		}

		$names{$name} = $reg;
		push (@{$current}, { 'reg' => $name, 'val' => $val });
	} elsif ($line =~ /};/) {
		$current = undef;
	}
}

print "-- OV9655 camera definitions\n\n";

print "--{{{  Registers Addreses\n";
my @regs = sort { $names{$a} <=> $names{$b} } (keys (%names));
foreach my $reg (@regs) {
	printf ("VAL BYTE %-24s IS #%02X:\n", 'OV9655.' . $reg, $names{$reg});
}
print "--}}}\n\n";

print "--{{{  Register Defaults\n";
foreach my $reg (@regs) {
	if (exists ($defaults{$reg})) {
		printf ("VAL BYTE %-24s IS #%02X:\n", 
			'OV9655.' . $reg . '.DEFAULT', 
			$defaults{$reg}
		);
	}
}
print "--}}}\n\n";

print "--{{{  Configurations\n";
foreach my $config_name (sort (keys (%configs))) {
	my $config = $configs{$config_name};
	my @config = @{$config};

	$config_name =~ tr/a-z/A-Z/;
	$config_name = 'OV9655.' . $config_name;

	printf ("VAL [][2]BYTE %-12s IS [", $config_name);
	for (my $i; $i < @config; ++$i) {
		my $set = $config[$i];
		my $reg = $set->{'reg'};
		my $val = $set->{'val'};
		if (exists ($defaults{$reg}) && ($defaults{$reg} == $val)) {
			$val = 'OV9655.' . $reg . '.DEFAULT';
		} else {
			$val = sprintf ('#%02X', $val);
		}
		printf ("[ %-14s, %-20s ]", 'OV9655.' . $reg, $val);
		if ($i < (@config - 1)) {
			printf (",\n%31s", "");
		}
	}
	print "]:\n\n";
}
print "--}}}\n\n";

