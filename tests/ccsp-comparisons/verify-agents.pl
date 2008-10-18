#!/usr/bin/perl

use strict;

if (!@ARGV) {
	print STDERR "perl verify-agents.pl <correct log> <test log> [<test log> ...]\n";
	exit 1;
}

my @files = @ARGV;
my %files;

foreach my $file (@files) {
	my @step;
	my $fh;
	open ($fh, $file) || die "Can't open $file: $!";
	while (my $line = <$fh>) {
		next if $line !~ /^(\d+) (\d+) at (\d+):(\S+),(\S+)/;
		my ($cycle, $id, $loc, $x, $y) = ($1, $2, $3, $4, $5);
		$step[$cycle] = {} if !$step[$cycle];

		$step[$cycle]->{$loc} = {} if !$step[$cycle]->{$loc};
		$step[$cycle]->{$loc}->{$id} = { 'x' => $x, 'y' => $y };
	}
	close ($fh);
	$files{$file} = \@step;
}

my $master_file = shift (@files);
my $master = $files{$master_file};
foreach my $file (@files) {
	my $step = $files{$file};
	for (my $i = 0; $i < @$master; ++$i) {
		die "Inconsistent $master_file versus $file [$i]"
			if !verify ($master->[$i], $step->[$i]);
	}
}

print "OK!\n";

exit 0;

sub verify ($$) {
	my ($master, $step)	= @_;
	my @master		= sort (keys (%$master));
	my @step		= sort (keys (%$step));

	if (scalar (@master) != scalar (@step)) {
		print STDERR "Inconsistent location counts\n";
		return 0;
	}
	for (my $i = 0; $i < @master; ++$i) {
		my $mlk = $master[$i];
		my $slk = $step[$i];
		if ($mlk != $slk) {
			print STDERR "Inconsistent locations ($mlk, $slk)\n";
			return 0;
		}
		my $ml = $master->{$mlk};
		my $sl = $step->{$slk};
		my @ml = sort (keys (%$ml));
		my @sl = sort (keys (%$sl));

		if (scalar (@ml) != scalar (@sl)) {
			print STDERR "Inconsistent agent counts ($mlk)\n";
			return 0;
		}

		for (my $i = 0; $i < @ml; ++$i) {
			my $m = $ml->{$ml[$i]};
			my $s = $sl->{$sl[$i]};
			if ($m->{'x'} != $s->{'x'} || $m->{'y'} != $s->{'y'}) {
				print STDERR "Inconsistent elements\n";
				print STDERR "\t", $m->{'x'}, ",", $m->{'y'}, "\n";
				print STDERR "\t", $s->{'x'}, ",", $s->{'y'}, "\n";
				return 0;
			}
		}
	}
	return 1;
}
