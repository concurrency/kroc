#!/usr/bin/perl

use POSIX ":sys_wait_h";
use strict;

my ($timeout, @cmd) = @ARGV;
my $pid;

die "perl timeout.pl <timeout> <cmd...>" if !int($timeout);

if ($pid = fork()) {
	my $end = time () + $timeout;
	my $res = 0;
	my $sleep = 1;
	do {
		sleep ($sleep);
		$sleep = $sleep < 10 ? $sleep + 1 : 10;
		$res = waitpid ($pid, WNOHANG);
	} while (!$res && ($end > time ()));
	
	if (!$res) {
		print STDERR "Killed by timeout\n";
		kill (1, $pid);
		exit (1);
	} else {
		exit (0);
	}
} else {
	select STDOUT; $|++;
	exec ('bash', '-c', join (' ', @cmd));
}
