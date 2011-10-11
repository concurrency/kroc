#!/usr/bin/perl

use strict;

my $firmware_tbc = 'tvm-nacl.tbc';

my ($fh, $buffer, $data);
open ($fh, $firmware_tbc) || die "Could not open $firmware_tbc: $!";
while (read ($fh, $buffer, 4096)) {
	$data .= $buffer;
}
close ($fh);

my @data = unpack('C*', $data);

printf ("static const uint8_t firmware_tbc[%d] = {\n", scalar(@data));
my $n = 0;
foreach my $byte (@data) {
	printf (' 0x%02x,', $byte);
	if ($n++ >= 8) {
		print "\n";
		$n = 0;
	}
}
print "};\n";

printf ("static const int firmware_tbc_len = %d;\n", scalar(@data));

