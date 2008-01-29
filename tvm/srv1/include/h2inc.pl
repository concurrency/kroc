#!/usr/bin/perl

my %KEYWORDS = ( 'REC' => 0, 'SIZE' => 0, 'STOP' => 0 );

use strict;

foreach my $file (@ARGV) {
	next if $file !~ /\.h$/;

	my ($fh, $line, @lines);
	my $state = { 'file' => $file, 'symbols' => {} };
	my $out_file = ($file =~ m/(.*)h/)[0] . 'inc';

	# File input pass
	open ($fh, "<", $file);
	if (!$fh) {
		print ("Unable to open \"$file\"")
	}
	while ($line = <$fh>) {
		$line =~ s/\r?\n//s;
		$line =~ s/^\s+//;
		$line =~ s/\s+$//;
		push (@lines, $line);
	}
	close ($fh);

	# Initial transformation pass
	my $comment = 0;
	for(my $ln = 0; $ln < @lines; ++$ln) {
		my $orig_line	= @lines[$ln];
		my $line	= $orig_line;

		$state->{'ln'}	= $ln;
		$lines[$ln] 	= -1;

		if ($comment) {
			if ($line =~ m/\*\//) {
				$line = "";
				$comment = 0;
			} else {
				$line =~ s/\*{2,}//;
				$line = '--' . $line;
			}
		} else {
			if ($line =~ m/^\/\*/) {
				if ($line =~ m/\*\//) {
					$line =~ s/\/\*/--/;
					$line =~ s/\*\///;
				} else {
					$line = "";
					$comment = 1;
				}
			} elsif ($line =~ m/^#define\s+(.+?)\s+(.*)/) {
				my ($def, $rest) = ($1, $2);
				my ($val, $comment);
				
				$def =~ s/_/./g;
				
				if ($rest =~ /\/\*/) {
					($val, $comment) = ($rest =~ m/(.*?)\s*\/\*\s*(.+?)\s*\*\//);
				} elsif ($rest =~ /\/\//) {
					($val, $comment) = ($rest =~ m/(.*?)\s*\/\/\s*(.+?)\s*$/);
				} else {
					($val, $comment) = ($rest, "");
				}
				
				if ($def =~ /^(\S+)\(([a-z0-9]+)\)$/i) {
					my ($name, $var) = ($1, $2);
					$val =~ s/_/./g;
					$val =~ s/\&/\/\\/g;
					$val =~ s/\|/\\\//g;
					$val =~ s/\^/\>\</g;
					$val =~ s/0x([0-9A-F]+)/(#\1)/g;
					$name = verify_symbol ($state, 'function', $name);
					$line  = "#IF NOT DEFINED ($name.DEF)\n";
					$line .= "#DEFINE $name.DEF\n";
					$line .= "INT FUNCTION $name (VAL INT $var) IS ($val):";
					$line .= " -- $comment" if $comment;
					$line .= "\n";
					$line .= "#ENDIF";
				} elsif ($val =~ /^(0x[0-9a-f]+|\d+)$/i) {
					my $name = $def;
					if ($val =~ /^0x/) {
						$val =~ s/^0x//;
						$val =~ tr/a-f/A-F/;
						if (hex($val) > 0xff000000) {
							$name .= '.ADDR';
						}
						$val = "#$val";
					}
					$name =~ s/_/./g;
					$name = verify_symbol ($state, 'constant', $name);
					$line = "VAL INT $name IS $val:";
					$line .= " -- $comment" if $comment;
				} elsif ($val =~ /^(\(.*\)|[a-z0-9_]+)$/i) {
					my $name = $def;
					$val =~ s/_/./g;
					$name =~ s/_/./g;
					$name = verify_symbol ($state, 'expression', $name);
					$line = "VAL INT $name IS $val:";
					$line .= " -- $comment" if $comment;
				}
			} elsif ($line =~ m/^#include\s+["<](.+?)[">]/) {
				my $header = $1;
				$header =~ s/\.h$/\.inc/;
				if ($header =~ /\.inc$/) {
					$line = "#INCLUDE \"$header\"";
				}
			} elsif ($line =~ m/\/\/\s*(.+)/) {
				$line = "-- $1";
			}
		}

		$lines[$ln] = $line if $line ne $orig_line;
	}
	
	@lines = expand_and_clean (@lines);

	# Read address range information
	my %addrs;

	foreach my $line (@lines) {
		next if $line !~ m/^VAL INT (.+?)\.(.+\.)?ADDR IS #(.+):/;
		my ($base, $sub, $address) = ($1, $2, $3);
		$sub =~ s/\.$//;
		$address = hex($address);
		if (exists($addrs{$base})) {
			my $addr = $addrs{$base};
			my $offsets = $addr->{'offsets'};
			$offsets->{$sub} = $address;
			if ($address < $addr->{'address'}) {
				$addr->{'address'} = $address;
			}
			if ($address > $addr->{'top'}) {
				$addr->{'top'} = $address;
			}
		} else {
			$addrs{$base} = {
				'base' 		=> $base,
				'offsets' 	=> { $sub => $address },
				'address' 	=> $address,
				'top'		=> $address,
				'length' 	=> 0
			};
		}
	}

	# Resolve address ranges
	foreach my $base (keys(%addrs)) {
		my $addr = $addrs{$base};
		my $address = $addr->{'address'};
		foreach my $sub (keys(%{$addr->{'offsets'}})) {
			$addr->{'offsets'}->{$sub} -= $address;
			$addr->{'offsets'}->{$sub} /= 4;
		}
		$addr->{'length'} = (($addr->{'top'} - $address) / 4) + 1;
	}

	# Inject addressing information
	foreach my $line (@lines) {
		next if $line !~ m/^VAL INT (.+?)\.(.+\.)?ADDR IS #(.+):/;
		my ($base, $sub, undef) = ($1, $2, $3);
		my $addr = $addrs{$base};

		$sub =~ s/\.$//;

		if (!$addr->{'written'}) {
			my $address = $addr->{'address'};
			my $length = $addr->{'length'};
			my $old_line = $line;
			$line = "VAL INT $base.ADDR IS #" . 
				sprintf('%08X', $address) .
				":\n" .
				"VAL INT $base.LEN IS $length:";
			$line .= "\n$old_line" if $sub;
			$addr->{'written'} = 1;
		}
		my $offset = $addr->{'offsets'}->{$sub};
		if ($sub) {
			$line .= "\nVAL INT $base.$sub IS $offset:";
		} else {
			$line .= "\nVAL INT $base IS $offset:";
		}
	}

	# Inject space before comment lines
	my $prev = undef;
	foreach my $line (@lines) {
		if ($prev && $prev !~ /^--/ && $line =~ /^--/) {
			$line = "\n" . $line;
		}
		$prev = $line if $line != -1;
	}

	@lines = expand_and_clean (@lines);

	# Width finding pass
	my $max_width = 0;
	foreach my $line (@lines) {
		next if $line !~ m/^VAL INT (.+) IS/;
		my $value = $1;
		my $width = length($value);
		$max_width = $width if $width > $max_width;
	}
	
	# Formatting pass
	my $fmt = sprintf('%%-%ds', $max_width);
	foreach my $line (@lines) {
		next if $line !~ m/^VAL INT (.+) IS(.*)/;
		my ($value, $rest) = ($1, $2);
		$line = "VAL INT " .
			sprintf($fmt, $value) .
			" IS$rest";
	}

	# Output pass
	open ($fh, ">", $out_file) || die $!;
	foreach my $line (@lines) {
		print $fh $line, "\n";
	}
	close ($fh);
}

exit 0;

sub verify_symbol($$$) {
	my ($state, $type, $name) = @_;
	my $symbols = $state->{'symbols'};
	if (exists ($KEYWORDS{$name})) {
		my $old_name = $name;
		$name = "X.$name";
		printf (
			"%s:%d\t'%s' renamed to '%s' (keyword collision)\n",
			$state->{'file'}, $state->{'ln'},
			$old_name, $name
		);
	}
	if (exists ($symbols->{$name})) {
		printf (
			"%s:%d\twarning duplicate definition '%s' (%s)\n",
			$state->{'file'}, $state->{'ln'},
			$name, $type
		);
	} else {
		$symbols->{$name} = $type;
	}
	return $name;
}

sub expand_and_clean(@) {
	my @lines = @_;
	my @new_lines;
	foreach my $line (@lines) {
		if ($line =~ m/\n/s) {
			push (@new_lines, split(/\n/, $line));
		} else {
			push (@new_lines, $line) if $line != -1;
		}
	}
	return @new_lines;
}
