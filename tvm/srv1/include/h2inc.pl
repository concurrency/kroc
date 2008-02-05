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
		my $orig_line	= $lines[$ln];
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
			# Detect lines split with '\'
			if ($line =~ /^(.*)\\$/) {
				my $prepend = $1;
				$lines[$ln+1] = "$prepend " . $lines[$ln+1];
				next;
			}

			my $xline = $line;
			
			$xline =~ s/\(\s+/\(/g;
			$xline =~ s/\s+\)/\)/g;

			if ($line =~ m/^\/\*/) {
				if ($line =~ m/\*\//) {
					$line =~ s/\/\*/--/;
					$line =~ s/\*\///;
				} else {
					$line = "";
					$comment = 1;
				}
			} elsif ($xline =~ m/^#define\s+(.+?)\s+(.*)/) {
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

				if ($def =~ /^(\S+)\s*\(([a-z0-9,\s]+)\)$/i) {
					my ($name, $var) = ($1, $2);
					
					$var =~ s/\s+//g;
					my @var = split (/,/, $var);
					$val =~ s/_/./g;
					$val =~ s/\&/\/\\/g;
					$val =~ s/\|/\\\//g;
					$val =~ s/\^/\>\</g;
					$val =~ s/0x([0-9A-F]+)/(#\1)/g;
					
					$name = verify_symbol ($state, 'function', $name);
					
					$line  = "#IF NOT DEFINED ($name.DEF)\n";
					$line .= "#DEFINE $name.DEF\n";
					$line .= "INT FUNCTION $name (VAL INT ";
					$line .= join(', ', @var);
					$line .= ") IS ($val):";
					$line .= " -- $comment" if $comment;
					$line .= "\n";
					$line .= "#ENDIF";
				} elsif ($val =~ /^(0x[0-9a-f]+|\d+)$/i) {
					my $name = $def;
					my $type = "INT";
					
					if ($val =~ /^0x/) {
						my $n = hex($val);
						$val =~ s/^0x//;
						$val =~ tr/a-f/A-F/;
						if (($n > 0xff000000) && (($n & 3) == 0)) {
							$name .= '.ADDR';
						} elsif (length($val) == 4) {
							$type = "INT16";
						}
						$val = "#$val";
					}
					
					$name =~ s/_/./g;
					$name = verify_symbol ($state, 'constant', $name);
					
					$line = "VAL $type $name IS $val:";
					$line .= " -- $comment" if $comment;
				} elsif ($val =~ /^(\(.*\)|[a-z0-9_]+(\([^\)]+\))?|)$/i) {
					my $name = $def;

					$val =~ s/_/./g;
					$name =~ s/_/./g;
					$name = verify_symbol ($state, 'expression', $name);
					
					my $symbols = $state->{'symbols'};
					my @syms = ($val =~ m/([a-z0-9\.]+)/gi);
					my $ok = 1;
					foreach my $sym (@syms) {
						next if $sym =~ m/^0x[a-f0-9]+$/i;
						next if $sym =~ m/^[0-9]+$/i;
						$ok = $ok && exists($symbols->{$sym});
					}
					
					$line = "";
					$line .= "-- " if !$ok;
					$line .= "VAL _ $name IS $val:";
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
		}
		$addr->{'length'} = ($addr->{'top'} - $address) + 4;
	}

	# Inject addressing information
	foreach my $line (@lines) {
		next if $line !~ m/^VAL INT (.+?)\.(.+\.)?ADDR IS #(.+):/;
		my ($base, $sub, undef) = ($1, $2, $3);
		my $addr = $addrs{$base};

		$sub =~ s/\.$//;

		if (!$addr->{'written'}) {
			my $address = $addr->{'address'};
			my $length_16b = $addr->{'length'} / 2;
			my $length_32b = $length_16b / 2;
			my $old_line = $line;
			$line = "VAL INT $base.ADDR IS #" . 
				sprintf('%08X', $address) .
				":\n" .
				"VAL INT $base.LEN IS $length_16b:\n" .
				"VAL INT $base.LEN.32 IS $length_32b:";
			$line .= "\n$old_line" if $sub;
			$addr->{'written'} = 1;
		}
		my $offset_16b = $addr->{'offsets'}->{$sub} / 2;
		my $offset_32b = $offset_16b / 2;
		if ($sub) {
			$line .= "\nVAL INT $base.$sub IS $offset_16b:";
			$line .= "\nVAL INT $base.$sub.32 IS $offset_32b:";
		} else {
			$line .= "\nVAL INT $base IS $offset_16b:";
			$line .= "\nVAL INT $base.32 IS $offset_32b:";
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
		next if $line !~ m/^VAL \S{0,5} (.+) IS/;
		my $value = $1;
		my $width = length($value);
		$max_width = $width if $width > $max_width;
	}
	
	# Formatting and VAL type cleaning pass
	my $fmt = sprintf('%%-%ds', $max_width);
	foreach my $line (@lines) {
		next if $line !~ m/^VAL (\S{0,5})? (.+) IS(.*)/;
		my ($type, $value, $rest) = ($1, $2, $3);
		$type = "" if $type eq '_';
		$line = sprintf("VAL %-5s $fmt IS %s", $type, $value, $rest);
	}

	# Output pass
	my $out_file_n = $out_file;
	$out_file_n =~ s/(.*)\///;
	$out_file_n =~ tr/a-z/A-Z/;
	$out_file_n =~ s/[^A-Z0-9.]/./g;

	open ($fh, ">", $out_file) || die $!;
	print $fh "#IF NOT DEFINED ($out_file_n)\n";
	print $fh "#DEFINE $out_file_n\n";
	foreach my $line (@lines) {
		print $fh $line, "\n";
	}
	print $fh "\n";
	print $fh "#ENDIF -- !$out_file_n\n";
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
