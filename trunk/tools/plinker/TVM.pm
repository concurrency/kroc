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

package Transterpreter::VM;

use strict;

sub new ($$) {
	my ($class) = @_;
	my $self = bless {}, $class;
	return $self;
}

sub assemble ($@) {
	my ($self, @labels) = @_;
	my $verbose = $self->{'verbose'};
	my @bytecode;
	my %debug;
	my $pos = 0;

	foreach my $label (@labels) {
		printf ("0x%05x %-6s  % 3d  % 3d  %s\n",
			$label->{'pos'},
			$label->{'name'},
			$pos,
			$label->{'length'},
			$label->{'symbol'}
		) if $verbose;

		while ($pos < $label->{'pos'}) {
			push (@bytecode, "\0");
			$pos++;
		}

		if ($pos != $label->{'pos'}) {
			die sprintf (
				"Label positioning error %s at %d (pos:%d)",
				$label->{'name'}, $label->{'pos'}, $pos
			);
		}
		
		foreach my $op (@{$label->{'inst'}}) {
			my $name = $op->{'name'};
			if ($op->{'bytes'}) {
				my $bytes = $op->{'bytes'};
				push (@bytecode, @$bytes);
				$pos += scalar (@$bytes);
				
				if ($verbose) {
					for (my $i = 0; $i < 23; ++$i) { print " "; }
					printf ('%-16s', $name);
					for (my $i = 0; $i < @$bytes; ++$i) {
						printf ('%02x ', unpack ('C', $bytes->[$i]));
					}
					print "\n";
				}
			} elsif ($name =~ /^\.FILENAME$/) {
				$debug{$pos} = {} if !$debug{$pos};
				$debug{$pos}->{'file'} = $op->{'arg'};
			} elsif ($name =~ /^\.LINE$/) {
				$debug{$pos} = {} if !$debug{$pos};
				$debug{$pos}->{'line'} = $op->{'arg'};
			} elsif ($name =~ /^\.DATABYTES/) {
				my @bytes = split (//, $op->{'arg'});
				push (@bytecode, @bytes);
				$pos += scalar (@bytes);
				
				if ($verbose) {
					for (my $i = 0; $i < 23; ++$i) { print " "; }
					for (my $i = 0; $i < @bytes; ++$i) {
						printf ('%02x ', unpack ('C', $bytes[$i]));
					}
					print "\n";
				}
			}
		}
	}

	# Debug info: fill in missing file entries and delete duplicates
	my $last;
	foreach my $key (sort { $a <=> $b } (keys (%debug))) {
		my $entry = $debug{$key};
		
		$entry->{'pos'} = $key;
		
		if (!exists ($entry->{'file'})) {
			next if !$last;
			$entry->{'file'} = $last->{'file'};
		}

		if ($last->{'file'} eq $entry->{'file'} 
				&& $last->{'line'} == $entry->{'line'}) {
			delete ($debug{$key});
		} else {
			$last = $entry;
		}
	}

	# Debug info: build return array
	my @debug;
	foreach my $key (sort { $a <=> $b } (keys (%debug))) {
		push (@debug, $debug{$key});
	}

	return (\@bytecode, \@debug);
}

1;
