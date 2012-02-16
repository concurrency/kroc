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

require 'PLinker.pm';

package Transputer::TCOFF;

use strict;

sub new ($$) {
	my ($class, $tcoff_h) = @_;

	my $self	= {
		'tcoff'	=> PLinker::get ('tcoff')
	};
	
	$self = bless $self, $class;

	return $self;
}

sub read_int ($) {
	my $fh = shift;
	my $b;

	read ($fh, $b, 1) || return;
	$b = unpack ('C', $b);

	if ($b > 250 && $b <= 255) {
		my ($len, $n);

		if ($b == 255) {
			$len = 4;
		} else {
			$len = (1 << ($b - 251));
		}

		die "Invalid length $len" if $len > 4;

		read ($fh, $n, $len) || return;
		
		if ($len == 1) {
			$n = unpack ('C', $n);
		} elsif ($len == 2) {
			$n = unpack ('v', $n);
		} else {
			$n = unpack ('V', $n);
		}
		
		$n ^= $n if $b == 255;

		return $n;
	} else {
		return $b;
	}
}

sub decode_int {
	my ($self, $data);
	
	if (@_ > 1) {
		($self, $data) = @_;
	} else {
		($data) = @_;
	}

	my $b = unpack ('C', substr ($data, 0, 1));

	if ($b > 250 && $b <= 255) {
		my ($len, $n);

		if ($b == 255) {
			$len = 4;
		} else {
			$len = (1 << ($b - 251));
		}

		die "Invalid length $len" if $len > 4;

		my $n = substr ($data, 1, $len);
		
		if ($len == 1) {
			$n = unpack ('C', $n);
		} elsif ($len == 2) {
			$n = unpack ('v', $n);
		} else {
			$n = unpack ('V', $n);
		}
		
		$n ^= $n if $b == 255;

		return ($n, substr ($data, $len + 1));
	} else {
		return ($b, substr ($data, 1));
	}
}

sub read_file ($$) {
	my ($self, $filename)	= @_;
	my $tcoff		= $self->{'tcoff'};
	
	my $fh;
	my %sections;
	my @sections;

	if (!open ($fh, $filename)) {
		print STDERR "Unable to open $filename: $!\n";
		return;
	}

	binmode ($fh);
	while (!eof ($fh)) {
		my $r_tag	= read_int ($fh);
		my $r_len	= read_int ($fh);
		my $tag		= $tcoff->{$r_tag} || $r_tag;
		my $data;
		
		if ($r_len && !read ($fh, $data, $r_len)) {
			return;
		}
		
		my $section	= {
			'tag' 		=> $tag,
			'length'	=> $r_len,
			'data'		=> $data
		};

		$sections{$tag} = [] if !$sections{$tag};
		push (@{$sections{$tag}}, $section);
		push (@sections, $section);
	}
	close ($fh);

	$sections{'sections'} = \@sections;
	
	$self->decode_comments (\%sections);
	$self->decode_symbols (\%sections);

	return \%sections;
}

sub decode_symbols ($$) {
	my ($self, $file) 	= @_;
	my $tcoff		= $self->{'tcoff'};
	my $symbol_id		= 0;
	my %symbols;

	foreach my $section (@{$file->{'sections'}}) {
		my $tag		= $section->{'tag'};
		my $data	= $section->{'data'};

		if ($tag eq 'SECTION' || $tag eq 'SYMBOL' || $tag eq 'SPECIFIC_SYMBOL') {
			my ($type, $scope, $len, $str, $origin_id);

			($type, $data)	= decode_int ($data)
				if $tag eq 'SECTION';

			($scope, $data) = decode_int ($data);
			($len, $data)	= decode_int ($data);

			$str		= substr ($data, 0, $len);
			$data		= substr ($data, $len);
			
			($origin_id, $data) = decode_int ($data)
				if $tag eq 'SPECIFIC_SYMBOL';

			my $sym = {
				'type'		=> $type,
				'scope'		=> $scope, 
				'string'	=> $str,
				'origin_id'	=> $origin_id
			};
			$symbols{'@' . $symbol_id++}	= $sym;
			$symbols{$str}			= $sym;
		} elsif ($tag eq 'DESCRIPTOR') {
			my ($id, $data) = decode_int ($data);

			$id = '@' . $id;
			
			if (exists ($symbols{$id}) && $symbols{$id}->{'scope'} == ($tcoff->{'EXPORT_USAGE'} | $tcoff->{'ROUTINE_USAGE'})) {
				my $symbol	= $symbols{$id};
				my ($lang, $p_len, $ws, $vs, $ms);

				($lang, $data)	= decode_int ($data);
				($p_len, $data)	= decode_int ($data);
				($ws, $data)	= decode_int ($data);
				($vs, $data)	= decode_int ($data);
				($ms, $data)	= decode_int ($data);

				$symbol->{'definition'} = $data;
				$symbol->{'ws'}		= $ws;
				$symbol->{'vs'}		= $vs;
				$symbol->{'ms'}		= $ms;
			}
		}
	}

	$file->{'symbols'} = \%symbols;
}

sub decode_comments ($$) {
	my ($self, $file) 	= @_;
	my $tcoff		= $self->{'tcoff'};
	
	foreach my $comment (@{$file->{'COMMENT'}}) {
		my ($copy, $print, $s_len);
		my $data	= $comment->{'data'};

		($copy, $data)	= decode_int ($data);
		($print, $data)	= decode_int ($data);
		($s_len, $data) = decode_int ($data);

		if ($print && $data =~ /^(\.[A-Z0-9\.]+)\s*(.+)$/) {
			my ($cmd, $data) = ($1, $2);
			$file->{$cmd} = [] if !$file->{$cmd};
			push (@{$file->{$cmd}}, $data);
		}
	}
}

sub compile ($$) {
	my ($store, $fn) = @_;
	my $tcoff = {};
	my $fh;

	open ($fh, $fn) || die $!;
	while (my $line = <$fh>) {
		if ($line =~ /^#define (.+?)_TAG\s+(\d+)/) {
			my ($name, $value) = ($1, $2);
			
			$tcoff->{$name}	 = $value;
			$tcoff->{$value} = $name if !exists ($tcoff->{$value});
		} elsif ($line =~ /^#define (.+?)\s+(\d+|0x[0-9a-f]+)\s*$/) {
			my ($name, $value) = ($1, $2);
			$value = hex ($value) if $value =~ /^0x/;

			$tcoff->{$name}	 = $value;
			$tcoff->{$value} = $name if !exists ($tcoff->{$value});
		}
	}
	close ($fh);

	$store->{'tcoff'} = $tcoff;
}

1;
