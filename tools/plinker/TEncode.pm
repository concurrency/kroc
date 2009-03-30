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

package Transterpreter::TEncode;

use strict;

sub new ($@) {
	my ($class, @elements) = @_;

	my $self = bless {
		'elements' => []
	}, $class;

	for (my $i = 0; $i < @elements; $i += 2) {
		$self->add ($elements[$i + 0], $elements[$i + 1]);
	}

	return $self;
}

sub add ($$$) {
	my ($self, $id, $data) = @_;

	die "Invalid element identifier: $id"
		if $id !~ /^.{3}[BILSU]$/;
	
	die "Data for '$id' must be a TEncode object"
		if $id =~ /L$/ && (!ref ($data));

	push (@{$self->{'elements'}}, {
		'id'	=> $id,
		'data'	=> $data
	});
}

sub _encode ($$) {
	my ($self, $ib)	= @_;
	my $ifmt	= $ib == 2 ? 'n' : 'N';
	my $elements	= $self->{'elements'};
	my $bytes;

	foreach my $element (@$elements) {
		my $id		= $element->{'id'};
		my $data 	= $element->{'data'};
		my $length;
		
		$element->{'pad'}	= 0;
		
		if ($id =~ /L$/) {
			$element->{'bytes'} 	= $data->_encode ($ib);
			$length			= length ($element->{'bytes'});
		} elsif ($id =~ /[IU]$/) {
			# Unsign data
			$data += 2 ** ((8 * $ib) - 1) 
				if $id =~ /I$/ && $data < 0;
			
			my $coded	= pack ($ifmt, $data);
			my $decoded	= unpack ($ifmt, $coded);
			
			die "In consistent data coding for $id = $data"
				if $decoded != $data;

			$element->{'bytes'}	= '';
			$length			= $data;
		} else {
			$element->{'bytes'} 	= $data;
			$element->{'bytes'}	.= "\0" if $id =~ /S$/;
			$length			= length ($element->{'bytes'});
			$element->{'pad'}	= $ib - ($length & ($ib - 1))
				if ($length & ($ib - 1));
		}

		$bytes .= $id;
		$bytes .= pack ($ifmt, $length);
		$bytes .= $element->{'bytes'};

		for (my $i = 0; $i < $element->{'pad'}; ++$i) {
			$bytes .= "\0";
		}
	}

	$self->{'bytes'} = $bytes;

	return $bytes;
}

sub encode ($$) {
	my ($self, $header) = @_;

	die "Invalid TEncode header $header"
		if $header !~ /^(TE|te)nc$/;
	
	my $ib 		= $header eq 'TEnc' ? 4 : 2;
	my $bytes 	= $self->_encode ($ib);
	my $length	= length ($bytes);

	return $header . pack ($ib == 4 ? 'N' : 'n', $length) . $bytes;
}

1;
