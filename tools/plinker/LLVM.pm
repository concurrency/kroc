#
#  Perl code for generating LLVM assembly from ETC assembly
#  Copyright (C) 2009 Carl Ritson <cgr@kent.ac.uk>
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

package Transputer::LLVM;

use strict;
use vars qw($GRAPH);
use Data::Dumper;

$GRAPH = {
	'J' 	=> { 'in' => 3, 'out' => 0 },
	'LDLP'	=> { 'in' => 0, 'out' => 1 },
	'LDNL'	=> { 'in' => 1, 'out' => 1 },
	'LDC'	=> { 'in' => 0, 'out' => 1 },
	'LDNLP'	=> { 'in' => 1, 'out' => 1 },
	'LDL'	=> { 'in' => 0, 'out' => 1 },
	'ADC'	=> { 'in' => 1, 'out' => 1 },
	'CALL'	=> { 'in' => 3, 'out' => 0 }, # actually 3,3
	'CJ'	=> { 'in' => 3, 'out' => 3 },
	'AJW'	=> { 'in' => 0, 'out' => 0, 'wptr' => 1 },
	'EQC'	=> { 'in' => 1, 'out' => 1 },
	'STL'	=> { 'in' => 1, 'out' => 0 },
	'STNL'	=> { 'in' => 2, 'out' => 0 },
	'REV'	=> { 'in' => 2, 'out' => 2 },
	'LB'	=> { 'in' => 1, 'out' => 1 },
	'BSUB'	=> { 'in' => 2, 'out' => 1 },
	'MUL'	=> { 'in' => 2, 'out' => 1 },
	'SUB'	=> { 'in' => 2, 'out' => 1 },
	
};


sub new ($$) {
	my ($class) = @_;
	my $self = bless {}, $class;
	return $self;
}

sub foreach_label ($$@) {
	my ($labels, $func, @param) = @_;
	my @labels = keys (%$labels);
	foreach my $label (@labels) {
		&$func ($labels, $labels->{$label}, @param);
	}
}

sub foreach_inst ($$$@) {
	my ($labels, $label, $func, @param) = @_;
	my $inst = $label->{'inst'};
	foreach my $inst (@$inst) {
		&$func ($labels, $label, $inst, @param);
	}
}

sub resolve_inst_label ($$$$) {
	my ($labels, $label, $inst, $fn) = @_;
	
	return if $inst->{'name'} =~ /^\..*BYTES$/;
	
	my $arg = $inst->{'arg'};
	foreach my $arg (ref ($arg) =~ /^ARRAY/ ? @$arg : $arg) {
		if ($arg =~ /^L([0-9_\.]+)$/) {
			my $num	= $1;
			my $n	= 'L' . $fn . ':' . $num;
			if (!exists ($labels->{$n})) {
				die "Undefined label $n";
			} else {
				if ($arg eq $inst->{'arg'}) {
					$inst->{'arg'} = $labels->{$n};
				} else {
					$arg = $labels->{$n};
				}
				$labels->{$n}->{'refs'}++;
			}
			$inst->{'label_arg'} = 1;
		}
	}
}

sub resolve_labels ($$$) {
	my ($labels, $label, $fn) = @_;
	foreach_inst ($labels, $label, \&resolve_inst_label, $fn);
}

sub resolve_inst_globals ($$$$$) {
	my ($labels, $label, $inst, $globals, $ffi) = @_;
	if ($inst->{'label_arg'}) {
		my $arg = $inst->{'arg'};
		if ((ref ($arg) =~ /^HASH/) && $arg->{'stub'}) {
			my $n = $arg->{'stub'};
			if (exists ($globals->{$n})) {
				$inst->{'arg'} = $globals->{$n};
				$globals->{$n}->{'refs'}++;
			} elsif (exists ($ffi->{$n})) {
				$inst->{'arg'} = $ffi->{$n};
				$ffi->{$n}->{'refs'}++;
			} else {
				#die "Undefined global reference $n";
			}
		}
	}
}

sub resolve_globals ($$$$) {
	my ($labels, $label, $globals, $ffi) = @_;
	foreach_inst ($labels, $label, \&resolve_inst_globals, $globals, $ffi);
}

sub build_data_blocks ($$) {
	my ($labels, $label) = @_;
	my ($data, $inst) = (undef, 0);
	foreach my $op (@{$label->{'inst'}}) {
		my $name = $op->{'name'};
		if ($name =~ /^[^\.]/) {
			$inst++;
		} elsif ($name eq '.DATABYTES') {
			$data .= $op->{'arg'};
			$op->{'bytes'}	= [ split (//, $op->{'arg'}) ];
			$op->{'length'}	= length ($op->{'arg'});
		}
	}
	if (!$inst && $data) {
		$label->{'data'} = $data;
		if ($label->{'prev'}) {
			$label->{'prev'}->{'next'} = $label->{'next'};
		}
		if ($label->{'next'}) {
			$label->{'next'}->{'prev'} = $label->{'prev'};
		}
		delete ($label->{'prev'});
		delete ($label->{'next'});
	}
}

sub add_data_lengths ($$) {
	my ($labels, $label) = @_;
	if ($label->{'data'}) {
		$label->{'length'} = length ($label->{'data'});
	}
}

sub new_sub_label ($$$$) {
	my ($labels, $label, $current, $sub_idx) = @_;
	my $name		= sprintf ('%s_%d', $label->{'name'}, ($$sub_idx)++);
	my $new 		= {
		'name' => $name, 
		'prev' => $current,
		'next' => $current->{'next'},
		'inst' => undef
	};
	$current->{'next'}	= $new;
	if ($new->{'next'}) {
		$new->{'next'}->{'prev'} = $new;
	}
	$labels->{$name}	= $new;
	return $new;
}

sub isolate_static_sections ($$) {
	my ($labels, $label) = @_;
	
	return if $label->{'data'};

	my @inst	= @{$label->{'inst'}};
	my $sub_idx	= 0;
	my $current	= $label;
	my $cinst	= [];
	for (my $i = 0; $i < @inst; ++$i) {
		my $inst = $inst[$i];
		if ($inst->{'label_arg'}) {
			if (@$cinst > 0) {
				$current->{'inst'}	= $cinst;
				$current		= new_sub_label (
					$labels, $label, $current, \$sub_idx
				);
				$cinst			= [];
			}
			$current->{'inst'}		= [ $inst ];
			$current 			= new_sub_label (
				$labels, $label, $current, \$sub_idx
			) if $i < (@inst - 1);
		} else {
			push (@$cinst, $inst);
		}
	}
	if (@$cinst > 0) {
		$current->{'inst'} = $cinst;
	}
}

sub tag_and_index_code_blocks ($) {
	my ($procs) = @_;
	foreach my $proc (@$procs) {
		my @labels	= ($proc);
		my $label	= $proc->{'next'};
		my $idx		= 0;
		$proc->{'proc'}	= $proc;
		$proc->{'idx'}	= $idx++;
		while ($label && !$label->{'symbol'}) {
			$label->{'proc'}	= $proc;
			$label->{'idx'}		= $idx++;
			push (@labels, $label);
			$label = $label->{'next'};
		}
		$proc->{'labels'} = \@labels;
	}
}

sub separate_code_blocks ($) {
	my ($procs) = @_;
	foreach my $proc (@$procs) {
		my $labels	= $proc->{'labels'};
		my $first	= $proc;
		my $last	= $labels->[-1];

		if ($first->{'prev'}) {
			delete ($first->{'prev'}->{'next'});
		}
		delete ($first->{'prev'});

		if ($last->{'next'}) {
			delete ($last->{'next'}->{'prev'});
		}
		delete ($last->{'next'});
	}
}

sub instruction_proc_dependencies ($$$$) {
	my (undef, $label, $inst, $depends) = @_;
	if ($inst->{'label_arg'}) {
		my $arg = $inst->{'arg'};
		foreach my $arg (ref ($arg) =~ /^ARRAY/ ? @$arg : $arg) {
			next if !ref ($arg);
			if ($label->{'proc'} ne $arg->{'proc'}) {
				$depends->{$arg} = $arg;
			}
		}
	}
}

sub expand_etc_ops ($) {
	my ($etc) = @_;
	my %IGNORE_SPECIAL = (
		'CONTRJOIN'	=> 1,
		'CONTRSPLIT'	=> 1,
		'FPPOP'		=> 1
	);
	my ($labn, $labo) = (0, 0);
	
	for (my $i = 0; $i < @$etc; ++$i) {
		my $op		= $etc->[$i];
		my $name	= $op->{'name'};
		my $arg		= $op->{'arg'};
		if ($name =~ /^\.(SET|SECTION)LAB$/) {
			$labn = $arg;
			$labo = 0;
		} elsif ($name eq '.LABEL') {
			if (ref ($arg) =~ /^ARRAY/) {
				my $l1 = $arg->[0];
				my $l2 = $arg->[1];
				if ($l2->{'arg'} >= 0) {
					$l1->{'arg'}	= [ $l1->{'arg'}, 'L' . $l2->{'arg'} ];
					$etc->[$i]	= $l1;
				} else {
					$l1->{'arg'} 	= [ $l1->{'arg'}, 'LDPI' ];
					splice (@$etc, $i, 1, 
						$l1,
						{ 'name' => 'LDPI' }
					);
				}
			} else {
				$etc->[$i] = $arg;
			}
		} elsif ($name eq '.SPECIAL') {
			my $name	= $arg->{'name'};

			if ($name eq 'NOTPROCESS') {
				$op = { 
					'name'	=> 'LDC',
					'arg'	=> 0
				};
			} elsif ($name eq 'STARTTABLE') {
				my (@arg, $done, @table);
				for (my $j = ($i+1); !$done && $j < @$etc; ++$j) {
					my $op = $etc->[$j];
					if ($op->{'name'} eq '.LABEL') {
						my $op = $op->{'arg'};
						if ($op->{'name'} eq 'J') {
							push (@arg, $op->{'arg'});
							push (@table, $op);
							next;
						}
					}
					$done = 1;
				}
				my $size_op		= {
					'name'		=> 'LDC',
					'arg'		=> 0
				};
				$op->{'name'}		= 'TABLE';
				$op->{'arg'}		= \@arg;
				$op->{'label_arg'}	= 1;
				$op->{'table'}		= \@table;
				$op->{'size_op'}	= $size_op;
				my $mlab = $labn + (++$labo / 10.0);
				my $jlab = $labn + (++$labo / 10.0);
				splice (@$etc, $i, 1,
					{},
					$size_op,
					{ 'name' => 'PROD'	},
					{ 'name' => 'LDC', 'arg' => [ "L$jlab", "L$mlab" ] },
					{ 'name' => 'LDPI'	},
					{ 'name' => '.SETLAB', 'arg' => $mlab		},
					{ 'name' => 'BSUB'	},
					{ 'name' => 'GCALL'	},
					{ 'name' => '.SETLAB', 'arg' => $jlab		},
					$op
				);
			} elsif (!exists ($IGNORE_SPECIAL{$name})) {
				$op = $arg;
			}
			
			$etc->[$i] = $op;
		} elsif ($name =~ /^\.(LEND.?)$/) {
			my $name	= $1;
			my @arg		= @$arg;
			my $start	= ($arg[2] =~ /^L(\d+)$/)[0];
			my $end		= ($arg[1] =~ /^L(\d+)$/)[0];
			splice (@$etc, $i, 1, 
				$arg[0],
				{ 'name' => 'LDC', 'arg' => [ "L$end", "L$start" ]	},
				{ 'name' => $name					},
				{ 'name' => '.SETLAB', 'arg' => $end			}
			);
		} elsif ($name =~ /^\.SL([RL])IMM$/) {
			$op->{'name'} = "SH$1";
			splice (@$etc, $i, 1,
				{ 'name' => 'SAVECREG'				},
				{ 'name' => 'LDC', 'arg' => $op->{'arg'}	},
				$op,
				{ 'name' => 'RESTORECREG'			}
			);
			delete ($op->{'arg'});
		}
	}
}

sub preprocess_etc ($$$) {
	my ($self, $file, $etc) = @_;
	my ($current, %labels, @procs);
	my $globals	= $self->{'globals'};
	
	my $fn		= 0;
	my $align	= 0;
	my $filename	= undef;
	my $line	= undef;

	# Initial operation translation
	expand_etc_ops ($etc);

	# Build ETC stream for each label
	# Identify PROCs and global symbols
	# Carry alignment
	# Carry file names and line numbers
	foreach my $op (@$etc) {
		my $name	= $op->{'name'};
		my $arg		= $op->{'arg'};

		if ($name eq '.ALIGN') {
			$align	= $arg;
		} elsif ($name =~ /^\.(SET|SECTION)LAB$/) {
			my $label = 'L' . $fn . ':' . $arg;
			my @inst;

			die "Label collision $label" 
				if exists ($labels{$label});
			
			if ($filename) {
				push (@inst, { 
					'name'	=> '.FILENAME',
					'arg'	=> $filename
				});
			}
			if (defined ($line)) {
				push (@inst, {
					'name'	=> '.LINE',
					'arg'	=> $line
				});
			}
			if ($align) {
				push (@inst, {
					'name'	=> '.ALIGN',
					'arg'	=> $align
				});
			}

			my $new = { 
				'name'		=> $label, 
				'prev'		=> $current, 
				'inst'		=> \@inst,
				'align'		=> $align,
				'source'	=> $etc
			};

			$current->{'next'} 	= $new;
			$current 		= $new;
			$labels{$label}		= $new;

			$align			= 0;
		} elsif ($name eq '.FILENAME') {
			$filename		= $arg;
		} elsif ($name eq '.LINE') {
			$line			= $arg;
		} elsif ($name eq '.PROC') {
			$current->{'symbol'}	= $arg;
			push (@procs, $current);
		} elsif ($name eq '.STUBNAME') {
			$current->{'stub'}	= $arg;
			$current->{'symbol'}	= $arg;
			#if ($arg =~ /^(C|BX?)\./) {
			#	$ffi{$arg} = $current
			#		if !exists ($ffi{$arg});
			#}
		} elsif ($name eq '.GLOBAL') {
			if (exists ($globals->{$arg})) {
				my $current 	= $globals->{$arg};
				my $c_file	= $current->{'loci'}->{'file'};
				my $c_fn	= $current->{'loci'}->{'filename'};
				my $c_ln	= $current->{'loci'}->{'line'};
				print STDERR 
					"Warning: multiple definitions of global name '$arg'\n",
					"\tOld symbol is from $c_fn($c_file), line $c_ln.";
					"\tNew symbol is from $filename($file), line $line.";
			}
			$globals->{$arg}	= $current;
			$current->{'loci'}	= {
				'file'		=> $file,
				'filename'	=> $filename,
				'line'		=> $line
			};
		} elsif ($name eq '.GLOBALEND') {
			$globals->{$arg}->{'end'} = $current;
		}
		
		push (@{$current->{'inst'}}, $op)
			if $current; 
	}

	$self->{'file'}		= $file;
	$self->{'filename'}	= $filename;
	$self->{'labels'}	= \%labels;
	$self->{'procs'}	= \@procs;

	foreach_label (\%labels, \&resolve_labels, 0);
	foreach_label (\%labels, \&resolve_globals, $globals, {});
	foreach_label (\%labels, \&build_data_blocks);
	foreach_label (\%labels, \&add_data_lengths);
	foreach_label (\%labels, \&isolate_static_sections);
	tag_and_index_code_blocks (\@procs);
	separate_code_blocks (\@procs);
}

sub code_proc ($$) {
	my ($self, $proc) = @_;
	my $reg_n = 0;
	my $wptr_n = 0;
	my @stack;
	my $wptr = sprintf ('wptr_%d', $wptr_n++);

	foreach my $label (@{$proc->{'labels'}}) {
		print $label->{'name'}, " ", join (', ', @stack), " ($wptr)\n";
		
		$label->{'in'} = [ @stack ];
		$label->{'wptr'} = $wptr;

		foreach my $inst (@{$label->{'inst'}}) {
			my $name = $inst->{'name'};
			next if $name =~ /^\./;
			my (@in, @out);
			my $data	= $GRAPH->{$name};
			my $in		= $data->{'in'};
			my $out		= $data->{'out'};
			for (my $i = 0; $i < $in; ++$i) {
				my $reg = shift (@stack) || 'null';
				unshift (@in, $reg);
			}
			for (my $i = 0; $i < $out; ++$i) {
				my $reg = sprintf ('reg_%d', $reg_n++);
				unshift (@out, $reg);
				unshift (@stack, $reg);
			}
			$inst->{'in'} = \@in;
			$inst->{'out'} = \@out;
			$inst->{'wptr'} = $wptr;
			if ($data->{'wptr'}) {
				$wptr = sprintf ('wptr_%d', $wptr_n++);
				$inst->{'_wptr'} = $wptr;
			}
			print "\t";
			print join (', ', @in), " => " if @in;
			print $name;
			print " => ", join (', ', @out) if @out;
			if ($data->{'wptr'}) {
				print " (", $inst->{'wptr'}, ' => ', $inst->{'_wptr'}, ")";
			}
			print "\n";
		}
		
		$label->{'out'} = [ @stack ];
		$label->{'_wptr'} = $wptr;
	}
}

sub generate ($$) {
	my ($self, $text) = @_;
	my $verbose = $self->{'verbose'};
	my $file	= $text->{'file'};
	my $etc		= $text->{'etc'};
	my @assembly;

	$self->{'globals'} = {};
	$self->preprocess_etc ($file, $etc);
	foreach my $proc (@{$self->{'procs'}}) {
		$self->code_proc ($proc);
	}

	return \@assembly;
}

1;
