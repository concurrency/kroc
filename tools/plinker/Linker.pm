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

require 'Instructions.pm';

package Transterpreter::Linker;

use strict;
use Data::Dumper;

sub new ($) {
	my ($class, $instruct_h) = @_;

	my $self = {
		'instructions' => new Transterpreter::Instructions ()
	};
	
	$self = bless $self, $class;

	return $self;
}

sub link ($$@) {
	my ($self, $entry_point, @etc)	= @_;
	my $instructions		= $self->{'instructions'};

	# State
	my %ffi;
	my %globals;
	my $n = 0;

	# Tranform ETC
	foreach my $texts (@etc) {
		my @data;
		
		foreach my $text (@$texts) {
			my ($current, %labels, @procs);
			my $align	= 0;
			my $etc		= $text->{'etc'};
			my $file	= $text->{'file'};
			my $filename	= undef;
			my $line	= undef;
			my $global	= undef; # present global
			my $n_inst	= 0;     # "real" instructions in present/last global

			# Initial operation translation
			expand_etc_ops ($etc, $instructions);

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
					my $label = 'L' . $n . ':' . $arg;
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
					$n_inst			= 0;
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
					if ($arg =~ /^(C|BX?)\./) {
						$ffi{$arg} = $current
							if !exists ($ffi{$arg});
					}
				} elsif ($name eq '.GLOBAL') {
					if (exists ($globals{$arg})) {
						my $current 	= $globals{$arg};
						my $c_file	= $current->{'loci'}->{'file'};
						my $c_fn	= $current->{'loci'}->{'filename'};
						my $c_ln	= $current->{'loci'}->{'line'};
						print STDERR 
							"Warning: multiple definitions of global name '$arg'\n",
							"\tOld symbol is from $c_fn($c_file), line $c_ln.";
							"\tNew symbol is from $filename($file), line $line.";
					}
					$n_inst			= 0;
					$global			= $arg;
					$globals{$arg}		= $current;
					$current->{'loci'}	= {
						'file'		=> $file,
						'filename'	=> $filename,
						'line'		=> $line
					};
				} elsif ($name eq '.GLOBALEND') {
					$globals{$arg}->{'end'}	= $current;
					$global			= undef;
				} elsif ($name eq '.JUMPENTRY') {
					if ($global && !$n_inst) {
						$globals{$global} = {
							'jump_entry' => $arg
						}
					};
				} elsif ($name !~ /^\./) {
					$n_inst++;
				}
				
				push (@{$current->{'inst'}}, $op)
					if $current; 
			}

			# Resolve local labels
			foreach_label (\%labels, \&resolve_labels, $n);
			
			# Queue data for other passes
			push (@data, { 
				'file' 		=> $file, 
				'filename' 	=> $filename, 
				'labels' 	=> \%labels, 
				'procs' 	=> \@procs
			});
			$n++;
		}

		foreach my $d (@data) {
			my $labels 	= $d->{'labels'};
			my $procs	= $d->{'procs'};
			
			# Transform Passes
			foreach_label ($labels, \&resolve_globals, \%globals, \%ffi);
			foreach_label ($labels, \&follow_jump_entries, \%globals, \%ffi);
			foreach_label ($labels, \&build_data_blocks);
			foreach_label ($labels, \&add_data_lengths);
			foreach_label ($labels, \&isolate_static_sections);
			tag_and_index_code_blocks ($procs);
			separate_code_blocks ($procs);
			foreach_label ($labels, \&build_proc_dependencies);
		}
	}

	if (!exists ($globals{$entry_point})) {
		print STDERR "Unable to find entry point symbol $entry_point\n";
		return;
	}
	
	# The entry point might just be a jump entry
	while (exists ($globals{$entry_point}->{'jump_entry'})) {
		$entry_point = $globals{$entry_point}->{'jump_entry'};
	}
	
	# Code from entry point down through dependencies
	my @coding_order;
	build_coding_order ($globals{$entry_point}, \@coding_order);
	generate_code (\@coding_order, $instructions);
	
	# Add jump entry and shift labels
	my $jump = $self->jump_entry ($globals{$entry_point});
	@coding_order = expand_coding_order (@coding_order);
	shift_labels ($jump->{'length'}, @coding_order);

	return ($jump, @coding_order);
}

sub count_nybbles ($) {
	my $value = shift;
	my $count = 1;
	while ($value >>= 4) {
		$count++;
	}
	return $count;
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

sub expand_etc_ops ($$) {
	my ($etc, $instructions) = @_;
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
			} elsif (exists ($ffi->{$n})) {
				$inst->{'arg'} = $ffi->{$n};
			} else {
				die "Undefined global reference $n";
			}
			$inst->{'arg'}->{'refs'}++;
		}
	}
}

sub resolve_globals ($$$$) {
	my ($labels, $label, $globals, $ffi) = @_;
	foreach_inst ($labels, $label, \&resolve_inst_globals, $globals, $ffi);
}

sub follow_jump_entry_inst ($$$$$) {
	my ($labels, $label, $inst, $globals, $ffi) = @_;
	if (ref ($inst->{'arg'}) =~ /^HASH/ && $inst->{'arg'}->{'jump_entry'}) {
		my $arg = $inst->{'arg'};
		while ($arg->{'jump_entry'}) {
			my $n = $arg->{'jump_entry'};
			if (exists ($globals->{$n})) {
				$arg = $globals->{$n};
			} elsif (exists ($ffi->{$n})) {
				$arg = $ffi->{$n};
			} else {
				die "Undefined global reference $n (in jump entry)";
			}
		}
		$inst->{'arg'} = $arg;
		$arg->{'refs'}++;
	}
}

sub follow_jump_entries ($$$$) {
	my ($labels, $label, $globals, $ffi) = @_;
	foreach_inst ($labels, $label, \&follow_jump_entry_inst, $globals, $ffi);
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

sub build_proc_dependencies ($$) {
	my ($labels, $label) = @_;
	my $proc = $label->{'proc'};
	if ($proc) {
		my $depends = $proc->{'proc_depends'} || {};
		foreach_inst (undef, $label, \&instruction_proc_dependencies, $depends);
		delete ($depends->{$proc}); # no self-dependencies
		$proc->{'proc_depends'} = $depends;
	}
}

sub build_coding_order ($$) {
	my ($label, $order) = @_;
	my $depends = $label->{'proc_depends'};
	
	if ($depends) {
		$label->{'ordering'} = 1;
		
		my @suborder = sort {
			my $da = $depends->{$a};
			my $db = $depends->{$b};
			my $rc = $da->{'refs'} <=> $db->{'refs'};
			return $rc if $rc != 0;
			return ($a cmp $b);
		} (keys (%$depends));

		foreach my $dep (@suborder) {
			my $label = $depends->{$dep};
			$label = $label->{'proc'}
				if $label->{'proc'};
			if ($label->{'ordering'}) {
				my $name = $label->{'name'};
				my $proc = $label->{'proc'} || "data";
				die "Recursive label dependencies $name ($proc)";
			}
			build_coding_order ($label, $order)
				if !$label->{'ordered'};
		}
		
		delete ($label->{'ordering'});
	}

	$label->{'ordered'} = 1;
	push (@{$order}, $label);
}

sub code_instruction ($$$$) {
	my ($num, $arg, $length, $instructions) = @_;
	my @bytes;
	
	my $negate = 0;
	if ($arg < 0) {
		$negate = 1;
		$arg = ~$arg;
	}

	my $nybbles = count_nybbles ($arg);
	if ($nybbles == 1 && $negate) {
		$nybbles = 2;
	}

	for (my $i = ($nybbles - 1); $i >= 0; --$i) {
		my $byte = ($arg >> ($i * 4)) & 0xf;
		if ($i == 0) {
			$byte 	|= $num;
		} elsif ($i == 1 && $negate) {
			$byte	|= $instructions->primary ('NFIX');
			$arg 	= ~$arg;
		} else {
			$byte 	|= $instructions->primary ('PFIX');
		}
		push (@bytes, pack ('C', $byte));
	}

	if (defined ($length)) {
		if ($arg < 0) {
			while (@bytes < $length) {
				push (@bytes, code_instruction (
					$instructions->primary ('J'),
					0,
					undef,
					$instructions
				));
			}
		} else {
			while (@bytes < $length) {
				unshift (@bytes, code_instruction (
					$instructions->primary ('PFIX'),
					0,
					undef,
					$instructions
				));
			}
		}	
	}
	
	return @bytes;
}

sub code_static_instruction ($$$$) {
	my (undef, $label, $inst, $instructions) = @_;
	
	return if $inst->{'label_arg'};
	return if exists ($inst->{'length'});
	
	my $name	= $inst->{'name'};
	my $arg		= $inst->{'arg'};
	my $length	= $inst->{'target_length'};
	my @bytes;

	if ($name =~ /^\./) {
		# Special
	} else {
		my $num;

		if (defined ($arg) && $instructions->valid_primary ($name)) {
			# Primary
			$num = $instructions->primary ($name);
		} elsif ($instructions->valid_instruction ($name)) {
			# Other
			$arg = $instructions->numeric ($name);
			$num = $instructions->primary ('OPR');
		} else {
			die "Unknown instruction $name";
		}

		@bytes = code_instruction ($num, $arg, $length, $instructions);
	}

	if (@bytes) {
		$inst->{'bytes'} = \@bytes;
	}
	
	$inst->{'length'} = scalar (@bytes);
}

sub code_static_instructions ($$) {
	my ($labels, $instructions) = @_;
	foreach my $label (@$labels) {
		foreach_inst (undef, $label, \&code_static_instruction, $instructions);
	}
}

sub tag_if_complete ($) {
	my $label = shift;
	my $inst = $label->{'inst'};
	my $static = 1;
	my $length = 0;
	for (my $i = 0; $static && $i < @$inst; ++$i) {
		my $inst = $inst->[$i];
		if (exists ($inst->{'length'})) {
			$length += $inst->{'length'};
		} else {
			$static = 0;
		}
	}
	if ($static) {
		$label->{'length'} = $length;
	}
}

sub tag_complete_labels ($) {
	my $labels = shift;
	foreach my $label (@$labels) {
		die if exists ($label->{'data'});
		tag_if_complete ($label);
	}
}

sub merge_offsets ($$$) {
	my ($length, $offsets, $instructions) = @_;
	my $diff;
	my @d;

	foreach my $o (@$offsets) {
		if (ref ($o)) {
			push (@d, $o);
		} else {
			my @diff = code_instruction (
				$instructions->primary ('OPR'),
				$instructions->numeric ($o),
				undef,
				$instructions
			);

			$diff += scalar (@diff);
		}
	}

	die if !@d;
	my $ret = 0;

	if ($d[0]->{'d'} eq '+') {
		$ret = $d[0]->{'v'} - $diff;
	} elsif ($d[0]->{'d'} eq '-') {
		$ret = -($d[0]->{'v'} + $length + $diff);
	} else {
		die;
	}

	for (my $i = 1; $i < @d; ++$i) {
		my $o = $d[$i];
		if ($o->{'d'} eq '+') {
			$ret -= $o->{'v'} - $diff;
		} elsif ($o->{'d'} eq '-') {
			$ret -= -($o->{'v'} + $length + $diff);
		} else {
			die;
		}
	}

	return $ret;
}

sub code_offset_instruction ($$$$) {
	my ($name, $length, $offsets, $instructions) = @_;
	my $num	= $instructions->primary ($name);

	die "Unknown instruction $name" 
		if !defined ($num);

	# Estimate
	my @bytes = code_instruction (
		$num,
		merge_offsets (1, $offsets, $instructions),
		undef,
		$instructions
	);

	# Code until stable
	my $stable;
	do {
		my @new_bytes = code_instruction (
			$num,
			merge_offsets (scalar (@bytes), $offsets, $instructions),
			undef,
			$instructions
		);
		$stable = scalar (@bytes) == scalar (@new_bytes);
		@bytes	= @new_bytes;
	} until ($stable);

	# Add padding (if required)
	if (defined ($length)) {
		@bytes = code_instruction (
			$num,
			merge_offsets (scalar (@bytes), $offsets, $instructions),
			$length,
			$instructions
		);
	}

	return @bytes;
}

sub code_table ($$$) {
	my ($label, $inst, $instructions) = @_;
	my $max_length = 0;

	die if scalar (@{$inst->{'table'}}) == 0;

	foreach my $op (@{$inst->{'table'}}) {
		return 0 if !exists ($op->{'length'});
		my $length = $op->{'length'};
		$max_length = $length if $length > $max_length;
	}
	
	die if $max_length == 0;

	foreach my $op (@{$inst->{'table'}}) {
		$op->{'target_length'} = $max_length;
	}

	my $size_op	= $inst->{'size_op'};
	my @bytes	= code_instruction (
		$instructions->primary ($size_op->{'name'}),
		$max_length,
		undef,
		$instructions
	);
	
	$size_op->{'arg'}	= $max_length;
	$size_op->{'bytes'}	= \@bytes;
	$size_op->{'length'}	= scalar (@bytes);

	$inst->{'length'}	= 0;
	$label->{'length'}	= 0;

	return 1;
}

sub code_offset ($$$) {
	my ($label, $inst, $instructions) = @_;
	my $arg	= $inst->{'arg'};
	my @offsets;

	return code_table ($label, $inst, $instructions)
		if $inst->{'name'} eq 'TABLE';

	foreach my $arg (ref ($arg) =~ /^ARRAY/ ? @$arg : $arg) {
		if (!ref ($arg)) {
			push (@offsets, $arg);
			next;
		}

		my $this	= $label->{'idx'};
		my $backward;
		my $distance;
		my $target;

		if ($arg->{'proc'} eq $label->{'proc'}) {
			$target = $arg->{'idx'};
			$distance = 0;
		} else {
			$target = $label->{'proc'}->{'idx'};
			$distance = $label->{'proc'}->{'pos'} - $arg->{'pos'};
		}

		if ($target > $this) {
			# Forward Jump
			my $p = $label->{'next'};
			while ($p->{'idx'} != $target) {
				return 0 if !exists ($p->{'length'});
				$distance 	+= $p->{'length'};
				$p		= $p->{'next'};
			}
			push (@offsets, { 'd' => '+', 'v' => $distance });
		} elsif ($target < $this) {
			# Backward Jump
			my $p = $label->{'prev'};
			while ($p->{'idx'} != $target) {
				return 0 if !exists ($p->{'length'});
				$distance 	+= $p->{'length'};
				$p		= $p->{'prev'};
			}
			return 0 if !exists ($p->{'length'});
			$distance += $p->{'length'};
			push (@offsets, { 'd' => '-', 'v' => $distance });
		} else {
			# Self Reference
			push (@offsets, { 'd' => '-', 'v' => 0 });
		}
	}

	my $name	= $inst->{'name'};
	my @bytes	= code_offset_instruction (
		$name, $inst->{'target_length'}, \@offsets, $instructions
	);

	$inst->{'bytes'}	= \@bytes;
	$inst->{'length'}	= scalar (@bytes);
	$label->{'length'}	= $inst->{'length'};

	return 1;
}

sub code_offset_if_known ($$) {
	my ($label, $instructions) = @_;
	my $inst	= $label->{'inst'}->[0];
	my $arg		= $inst->{'arg'};
	
	foreach my $arg (ref ($arg) =~ /^ARRAY/ ? @$arg : $arg) {
		next if !ref ($arg);
		return 0 if $arg->{'proc'} ne $label->{'proc'};
	}

	return code_offset ($label, $inst, $instructions);
}

sub code_known_offsets ($$) {
	my ($labels, $instructions) = @_;
	my $coded = 0;
	foreach my $label (@$labels) {
		next if exists ($label->{'length'});
		$coded += code_offset_if_known ($label, $instructions);
	}
	return $coded;
}

sub code_static_external_offsets ($$) {
	my ($labels, $instructions) = @_;
	my $pos = $labels->[0]->{'pos'};

	foreach my $label (@$labels) {
		if (exists ($label->{'length'})) {
			# Computed label, move on to next
			$pos += $label->{'length'};
			next;
		}

		my $inst	= $label->{'inst'}->[0];
		my $arg		= $inst->{'arg'};
		my @offsets;
	
		# Can't compute tables
		return if $inst->{'name'} eq 'TABLE';

		foreach my $arg (ref ($arg) =~ /^ARRAY/ ? @$arg : $arg) {
			if (!ref ($arg)) {
				push (@offsets, $arg);
			} elsif ($arg->{'proc'} eq $label->{'proc'}) {
				# Uncomputed internal reference can't continue
				return;
			} else {
				push (@offsets, { 'd' => '-', 'v' => $pos - $arg->{'pos'} });
			}
		}

		my $name	= $inst->{'name'};
		my @bytes	= code_offset_instruction (
			$name, $inst->{'target_length'}, 
			\@offsets, $instructions
		);

		$inst->{'bytes'}	= \@bytes;
		$inst->{'length'}	= scalar (@bytes);
		$label->{'length'}	= $inst->{'length'};
		$pos 			+= $label->{'length'};
	}
}

sub build_dynamic_label_map ($$) {
	my ($proc, $labels) = @_;
	my @map = scalar (@$labels) * 2;
	my $i	= 0;

	foreach my $label (@$labels) {
		if ($label->{'length'}) {
			$map[$i++] = '';
			$map[$i++] = undef;
		} else {
			$map[$i++] = $label;
			$map[$i++] = $label;
		}
	}
	
	$proc->{'dynamic_label_map'} = \@map;
}

sub build_label_dependencies ($) {
	my ($label)	= @_;
	my $idx		= $label->{'idx'};
	my $map		= $label->{'proc'}->{'dynamic_label_map'};
	my $arg		= $label->{'inst'}->[0]->{'arg'};
	my %deps;

	foreach my $arg (ref ($arg) =~ /^ARRAY/ ? @$arg : $arg) {
		next if !ref ($arg);
		my $arg_idx = $arg->{'proc'} == $label->{'proc'} ?
				$arg->{'idx'} : 0;

		# Slice bounds (lower, upper)
		my $lb = ($arg_idx <= $idx ? $arg_idx : $idx) * 2;
		my $ub = (($arg_idx >= $idx ? $arg_idx : $idx) * 2) + 1;

		# Slice map, hash and merge
		my %arg_deps = @{$map}[$lb..$ub];
		merge_hashes (\%deps, \%arg_deps);
	}
	
	# Remove empty and self references
	delete ($deps{''});
	delete ($deps{$label});

	# If we have no dynamic dependencies something is wrong
	if (keys (%deps) == 0) {
		my $name = $label->{'name'};
		die "No dynamic dependencies $name";
	}

	# Record ourselves in dependent hashes of dependencies
	foreach my $k (keys (%deps)) {
		my $dependents = $deps{$k}->{'dependents'};
		if (!$dependents) {
			$dependents			= {};
			$deps{$k}->{'dependents'}	= $dependents;
		}
		$dependents->{$label} = $label;
	}
}

sub build_label_dependents ($) {
	my ($labels) = @_;
	foreach my $label (@$labels) {
		next if exists ($label->{'length'});
		build_label_dependencies ($label);
	}
}

sub add_initial_estimate_lengths ($) {
	my ($labels) = @_;
	foreach my $label (@$labels) {
		if (!exists ($label->{'length'})) {
			my $inst = $label->{'inst'}->[0];
			if (!$inst || ($inst->{'name'} eq 'TABLE')) {
				$label->{'length'}	= 0;
			} else {
				$inst->{'length'}	= 1
					if !exists ($inst->{'length'});
				$label->{'length'}	= 1;
			}
			$label->{'estimate'}		= 1;
		}
	}
}

sub merge_hashes ($$) {
	my ($dst, $src) = @_;
	foreach my $key (keys (%$src)) {
		$dst->{$key} = $src->{$key}
			if !exists ($dst->{$key});
	}
}

sub first_inst ($) {
	my $inst = shift;
	foreach my $inst (@$inst) {
		return $inst if $inst->{'name'} !~ /^\./;
	}
	die "Unable to find instruction in label";
}

sub code_dynamic_offsets ($$) {
	my ($labels, $instructions) = @_;
	my $to_code = {};
	
	# Initial pass list is all estimated labels
	foreach my $label (@$labels) {
		$to_code->{$label} = $label if (exists ($label->{'estimate'}));
	}

	# Keep interating until all labels are stable
	while (keys (%$to_code)) {
		my $pending = {};
		foreach my $k (keys (%$to_code)) {
			my $label = $to_code->{$k};
			my $length = $label->{'length'};
			die if !code_offset ($label, first_inst ($label->{'inst'}), $instructions);
			if ($label->{'length'} != $length) {
				merge_hashes ($pending, $label->{'dependents'});
			}
		}
		$to_code = $pending;
	}

	# Code all the labels again now they are stable
	foreach my $label (@$labels) {
		next if !$label->{'estimate'};
		my $length = $label->{'length'};

		die if !code_offset ($label, first_inst ($label->{'inst'}), $instructions);
		die if $length != $label->{'length'};
		
		delete ($label->{'estimate'});
	}
}

sub tag_positions ($$) {
	my ($labels, $pos) = @_;
	foreach my $label (@$labels) {
		$label->{'pos'} = $pos;
		$pos += $label->{'length'};
	}
	return $pos;
}

sub build_ffi_call ($$) {
	my ($label, $ffi_idx)	= @_;
	my $inst		= $label->{'inst'};
	my $stub		= $label->{'stub'};
	my $idx			= $$ffi_idx;
	
	if ($stub =~ /^C\.tvmspecial\.(\d+)/) {
		$idx = -($1 + 1);
	} else {
		my ($name) = ($stub =~ /^.*?\.(.*)$/);
		$name =~ s/\./_/g;

		$label->{'ffi_index'} 	= $idx;
		$label->{'ffi_symbol'}	= $name;

		${$ffi_idx}++;
	}

	push (@$inst, 
		{ 'name' => 'LDC', 'arg' => $idx	},
		{ 'name' => 'FFICALL'			}
	);

	delete ($label->{'prev'});
	delete ($label->{'next'});
}

sub generate_code ($$) {
	my ($labels, $instructions) = @_;
	my $ffi_idx	= 0;
	my $pos		= 0;
	foreach my $label (@$labels) {
		if ($label->{'align'}) {
			my %alignment = ( 1 => 0x1, 2 => 0x3, 3 => 0x7, 4 => 0xf );
			my $mask = $alignment{$label->{'align'}};
			if ($pos & $mask) {
				$pos &= ~$mask;
				$pos += $mask + 1;
			}
		}

		$label->{'pos'} = $pos;

		if ($label->{'proc'}) {
			my $proc	= $label->{'proc'};
			my $labels	= $proc->{'labels'};
			code_static_instructions ($labels, $instructions);
			tag_complete_labels ($labels);
			code_static_external_offsets ($labels, $instructions);
			while (code_known_offsets ($labels, $instructions) > 0) {
				code_static_external_offsets ($labels, $instructions);
			}
			build_dynamic_label_map ($proc, $labels);
			build_label_dependents ($labels);
			add_initial_estimate_lengths ($labels);
			code_dynamic_offsets ($labels, $instructions);
			$pos = tag_positions ($labels, $pos);
		} elsif ($label->{'stub'}) {
			my $labels = [ $label ];
			build_ffi_call ($label, \$ffi_idx);
			code_static_instructions ($labels, $instructions);
			tag_complete_labels ($labels);
			$pos = tag_positions ($labels, $pos);
		} else {
			$pos += $label->{'length'};
		}
	}
}

sub jump_entry ($$) {
	my ($self, $entry) 	= @_;
	my $instructions	= $self->{'instructions'};

	my @bytes		= code_instruction (
		$instructions->numeric ('J'),
		$entry->{'pos'},
		undef,
		$instructions
	);

	while (@bytes < 4) {
		@bytes = (
			code_instruction (
				$instructions->numeric ('PFIX'),
				0,
				undef,
				$instructions
			),
			@bytes
		);
	}

	return {
		'name'	=> 'Jump',
		'pos'	=> 0,
		'inst'	=> [
			{
				'name'	=> 'J',
				'arg'	=> $entry->{'pos'},
				'bytes'	=> \@bytes
			}
		],
		'length' => scalar (@bytes)
	};
}

sub expand_coding_order (@) {
	my @coding_order = @_;
	my @ret;
	foreach my $ent (@coding_order) {
		my $label = $ent;
		do {
			push (@ret, $label);
			$label = $label->{'next'};
		} while ($label);
	}
	return @ret;
}

sub shift_labels ($@) {
	my ($shift, @labels) = @_;
	foreach my $label (@labels) {
		$label->{'pos'} += $shift;
	}
}

1;
