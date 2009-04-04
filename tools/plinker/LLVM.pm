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
	'J' 		=> { 'in' => 3 },
	'LDLP'		=> { 'out' => 1 },
	'LDNL'		=> { 'in' => 1, 'out' => 1 },
	'LDC'		=> { 'out' => 1 },
	'LDNLP'		=> { 'in' => 1, 'out' => 1 },
	'LDL'		=> { 'out' => 1 },
	'ADC'		=> { 'in' => 1, 'out' => 1 },
	'CALL'		=> { 'in' => 3, 'out' => 0, 'vstack' => 1 }, # actually 3,3
	'CJ'		=> { 'in' => 3, 'out' => 2 },
	'AJW'		=> { 'wptr' => 1 },
	'EQC'		=> { 'in' => 1, 'out' => 1 },
	'STL'		=> { 'in' => 1 },
	'STNL'		=> { 'in' => 2 },
	'REV'		=> { 'in' => 2, 'out' => 2 },
	'LB'		=> { 'in' => 1, 'out' => 1 },
	'BSUB'		=> { 'in' => 2, 'out' => 1 },
	'ENDP'		=> { 'in' => 1 },
	'DIFF'		=> { 'in' => 2, 'out' => 1 },
	'ADD'		=> { 'in' => 2, 'out' => 1 },
	'GCALL'		=> { 'in' => 3, 'vstack' => 1 }, # check
	'IN'		=> { 'in' => 3 },
	'PROD'		=> { 'in' => 2, 'out' => 1 },
	'GT'		=> { 'in' => 2, 'out' => 1 },
	'WSUB'		=> { 'in' => 2, 'out' => 1 },
	'OUT'		=> { 'in' => 3 },
	'SUB'		=> { 'in' => 2, 'out' => 1 },
	'STARTP'	=> { 'in' => 3, 'vstack' => 1 },
	'OUTBYTE'	=> { 'in' => 2, 'vstack' => 1 },
	'OUTWORD'	=> { 'in' => 2, 'vstack' => 1 },
	'SETERR'	=> { 'in' => 1, 'vstack' => 1 },
	'MRELEASEP'	=> { 'in' => 1, 'vstack' => 1 },
	'CSUB0'		=> { 'in' => 1 },
	'EXTVRFY' 	=> { 'in' => 2 },
	'STOPP'		=> { 'vstack' => 1 },
	'LADD'		=> { 'in' => 3, 'out' => 1 },
	'NORM'		=> { 'in' => 3, 'out' => 3 },
	'LDIV'		=> { 'in' => 3, 'out' => 2 },
	'REM'		=> { 'in' => 2, 'out' => 2 },
	'LDTIMER'	=> { 'out' => 1, 'vstack' => 1 },
	'TIN'		=> { 'in' => 1, 'vstack' => 1 },
	'DIV'		=> { 'in' => 2, 'out' => 2 },
	'DIST'		=> { 'in' => 3, 'out' => 1, 'vstack' => 1 },
	'DISS'		=> { 'in' => 2, 'out' => 1, 'vstack' => 1 },
	'LMUL'		=> { 'in' => 3, 'out' => 2 },
	'NOT'		=> { 'in' => 1, 'out' => 1 },
	'XOR'		=> { 'in' => 2, 'out' => 1 },
	'LSHR'		=> { 'in' => 3, 'out' => 2 },
	'LSHL'		=> { 'in' => 3, 'out' => 2 },
	'LSUM'		=> { 'in' => 3, 'out' => 2 },
	'LSUB'		=> { 'in' => 3, 'out' => 2 },
	'RUNP'		=> { 'in' => 1, 'vstack' => 1 },
	'SB'		=> { 'in' => 2 },
	'GAJW'		=> { 'in' => 1, 'out' => 1, 'wptr' => 1 },
	'SHR'		=> { 'in' => 2, 'out' => 1 },
	'SHL'		=> { 'in' => 2, 'out' => 1 },
	'MINT'		=> { 'out' => 1 },
	'AND'		=> { 'in' => 2, 'out' => 1 },
	'ENBT'		=> { 'in' => 2, 'out' => 1, 'vstack' => 1 },
	'ENBC'		=> { 'in' => 2, 'out' => 1, 'vstack' => 1 },
	'ENBS'		=> { 'in' => 1, 'out' => 1, 'vstack' => 1 },
	'MOVE'		=> { 'in' => 3 },
	'OR'		=> { 'in' => 2, 'out' => 1 },
	'LDIFF'		=> { 'in' => 3, 'out' => 2 },
	'SUM'		=> { 'in' => 2, 'out' => 1 },
	'MUL'		=> { 'in' => 2, 'out' => 1 },
	'DUP'		=> { 'in' => 1, 'out' => 2 },
	'EXTIN'		=> { 'in' => 3, 'vstack' => 1 },
	'EXTOUT'	=> { 'in' => 3, 'vstack' => 1 },
	'POSTNORMSN'	=> { 'in' => 3, 'out' => 3 },
	'ROUNDSN'	=> { 'in' => 3, 'out' => 1 },
	'ENBC3'		=> { 'in' => 3, 'out' => 1, 'vstack' => 1 },
	'LDINF'		=> { 'out' => 1 },
	'POP'		=> { 'in' => 1 },
	'WSUBDB'	=> { 'in' => 2, 'out' => 1 },
	'FPLDNLDBI'	=> { 'in' => 2, 'fout' => 1 },
	'FPCHKERR'	=> { 'vstack' => 1 },
	'FPSTNLDB'	=> { 'in' => 1, 'fin' => 1 },
	'FPLDNLSNI'	=> { 'in' => 2, 'fout' => 1 },
	'FPADD'		=> { 'fin' => 2, 'fout' => 1 },
	'FPSTNLSN'	=> { 'in' => 1, 'fin' => 1 },
	'FPSUB'		=> { 'fin' => 2, 'fout' => 1 },
	'FPLDNLDB'	=> { 'in' => 1, 'fout' => 1 },
	'FPMUL'		=> { 'fin' => 2, 'fout' => 1 },
	'FPDIV'		=> { 'fin' => 2, 'fout' => 1 },
	'FPLDNLSN'	=> { 'in' => 1, 'fout' => 1 },
	'FPNAN'		=> { 'fin' => 1, 'fout' => 1, 'out' => 1 },
	'FPORDERED'	=> { 'fin' => 2, 'fout' => 2, 'out' => 1 },
	'FPNOTFINITE'	=> { 'fin' => 1, 'fout' => 1, 'out' => 1 },
	'FPGT'		=> { 'fin' => 2, 'out' => 1 },
	'FPEQ'		=> { 'fin' => 2, 'out' => 1 },
	'FPI32TOR32'	=> { 'in' => 1, 'fout' => 1 },
	'FPI32TOR64'	=> { 'in' => 1, 'fout' => 1 },
	'ENBT3'		=> { 'in' => 3, 'out' => 1, 'vstack' => 1 },
	'FPB32TOR64'	=> { 'in' => 1, 'fout' => 1 },
	'FPRTOI32'	=> { 'fin' => 1, 'fout' => 1 },
	'FPSTNLI32'	=> { 'in' => 1, 'fin' => 1 },
	'FPLDZEROSN'	=> { 'fout' => 1 },
	'FPLDZERODB'	=> { 'fout' => 1 },
	'FPINT'		=> { 'fin' => 1, 'fout' => 1 },
	'GETPRI'	=> { 'out' => 1, 'vstack' => 1 },
	'FPDUP'		=> { 'fin' => 1, 'fout' => 2 },
	'FPREV'		=> { 'fin' => 2, 'fout' => 2 },
	'SETPRI'	=> { 'in' => 1, 'vstack' => 1 },
	'FPLDNLADDDB'	=> { 'in' => 1, 'fin' => 1, 'fout' => 1 },
	'FPLDNLMULDB'	=> { 'in' => 1, 'fin' => 1, 'fout' => 1 },
	'FPLDNLADDSN'	=> { 'in' => 1, 'fin' => 1, 'fout' => 1 },
	'FPLDNLMULSN'	=> { 'in' => 1, 'fin' => 1, 'fout' => 1 },
	'ENBS3'		=> { 'in' => 2, 'out' => 1, 'vstack' => 1 },
	'FPREM'		=> { 'fin' => 2, 'fout' => 1 },
	'FPDIVBY2'	=> { 'fin' => 1, 'fout' => 1 },
	'FPMULBY2'	=> { 'fin' => 1, 'fout' => 1 },
	'FPSQRT'	=> { 'fin' => 1, 'fout' => 1 },
	'FPRZ'		=> { },
	'FPR32TOR64'	=> { 'fin' => 1, 'fout' => 1 },
	'FPR64TOR32'	=> { 'fin' => 1, 'fout' => 1 },
	'FPEXPDEC32'	=> { 'fin' => 1, 'fout' => 1 },
	'FPABS'		=> { 'fin' => 1, 'fout' => 1 },
	'MALLOC'	=> { 'in' => 1, 'out' => 1, 'vstack' => 1 },
	'MRELEASE'	=> { 'in' => 1, 'vstack' => 1 },
	'XABLE'		=> { 'vstack' => 1 },
	'XIN'		=> { 'in' => 3, 'vstack' => 1 },
	'XEND'		=> { 'vstack' => 1 },
	'NULL'		=> { 'out' => 1 },
	'PROC_ALLOC'	=> { 'in' => 2, 'out' => 1, 'vstack' => 1 },
	'PROC_PARAM'	=> { 'in' => 3, 'vstack' => 1 },
	'PROC_MT_COPY'	=> { 'in' => 3, 'vstack' => 1 },
	'PROC_MT_MOVE'	=> { 'in' => 3, 'vstack' => 1 },
	'PROC_START'	=> { 'in' => 3, 'vstack' => 1 },
	'PROC_END'	=> { 'in' => 1, 'vstack' => 1 },
	'GETAFF'	=> { 'out' => 1, 'vstack' => 1 },
	'SETAFF'	=> { 'in' => 1, 'vstack' => 1 },
	'GETPAS'	=> { 'out' => 1, 'vstack' => 1 },
	'MT_ALLOC'	=> { 'in' => 2, 'out' => 1, 'vstack' => 1 },
	'MT_RELEASE'	=> { 'in' => 1, 'vstack' => 1 },
	'MT_CLONE'	=> { 'in' => 1, 'out' => 1, 'vstack' => 1 },
	'MT_IN'		=> { 'in' => 2, 'vstack' => 1 },
	'MT_OUT'	=> { 'in' => 2, 'vstack' => 1 },
	'MT_XCHG'	=> { 'in' => 2, 'vstack' => 1 },
	'MT_LOCK'	=> { 'in' => 2, 'vstack' => 1 },
	'MT_UNLOCK'	=> { 'in' => 2, 'vstack' => 1 },
	'MT_ENROLL'	=> { 'in' => 2, 'vstack' => 1 },
	'MT_RESIGN'	=> { 'in' => 2, 'vstack' => 1 },
	'MT_SYNC'	=> { 'in' => 1, 'vstack' => 1 },
	'MT_XIN'	=> { 'in' => 2, 'vstack' => 1 },
	'MT_XOUT'	=> { 'in' => 2, 'vstack' => 1 },
	'MT_XXCHG'	=> { 'in' => 2, 'vstack' => 1 },
	'MT_DCLONE'	=> { 'in' => 3, 'out' => 1, 'vstack' => 1 },
	'MT_BIND'	=> { 'in' => 3, 'out' => 1, 'vstack' => 1 },
	'EXT_MT_IN'	=> { 'in' => 2, 'vstack' => 1 },
	'EXT_MT_OUT'	=> { 'in' => 2, 'vstack' => 1 },
	'MT_RESIZE'	=> { 'in' => 3, 'out' => 1, 'vstack' => 1 },
	'LEND'		=> { 'in' => 1 },
	'LEND3'		=> { 'in' => 1 },
	'LENDB'		=> { 'in' => 1 }
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

sub isolate_branches ($$) {
	my ($labels, $label) = @_;
	
	return if $label->{'data'};

	my @inst	= @{$label->{'inst'}};
	my $sub_idx	= 0;
	my $current	= $label;
	my $cinst	= [];
	for (my $i = 0; $i < @inst; ++$i) {
		my $inst = $inst[$i];
		if ($inst->{'name'} =~ /^C?J$/i && $inst->{'label_arg'}) {
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
				{ 'name' => $name, 'arg' => "L$start"			},
				{ 'name' => '.SETLAB', 'arg' => $end			}
			);
		} elsif ($name =~ /^\.SL([RL])IMM$/) {
			$op->{'name'} = "SH$1";
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
	foreach_label (\%labels, \&isolate_branches);
	tag_and_index_code_blocks (\@procs);
	separate_code_blocks (\@procs);
}

sub define_registers ($$) {
	my ($self, $labels) = @_;
	my ($reg_n, $freg_n, $wptr_n) = (0, 0, 0);
	my $wptr = sprintf ('wptr_%d', $wptr_n++);
	my (@stack, @fstack);

	foreach my $label (@$labels) {
		#print $label->{'name'}, " ", join (', ', @stack, @fstack), " ($wptr)\n";
		
		$label->{'in'} = [ @stack ];
		$label->{'fin'} = [ @fstack ];
		$label->{'wptr'} = $wptr;

		foreach my $inst (@{$label->{'inst'}}) {
			my $name = $inst->{'name'};
			next if $name =~ /^\./;
			my (@in, @out, @fin, @fout);
			my $data	= $GRAPH->{$name};
			for (my $i = 0; $i < $data->{'in'}; ++$i) {
				my $reg = shift (@stack);
				push (@in, $reg) if $reg;
			}
			my $out = $data->{'out'};
			if ($name eq 'CJ') {
				$out = @in - ($data->{'in'} - $out);
			}
			for (my $i = 0; $i < $out; ++$i) {
				my $reg = sprintf ('reg_%d', $reg_n++);
				unshift (@out, $reg);
				unshift (@stack, $reg);
			}
			for (my $i = 0; $i < $data->{'fin'}; ++$i) {
				my $reg = shift (@fstack) || 'null';
				unshift (@in, $reg);
			}
			for (my $i = 0; $i < $data->{'fout'}; ++$i) {
				my $reg = sprintf ('freg_%d', $freg_n++);
				unshift (@out, $reg);
				unshift (@stack, $reg);
			}
			$inst->{'in'} = \@in if @in;
			$inst->{'out'} = \@out if @out;
			$inst->{'fin'} = \@fin if @fin;
			$inst->{'fout'} = \@fout if @fout;
			$inst->{'wptr'} = $wptr;
			if ($data->{'wptr'}) {
				$wptr = sprintf ('wptr_%d', $wptr_n++);
				$inst->{'_wptr'} = $wptr;
			}
			if (0) {
				print "\t";
				print join (', ', @in, @fin), " => " if @in || @fin;
				print $name;
				if ($inst->{'label_arg'}) {
					print ' ', $inst->{'arg'}->{'name'};
				}
				print " => ", join (', ', @out, @fout) if @out || @fout;
				if ($data->{'wptr'}) {
					print " (", $inst->{'wptr'}, ' => ', $inst->{'_wptr'}, ")";
				}
				print "\n";
			}
			@stack = @stack[0..2] if @stack > 3;
			@fstack = @fstack[0..2] if @fstack > 3;
		}
		
		$label->{'out'} = [ @stack ];
		$label->{'fout'} = [ @fstack ];
		$label->{'_wptr'} = $wptr;
	}
}	

sub build_phi_nodes ($$) {
	my ($self, $labels) = @_;

	foreach my $label (@$labels) {
		my $lname = $label->{'name'};

		foreach my $inst (@{$label->{'inst'}}) {
			next if $inst->{'name'} ne 'CJ';
			my $tlabel = $inst->{'arg'};
			$tlabel->{'phi'} = {} if !$tlabel->{'phi'};
			$tlabel->{'phi'}->{$lname} = $inst->{'in'};
		}
	}
}

sub output_regs ($) {
	my $regs = shift;
	return if !$regs;
	my @out;
	foreach my $reg (@$regs) {
		push (@out, '%' . $reg);
	}
	print join (', ', @out);
}

sub generate_proc ($$) {
	my ($self, $proc) = @_;
	
	print 'define void @O_', $proc->{'symbol'}, 
		'(i32 *%', $proc->{'labels'}->[0]->{'wptr'}, ') {', "\n";
	foreach my $label (@{$proc->{'labels'}}) {
		my ($name, $insts) = ($label->{'name'}, $label->{'inst'});
		print $label->{'name'}, ":\n";
		if ($label->{'phi'}) {
			my $in = $label->{'in'} || [];
			my $fin = $label->{'fin'} || [];
			my @vars = (@$in, @$fin);
			my %var_map;
			my %type;
			foreach my $var (@$in) {
				$type{$var} = 'i32'; # FIXME: i32
			}
			foreach my $var (@$fin) {
				$type{$var} = 'double';
			}
			foreach my $slabel (keys (%{$label->{'phi'}})) {
				my @svars = $label->{'phi'}->{$slabel};
				for (my $i = 0; $i < @vars; ++$i) {
					$var_map{$vars[$i]} = '[ %' . $svars[$i] . ', %' . $slabel . ']';
				}
			}
			foreach my $var (@vars) {
				print 	"\t", '%', $var, 
					' = phi ', $type{$var}, ' ', join (' ', @{$var_map{$var}}),
					"\n";
			}
		}
		foreach my $inst (@$insts) {
			my $name 	= $inst->{'name'};
			next if $name =~ /^\./;
			
			my $data	= $GRAPH->{$name};
			my $in 		= $inst->{'in'} || [];
			my $fin 	= $inst->{'fin'} || [];
			my $out 	= $inst->{'out'} || [];
			my $fout 	= $inst->{'fout'} || [];
			
			if ($data->{'generator'}) {
				&{$data->{'generator'}}($proc, $label, $inst);
			} elsif (@$out + @$fout == 1) {
				print "\t";
				output_regs ($out);
				output_regs ($fout);
				print " = call op_", $name, " (%", $inst->{'wptr'};
				print ', ', $inst->{'arg'} if exists ($inst->{'arg'});
				print ', ' if (@$in + @$fin > 0);
				output_regs ($in);
				output_regs ($fin);
				print ")\n";
			} else {
				print "\t";
				print '%', $inst->{'_wptr'}, ' = ' if $inst->{'_wptr'};
				print "call op_", $name, " (%", $inst->{'wptr'};
				print ', ', $inst->{'arg'} if exists ($inst->{'arg'});
				print ', ' if (@$in + @$fin > 0);
				output_regs ($in);
				output_regs ($fin);
				output_regs ($out);
				output_regs ($fout);
				print ")\n";
			}
		}
	}
	print "}\n";
}

sub code_proc ($$) {
	my ($self, $proc) = @_;
	
	print "-- ", $proc->{'symbol'}, "\n";
	$self->define_registers ($proc->{'labels'});
	$self->build_phi_nodes ($proc->{'labels'});
	$self->generate_proc ($proc);
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
