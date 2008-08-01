#!/usr/bin/perl

use strict;

# Constants
my @SRCPATH = ( '.' );

# State
my $next_id = 1;
my %id_map;
my @id_cache;
my %procs;
my %chans;
my %cache;

# Output
my @ops;

while (my $line = <STDIN>) {
	my ($wp_id, $cmd)	= ($line =~ m/^(#[A-F0-9]+)\s+(.*)/);
	my $id;

	if (!exists ($id_map{$wp_id})) {
		$id	 	= $next_id++;
		$id_map{$wp_id}	= $id;
		@id_cache	= ();
		$procs{$id} 	= { 'id' => $id };
		push (@ops, {
			'op'		=> 'newp',
			'target'	=> $id
		});
	} else {
		$id		= $id_map{$wp_id};
	}

	my $proc		= $procs{$id};

	if ($proc && $proc->{'blocked_on'}) {
		my $chan = $proc->{'blocked_on'};
		push (@ops, {
			'op'		=> 'read',
			'target'	=> $chan->{'input'}->{'id'},
			'channel'	=> $chan->{'id'}
		});
		push (@ops, {
			'op'		=> 'wrote',
			'target'	=> $chan->{'output'}->{'id'},
			'channel'	=> $chan->{'id'}
		});
		$chan->{'input'}->{'blocked_on'} = undef;
		$chan->{'output'}->{'blocked_on'} = undef;
	}

	if ($cmd =~ m/^@ (.*):(\d+)/) {
		my ($file, $line) = ($1, $2);
		if (($file ne $proc->{'file'}) || ($line != $proc->{'line'})) {
			$proc->{'file'} = $file;
			$proc->{'line'} = $line;
			my $lines = load_file (\%cache, $file);
			my @lines = pick_lines ($lines, $line);
			push (@ops, { 
				'op' 		=> 'position',
				'target' 	=> $id, 
				'heading'	=> "$file:$line",
				'lines'		=> \@lines
			});
		}
	} elsif ($cmd =~ m/^=> (#[A-F0-9]+)/) {
		my $new_wp_id	= $1;
		delete ($id_map{$wp_id});
		@id_cache	= ();
		if (!exists ($id_map{$new_wp_id})) {
			$id_map{$new_wp_id}	= $id;
		} elsif ($proc->{'parent'}) {
			# this is a merge ?
			delete ($procs{$id});
			delete ($proc->{'parent'}->{'children'}->{$proc});
			push (@ops, {
				'op'		=> 'endp',
				'target'	=> $id
			});
		}
	} elsif ($cmd =~ m/^start (#[A-F0-9]+)/) {
		my $new_wp_id	= $1;
		my $new_id	= $next_id++;
		my $new_proc	= {
			'id'		=> $new_id,
			'parent'	=> $proc
		};
		$proc->{'children'}	= {} if !exists ($proc->{'children'});
		$proc->{'children'}->{$new_proc} = $new_proc;
		$id_map{$new_wp_id}	= $new_id;
		@id_cache		= ();
		$procs{$new_id}		= $new_proc;
		push (@ops, {
			'op'		=> 'startp',
			'target'	=> $new_id,
			'parent'	=> $id
		});
	} elsif ($cmd =~ m/^fork (#[A-F0-9]+)/) {
		my $new_wp_id		= $1;
		my $new_id		= $next_id++;
		my $new_proc		= { 'id' => $new_id };
		$id_map{$new_wp_id}	= $new_id;
		$procs{$new_id}		= $new_proc;
		@id_cache		= ();
		push (@ops, {
			'op'		=> 'newp',
			'target'	=> $new_id
		});
	} elsif ($cmd =~ m/^end/) {
		delete ($id_map{$wp_id});
		@id_cache = ();
		delete ($procs{$id});
		if ($proc->{'parent'}) {
			delete ($proc->{'parent'}->{'children'}->{$proc});
		}
		push (@ops, {
			'op'		=> 'endp',
			'target'	=> $id
		});
	} elsif ($cmd =~ m/^call (.*)/) {
		my $symbol	= $1;
		$proc->{'callstack'} = [] if !exists ($proc->{'callstack'});
		push (@{$proc->{'callstack'}}, $symbol);
		push (@ops, {
			'op'		=> 'symbol',
			'target'	=> $id,
			'symbol'	=> $symbol
		});
	} elsif ($cmd =~ m/^return/) {
		my $stack = $proc->{'callstack'};
		pop (@$stack);
		my $symbol = $stack->[@$stack - 1];
		push (@ops, {
			'op'		=> 'symbol',
			'target'	=> $id,
			'symbol'	=> $symbol
		});
	} elsif ($cmd =~ m/^(input|output) (to|from) #([A-F0-9]+)/) {
		my ($io, $tf, $cid)	= ($1, $2, $3);
		my $chan;
		$cid			= hex ($cid);
		if (!exists ($chans{$cid})) {
			$chan 		= { 'id' => $cid };
			$chans{$cid} 	= $chan;
			if (!@id_cache) {
				@id_cache = sort {
					my $ma = hex (($a =~ m/#(.*)/)[0]);
					my $mb = hex (($b =~ m/#(.*)/)[0]);
					return hex ($ma) <=> hex ($mb);
				} (keys (%id_map));
			
				foreach my $id (@id_cache) {
					$id =~ s/^#//;
					$id = hex ($id);
				}
			}
			
			# FIXME: use binary search
			my $proc;
			for (my $i = 0; $id_cache[$i] < $cid && $i < @id_cache; ++$i) {
				$proc = $i;
			}
			$proc = sprintf ('#%08X', $id_cache[$proc]);
			$proc = $id_map{$proc};
			$chan = { 
				'id'		=> $cid,
				'parent'	=> $proc
			};
			$chans{$cid} 	= $chan;
			push (@ops, {
				'op'		=> 'addc',
				'target'	=> $proc,
				'channel'	=> $cid
			});
		} else {
			$chan		= $chans{$cid};
		}
		$chan->{$io}		= $proc;
		$proc->{'blocked_on'}	= $chan;
		push (@ops, {
			'op'		=>	$io,
			'target'	=>	$id,
			'channel'	=>	$cid
		});
	} elsif ($cmd =~ m/^release (#[A-F0-9]+) (#[A-F0-9]+)/) {
		my ($start, $end) = ($1, $2);
		$start	=~ s/^#//;
		$start	= hex ($start);
		$end	=~ s/^#//;
		$end 	= hex ($end);
		foreach my $cid (keys (%chans)) {
			my $pos = hex (($cid =~ /#(.*)/)[0]);
			
			next if $pos < $start || $pos >= $end;
			
			my $chan = $chans{$cid};
			delete ($chans{$cid});
			
			push (@ops, {
				'op'		=>	'deletec',
				'target'	=>	$chan->{'parent'},
				'channel'	=>	$cid
			});
		}
	}
}

# Minimise number of process IDs
my @free;
$next_id = 1;
%id_map = ();
for (my $i = 0; $i < @ops; ++$i) {
	my $op = $ops[$i];
	$id_map{$op->{'target'}} = $i;
	$id_map{$op->{'parent'}} = $i if exists ($op->{'parent'});
}
foreach my $id (keys (%id_map)) {
	my $pos = $id_map{$id};
	my $op = $ops[$pos];
	$op->{'free_ids'} = [] if !exists ($op->{'free_ids'});
	push (@{$op->{'free_ids'}}, $id);
}
for (my $i = 0; $i < @ops; ++$i) {
	my $op		= $ops[$i];
	if ($op->{'op'} =~ m/^startp|newp$/) {
		my $id;
		if (@free) {
			$id = shift (@free);
		} else {
			$id = $next_id++;
		}
		$id_map{$op->{'target'}} = $id;
	}
	$op->{'target'} = $id_map{$op->{'target'}}
		if exists ($op->{'target'});
	$op->{'parent'} = $id_map{$op->{'parent'}}
		if exists ($op->{'parent'});
	if (exists ($op->{'free_ids'})) {
		foreach my $id (@{$op->{'free_ids'}}) {
			push (@free, $id_map{$id});
		}
	}
}


# Pretty print
foreach my $op (@ops) {
	print $op->{'op'}, " ", $op->{'target'}, "\n";
	if ($op->{'op'} eq 'position') {
		print "\t", $op->{'heading'}, "\n";
		foreach my $line (@{$op->{'lines'}}) {
			print "\t\t", $line, "\n";
		}
	} elsif ($op->{'op'} eq 'startp') {
		print "\tparent = ", $op->{'parent'}, "\n";
	} elsif ($op->{'op'} eq 'symbol') {
		print "\t", $op->{'symbol'}, "\n";
	} elsif ($op->{'op'} =~ /^(addc|deletec|input|output|read|wrote)$/) {
		print "\tchannel = ", $op->{'channel'}, "\n";
	}
}

exit 0;

sub output_process {
	my ($proc, $indent) = @_;
	print 	$indent, $proc->{'id'}, " ", $proc->{'file'}, " ", $proc->{'line'}, "\n";
	if (exists ($proc->{'callstack'})) {
		print $indent, " (", join (', ', @{$proc->{'callstack'}}), ")\n";
	}
	if (exists ($proc->{'input'})) {
		print $indent, " ? ", join (', ', keys (%{$proc->{'input'}})), "\n";
	}
	if (exists ($proc->{'output'})) {
		print $indent, " ! ", join (', ', keys (%{$proc->{'output'}})), "\n";
	}
	if (exists ($proc->{'children'})) {
		foreach my $key (sort (keys (%{$proc->{'children'}}))) {
			my $proc = $proc->{'children'}->{$key};
			output_process ($proc, "$indent\t");
		}
	}
}

sub load_file ($$) {
	my ($cache, $fn) = @_;
	return $cache->{$fn} if exists ($cache->{$fn});
	foreach my $path (@SRCPATH) {
		next if !stat ("$path/$fn");
		my ($fh, @lines);
		open ($fh, "$path/$fn") || die "can't open $path/$fn: $!";
		while (my $line = <$fh>) {
			$line =~ s/\r?\n$//s;
			push (@lines, $line);
		}
		close ($fh);
		$cache->{$fn} = \@lines;
		return \@lines;
	}
}

sub pick_lines ($$) {
	my ($lines, $n) = @_;
	my (@l) = ('...', '...', '...');

	# Line number data from 1
	$n -= 1;

	# Pick lines
	if ($lines) {
		@l = (
			$lines->[$n - 1],
			$lines->[$n],
			$lines->[$n + 1]
		);
	} elsif ($n == 0) {
		$l[0] = undef;
	}

	# Reduce leading whitespace to bare minimum
	foreach my $l (@l) {
		next if !defined ($l);
		$l =~ s/\t/        /g;
	}
	my ($shift, $done);
	do {
		foreach my $l (@l) {
			next if !defined ($l);
			$done = 1 if $l !~ /^ /;
		}
		if (!$done) {
			$shift++;
			foreach my $l (@l) {
				next if !defined ($l);
				$l =~ s/^ //;
			}
		}
	} while (!$done);

	# Add line numbers
	for (my $i = 0; $i < 3; ++$i) {
		my $offset = $i - 1;
		$l[$i] = sprintf ('%d: %s', $n + $offset, $l[$i]) if defined ($l[$i]);
	}

	return @l;
}
