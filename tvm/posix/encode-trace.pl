#!/usr/bin/perl

use strict;

# Constants
my @SRCPATH = ( '.' );

# State
my $next_id = 1;
my %id_map;
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
		$procs{$id} 	= { 'id' => $id };
		push (@ops, {
			'op'		=> 'new',
			'target'	=> $id
		});
	} else {
		$id		= $id_map{$wp_id};
	}

	my $proc		= $procs{$id};

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
		if (!exists ($id_map{$new_wp_id})) {
			$id_map{$new_wp_id} = $id;
		} elsif ($proc->{'parent'}) {
			# this is a merge ?
			delete ($procs{$id});
			delete ($proc->{'parent'}->{'children'}->{$proc});
			push (@ops, {
				'op'		=> 'delete',
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
		$procs{$new_id}		= $new_proc;
		push (@ops, {
			'op'		=> 'start',
			'target'	=> $new_id,
			'parent'	=> $id
		});
	} elsif ($cmd =~ m/^fork (#[A-F0-9]+)/) {
		my $new_wp_id		= $1;
		my $new_id		= $next_id++;
		my $new_proc		= { 'id' => $new_id };
		$id_map{$new_wp_id}	= $new_id;
		$procs{$new_id}		= $new_proc;
		push (@ops, {
			'op'		=> 'new',
			'target'	=> $new_id
		});
	} elsif ($cmd =~ m/^end/) {
		delete ($id_map{$wp_id});
		delete ($procs{$id});
		if ($proc->{'parent'}) {
			delete ($proc->{'parent'}->{'children'}->{$proc});
		}
		push (@ops, {
			'op'		=> 'end',
			'target'	=> $id
		});
	} elsif ($cmd =~ m/^call (.*)/) {
		my $symbol	= $1;
		$proc->{'callstack'} = [] if !exists ($proc->{'callstack'});
		push (@{$proc->{'callstack'}}, $symbol);
		push (@ops, {
			'op'		=> 'title',
			'target'	=> $id,
			'title'		=> $symbol
		});
	} elsif ($cmd =~ m/^return/) {
		my $stack = $proc->{'callstack'};
		pop (@$stack);
		my $symbol = $stack->[@$stack - 1];
		push (@ops, {
			'op'		=> 'title',
			'target'	=> $id,
			'title'		=> $symbol
		});
	} elsif ($cmd =~ m/^(input|output) (to|from) (#[A-F0-9]+)/) {
		my ($io, $tf, $cid)	= ($1, $2, $3);
		my $chan		= $chans{$cid};
		if (!$chan) {
			$chan 		= { 'id' => $cid };
			$chans{$cid} 	= $chan;
		}
		$chan->{$proc}	= $proc;
		$proc->{$io}	= {} if !exists ($proc->{$io});
		$proc->{$io}->{$cid} = scalar (@ops);
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
			
			foreach my $id (keys (%$chan)) {
				next if $id !~ /^#/;
				my $proc = $chan->{$id};
				delete ($proc->{'input'}->{$cid})
					if exists ($proc->{'input'});
				delete ($proc->{'output'}->{$cid})
					if exists ($proc->{'output'});
			}
		}
	}
}

foreach my $op (@ops) {
	print $op->{'op'}, " ", $op->{'target'}, "\n";
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
