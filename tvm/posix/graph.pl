#!/usr/bin/perl

use strict;

my $event = 0;
my $root;
my %procs;
my %chans;

while (my $line = <STDIN>) {
	my ($id, $cmd)	= ($line =~ m/^(#[A-F0-9]+)\s+(.*)/);
	my $proc	= $procs{$id};

	$event++;

	if (!$proc) {
		$proc		= { 'id' => $id };
		$procs{$id} 	= $proc;
		$root 		= $proc if !$root;
	}

	if ($cmd =~ m/^@ (.*):(\d+)/) {
		my ($file, $line) = ($1, $2);
		$proc->{'file'} = $file;
		$proc->{'line'} = $line;
	} elsif ($cmd =~ m/^=> (#[A-F0-9]+)/) {
		my $new_id	= $1;
		delete ($procs{$id});
		if (!exists ($procs{$new_id})) {
			$procs{$new_id}	= $proc;
			$proc->{'id'}	= $new_id;
		} elsif ($proc->{'parent'}) {
			delete ($proc->{'parent'}->{'children'}->{$proc});
		}
	} elsif ($cmd =~ m/^start (#[A-F0-9]+)/) {
		my $new_id	= $1;
		my $new_proc	= {
			'id'		=> $new_id,
			'parent'	=> $proc
		};
		$proc->{'children'}	= {} if !exists ($proc->{'children'});
		$proc->{'children'}->{$new_proc} = $new_proc;
		$procs{$new_id}		= $new_proc;
	} elsif ($cmd =~ m/^fork (#[A-F0-9]+)/) {
		$procs{$1}	= { 'id' => $1 };
	} elsif ($cmd =~ m/^end/) {
		delete ($procs{$id});
		if ($proc->{'parent'}) {
			delete ($proc->{'parent'}->{'children'}->{$proc});
		}
	} elsif ($cmd =~ m/^call (.*)/) {
		my $symbol	= $1;
		$proc->{'callstack'} = [] if !exists ($proc->{'callstack'});
		push (@{$proc->{'callstack'}}, $symbol);
	} elsif ($cmd =~ m/^return/) {
		pop (@{$proc->{'callstack'}});
	} elsif ($cmd =~ m/^(input|output) (to|from) (#[A-F0-9]+)/) {
		my ($io, $tf, $cid)	= ($1, $2, $3);
		my $chan		= $chans{$cid};
		if (!$chan) {
			$chan 		= { 'id' => $cid };
			$chans{$cid} 	= $chan;
		}
		$chan->{$proc}	= $proc;
		$proc->{$io}	= {} if !exists ($proc->{$io});
		$proc->{$io}->{$cid} = $event;
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

	output_process ($root);
	print "---\n";
}

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
