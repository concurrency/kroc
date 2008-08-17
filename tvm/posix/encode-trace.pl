#!/usr/bin/perl

use strict;

# Constants
my @SRCPATH = ( '.' );

# State
my $next_id	= 1;
my %id_map;
my @id_cache;
my %procs;

my $next_cid	= 1;
my %cid_map;
my @cid_free;
my %chans;

my %cache;

# Output
my @ops;

while (my $line = <STDIN>) {
	my ($wp_id, $cmd)	= ($line =~ m/^(#[A-F0-9]+)\s+(.*)/);
	my $id;

	$wp_id = ptr_to_val ($wp_id);

	if (!exists ($id_map{$wp_id})) {
		$id	 	= $next_id++;
		$id_map{$wp_id}	= $id;
		@id_cache	= ();
		$procs{$id} 	= { 
			'id' 		=> $id, 
			'ws_top' 	=> $wp_id,
			'ws_bottom'	=> $wp_id
		};
		push (@ops, {
			'op'		=> 'newp',
			'target'	=> $id
		});
	} else {
		$id		= $id_map{$wp_id};
	}

	my $proc = $procs{$id};

	if ($proc && $proc->{'blocked_on'}) {
		my $chan = $proc->{'blocked_on'};
		push (@ops, {
			'op'		=> 'unblocked',
			'target'	=> $chan->{'parent'}->{'id'},
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
				'heading'	=> "$file",
				'lines'		=> \@lines
			});
		}
	} elsif ($cmd =~ m/^=> (#[A-F0-9]+)/) {
		my $new_wp_id	= ptr_to_val ($1);
		delete ($id_map{$wp_id});
		@id_cache	= ();
		if (!exists ($id_map{$new_wp_id})) {
			$id_map{$new_wp_id}	= $id;
			$proc->{'ws_bottom'}	= $new_wp_id;
		} elsif ($proc->{'parent'}) {
			# this is a merge ?
			delete ($procs{$id});
			push (@ops, {
				'op'		=> 'endp',
				'target'	=> $id
			});
		}
	} elsif ($cmd =~ m/^start (#[A-F0-9]+)/) {
		my $new_wp_id	= ptr_to_val ($1);
		my $new_id	= $next_id++;
		my $new_proc	= {
			'id'		=> $new_id,
			'parent'	=> $proc,
			'ws_top'	=> $new_wp_id,
			'ws_bottom'	=> $new_wp_id
		};
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
		push (@ops, {
			'op'		=> 'endp',
			'target'	=> $id
		});
	} elsif ($cmd =~ m/^call (.*)/) {
		my $symbol	= $1;
		$proc->{'callstack'} = [] if !exists ($proc->{'callstack'});
		my $stack	= $proc->{'callstack'};
		push (@$stack, $symbol);
		push (@ops, {
			'op'		=> 'symbol',
			'target'	=> $id,
			'symbol'	=> join (' / ', @$stack)
		});
	} elsif ($cmd =~ m/^return/) {
		my $stack = $proc->{'callstack'};
		pop (@$stack);
		push (@ops, {
			'op'		=> 'symbol',
			'target'	=> $id,
			'symbol'	=> join (' / ', @$stack)
		});
	} elsif ($cmd =~ m/^(input|output|en|dis)( to| from |able) (#[A-F0-9]+)/) {
		my ($io, $tf, $cp_id)	= ($1, $2, ptr_to_val ($3));
		my ($cid, $chan);
		if (!exists ($cid_map{$cp_id})) {
			$cid		= (@cid_free ? shift (@cid_free) : $next_cid++);
			$chan 		= { 'id' => $cid };
			$cid_map{$cp_id}= $cid;
			$chans{$cid} 	= $chan;
			@id_cache	= sort (keys (%id_map));
			
			# FIXME: use binary search
			my $proc;
			for (my $i = 0; $id_cache[$i] < $cp_id && $i < @id_cache; ++$i) {
				$proc = $i;
			}
			$proc = $procs{$id_map{$id_cache[$proc]}};
			if ($proc->{'ws_bottom'} > $cp_id || $proc->{'ws_top'} < $cp_id) {
				# Create a fake parent
				my $id	 	= $next_id++;
				$id_map{$cp_id}	= $id;
				$proc		= { 
					'id' 		=> $id, 
					'ws_top' 	=> $cp_id + 4, # XXX: bad constant
					'ws_bottom'	=> $cp_id
				};
				$procs{$id}	= $proc;
				push (@ops, {
					'op'		=> 'newp',
					'target'	=> $id
				});
			}
			$chan = {
				'id'		=> $cid,
				'cp_id'		=> $cp_id,
				'parent'	=> $proc
			};
			$chans{$cid} 	= $chan;
			push (@ops, {
				'op'		=> 'addc',
				'target'	=> $proc->{'id'},
				'channel'	=> $cid
			});
		} else {
			$cid			= $cid_map{$cp_id};
			$chan			= $chans{$cid};
		}
		if ($io =~ /^(en|dis)$/) {
			$io			.= 'able';
		} else {
			$chan->{$io}		= $proc;
			$proc->{'blocked_on'}	= $chan;
		}
		push (@ops, {
			'op'		=>	$io,
			'target'	=>	$id,
			'channel'	=>	$cid
		});
	} elsif ($cmd =~ m/^release (#[A-F0-9]+) (#[A-F0-9]+)/) {
		my ($start, $end) = (ptr_to_val ($1), ptr_to_val ($2));
		foreach my $cp_id (keys (%cid_map)) {
			next if $cp_id < $start || $cp_id >= $end;
			
			my $cid	= $cid_map{$cp_id};
			my $chan = $chans{$cid};
			delete ($cid_map{$cp_id});
			delete ($chans{$cid});
			push (@cid_free, $cid);
			
			push (@ops, {
				'op'		=>	'deletec',
				'target'	=>	$chan->{'parent'}->{'id'},
				'channel'	=>	$cid
			});
		}
	} elsif ($cmd =~ /^wait$/) {
		push (@ops, {
			'op'		=> 'wait',
			'target'	=> $id,
		});
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
		delete ($op->{'free_ids'});
	}
}

# Encode output
print <<END;
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist SYSTEM "file://localhost/System/Library/DTDs/PropertyList.dtd">
<plist version="1.0">
END
plist_encode (\@ops);
print <<END;
</plist>
END

exit 0;

sub ptr_to_val($) {
	return hex (($_[0] =~ m/#?([A-F0-9]+)/i)[0]);
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
			$lines->[$n - 2],
			$lines->[$n - 1],
			$lines->[$n]
		);
	} elsif ($n <= 1) {
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

sub plist_encode ($) {
	my ($val, $indent) = @_;
	if (ref ($val) =~ /ARRAY/) {
		print $indent, "<array>\n";
		foreach my $element (@$val) {
			plist_encode ($element, "$indent    ");
		}
		print $indent, "</array>\n";
	} elsif (ref ($val) =~ /HASH/) {
		print $indent, "<dict>\n";
		foreach my $key (keys (%$val)) {
			print $indent, "    ", "<key>$key</key>\n";
			plist_encode ($val->{$key}, "$indent    ");
		}
		print $indent, "</dict>\n";
	} elsif ($val =~ /^[0-9]+$/) {
		print $indent, "<integer>$val</integer>\n";
	} else {
		# FIXME: encode string special characters
		print $indent, "<string>$val</string>\n";
	}
}
