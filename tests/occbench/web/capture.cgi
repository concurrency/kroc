#!/usr/bin/perl

use CGI qw(:standard);
use DBI;
use strict;

my $ALLOW_LIST = 'allow.cfg';
my $DB_CONFIG = 'db.cfg';

my $page = new CGI;
my @params = $page->param;

my %allowed;
my ($db_dsn, $db_user, $db_pass);
my $fh;

# Allow list
open ($fh, $ALLOW_LIST) || die $!;
while (my $line = <$fh>) {
	chomp ($line);
	$allowed{$line} = 1;
}
close ($fh);

if (!exists ($allowed{$ENV{'REMOTE_ADDR'}})) {
	print "Denied: ", $ENV{'REMOTE_ADDR'}, "\n";
	exit 0;
}

# DB Setup
open ($fh, $DB_CONFIG) || die $!;
$db_dsn = <$fh>; chomp ($db_dsn);
$db_user = <$fh>; chomp ($db_user);
$db_pass = <$fh>; chomp ($db_pass);
close ($fh);

my $dbh = DBI->connect (
	$db_dsn, $db_user, $db_pass,
	{ RaiseError => 0, AutoCommit => 0 }
);
if (!$dbh) {
	print "No DB\n";
	exit 0;
}

# Capture Data
my $host 	= $page->param ('host');
my $rev 	= $page->param ('revision'); $rev =~ s/^r//;
my $start 	= $page->param ('start');
my $name	= $page->param ('name');
my $type	= $page->param ('type');
my $cpus	= $page->param ('cpus');
my $avg		= $page->param ('avg');

my $sth = $dbh->prepare (
	'INSERT INTO result (host, rev, start, name, type, cpus, avg) VALUES (?, ?, ?, ?, ?, ?, ?)'
);
my $rv = $sth->execute ($host, $rev, $start, $name, $type, $cpus, $avg);

if ($rv) {
	print "OK\n";
} else {
	print "ERROR\n";
}

$dbh->disconnect ();

exit 0;

