#!perl -T

use Test::More;
use FindBin;
BEGIN { unshift @INC, "$1/../blib/lib" if $FindBin::Bin =~ m{(.*)} };
use File::Unpack;
use File::Temp;
use JSON;

plan tests => 4;

my $testdir = File::Temp::tempdir("FU_06_XXXXX", TMPDIR => 1, CLEANUP => 1);

my $u = File::Unpack->new(destdir => $testdir, verbose => 0, logfile => "$testdir/log");
$u->exclude(vcs => 1, add => ['data']);
$u->unpack("t");
ok(-f "$testdir/log", "have $testdir/log after unpacking");
open IN, "<", "$testdir/log";
my $log = JSON::from_json(join '', <IN>);
close IN;
ok(ref($log) eq 'HASH', "logfile is valid JSON");
ok(!exists($log->{unpacked}{'/'}), "Dummy not file seen");
ok(length($log->{end}), "end timstamp file seen");
