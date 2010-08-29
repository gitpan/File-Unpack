#!perl -T

use Test::More;
use FindBin;
BEGIN { unshift @INC, "$1/../blib/lib" if $FindBin::Bin =~ m{(.*)} };
use File::Unpack;
use File::Temp;

plan tests => 4;

my $testdir = File::Temp::tempdir("FU_04_XXXXX", TMPDIR => 1, CLEANUP => 1);
my $u = File::Unpack->new(destdir => $testdir, verbose => 0, logfile => '/dev/null');
$u->exclude(vcs => 1, add => ['*.t']);
ok(-d "t/data", "have t/data before unpacking. test is useless without");
ok(-f "t/04-subdir.t", "have t/04-subdir.t before unpacking. test is useless without");
$u->unpack("t");
ok(-d "$testdir/data", "have $testdir/data after unpacking");
ok(!-f "$testdir/04-subdir.t", "have ignored *.t after unpacking");


