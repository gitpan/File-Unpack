#!perl -T

use strict;
use warnings;
use Test::More;

#unless ( $ENV{RELEASE_TESTING} ) {
#    plan( skip_all => "ok" );
#}

eval "use Test::CheckManifest 0.9;";
ok_manifest({filter => [ qr/\.svn/, qr/\.(sw.|files|orig|bak|old|tmp|tar\.bz2)$/]});

# done_testing does not exist on 11.1
#done_testing();
