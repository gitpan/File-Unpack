#!perl -T

use strict;
use warnings;
use Test::More;

#unless ( $ENV{RELEASE_TESTING} ) {
#    plan( skip_all => "ok" );
#}

eval "use Test::CheckManifest 0.9";
plan skip_all => "Test::CheckManifest 0.9 required" if $@;
ok_manifest();
