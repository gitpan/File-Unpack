#! /usr/bin/perl -w
#
# file_unpack.pl -- Demo of File::Unpack features.
# 
# (C) 2010-2012, jnw@cpan.org, all rights reserved.
# Distribute under the same license as Perl itself.
#
# 2010-06-29, jw -- initial draught
# 2010-08-03, jw -- fixed -v.
# 2010-08-31, jw -- fixed -q with -m.
# 2010-09-01, jw -- added --list
# 2011-01-03, jw -- allow multiple arguments. Improved -m
# 2011-03-08, jw -- fixed usage of -l, added -p.
# 2011-04-21, jw -- better format error messages, and stop after error.
# 2011-05-12, jw -- added -n for no_op
# 2012-02-16, jw -- added -A for archive_name_as_dir

use Data::Dumper;
$Data::Dumper::Terse = 1;
$Data::Dumper::Indent = 1;
use Getopt::Long qw(:config no_ignore_case);
use Pod::Usage;
use FindBin;
use lib "$FindBin::RealBin/blib/lib";
use File::Unpack;

my $version = $File::Unpack::VERSION;
my @exclude;
my $exclude_vcs = 1;
my $help;
my $mime_only;
my $list_only;
my $list_perlish;
my @mime_helper_dirs;

my %opt = ( verbose => 1, maxfilesize => '2.6G', one_shot => 0, no_op => 0, world_readable => 0, log_fullpath => 0, archive_name_as_dir => 0);

push @mime_helper_dirs, "$FindBin::RealBin/helper" if -d "$FindBin::RealBin/helper";

GetOptions(
	"verbose|v+"   	=> \$opt{verbose},
	"version|V"    	=> sub { print "$version\n"; exit },
	"quiet"        	=> sub { $opt{verbose} = 0; },
	"destdir|D|C=s" => \$opt{destdir},
	"exclude|E=s"  	=> \@exclude,
	"exclude-vcs!" 	=> \$exclude_vcs,
	"vcs|include-vcs!" 	=> sub { $exclude_vcs = !$_[1]; },
	"help|?"       		=> \$help,
	"logfile|L=s"  		=> \$opt{logfile},
	"fullpath-log|F" 	=> \$opt{log_fullpath},
	"one-shot|one_shot|1"   => \$opt{one_shot},
	"mimetype|m+"  		=> \$mime_only,
	"no_op|no-op|noop|n+" 	=> \$opt{no_op},
	"list-helpers|l+" 	=> \$list_only,
	"print-helpers|p+" 	=> \$list_perlish,
	"params|P=s"		=> \%{$opt{log_params}},
	"maxfilesize=s"		=> \$opt{maxfilesize},
	"use-mime-helper-dir|I|u=s" 		=> \@mime_helper_dirs,
	"world-readable|world_readable|R+" 	=> \$opt{world_readable},
	"archive-dirs|archive_dirs|A"		=> \$opt{archive_name_as_dir},
) or $help++;

@mime_helper_dirs = split(/,/,join(',',@mime_helper_dirs));
my $archive = shift or $list_perlish or $list_only or $help++;

pod2usage(-verbose => 1, -msg => qq{
file_unpack V$version Usage: 

$0 [options] input.tar.gz ...
$0 [options] input/ ...
$0 -l
$0 -p

Valid options are:
 -v	Be more verbose. Default: $opt{verbose}.
 -q     Be quiet, not verbose.

 -A --archive_dirs
 	Use archive names as directories. Default: no directory for single files,
	truncated or modified archive names otherwise.

 -C dir
 -D --destdir dir
        Directory, where to place the output file or directory.
	A subdirectory is created, if there are more than one files to unpack.
	Default: current dir.

 -E --exclude glob.pat
 	Specify files and directories that are not unpacked.
	This option can be specified multiple times.

 -F --fullpath-log
 	Always use full path names in logfile. Default: 
	unpacked path names are written relative to destdir.

 --exclude-vcs	--no-exclude-vcs 
 --include-vcs  --no-include-vcs --vcs --no-vcs
 	Group switch for directory glob patterns of most version control systems.
	This affects at least SCCS, RCS, CVS, .svn, .git, .hg, .osc .
        Default: exclude-vcs=$exclude_vcs .

 -1 --one-shot
 	Make unpacker non-recursive. Perform one level of unpacking only.

 -h --help -?
        Print this online help.
 
 -L --logfile  file.log
 	Specify a logfile, where freshly unpacked files are reported.
	The format of the logfile is JSON; default is STDOUT.
 
 -l --list-helpers
        Overview of mime-type patterns and their helper commands.

 -p --print-helpers
 	List all builtin mime-helpers and all external mime-helpers as 
	a nested perl datastructure.

 -P --param KEY=VALUE
 	Place additional params into the log file.

 --maxfilesize size
        Truncate an unpacked file, if it gets larger than the specified size.
	Size can be specified as bytes (plain integer), kilo-, mega-, giga-, or 
	tera-bytes (suffix K,M,G,T). Default: $opt{maxfilesize}.

 -m --mimetype
        Do not unpack, just report mimetype of the archive. Output format is 
	similar to '/usr/bin/file -i', unless -q or -v are given.
	With -v, the unpacker command is also printed.

 -R --world-readable
 	Make the unpacked tree world readable. Default: user readable.

 -n --no-op
 	Do not unpack. Print the first unpack command only.

 -u --use-mime-helper-dir dir
 	Include an additonal directory of mime helpers.
	Useable multiple times. Later additions take precedence.

}) if $help;

$opt{logfile} ||= '/dev/null' if $list_only or $list_perlish or $mime_only or $opt{no_op};
my $u = File::Unpack->new(%opt);
my $list = $u->mime_helper_dir(@mime_helper_dirs);

if ($list_perlish)
  {
    print Dumper $list;
    exit 0;
  }

if ($list_only)
  {
    printf @$_ for $u->list();
    exit 0;
  }

if ($mime_only)
  {
    $u->{verbose}-- if $u->{verbose};
    while (defined $archive)
      {
	my $m = $u->mime($archive);
	my ($h,$r) = $u->find_mime_helper($m);
	if ($opt{verbose} > 1)
	  {
	    print "$archive: ", Dumper $m;
	    print File::Unpack::fmt_run_shellcmd($h) . "\n";
	  }
	elsif ($opt{verbose} == 1)
	  {
	    print "$archive: $m->[0]; charset=$m->[1]\n";
	  }
	else
	  {
	    print "$m->[0]\n";
	  }
        $archive = shift;
      }
    exit 0;
  }

while (defined $archive and !$u->{error}) 
  {
    $u->exclude(vcs => $exclude_vcs);
    $u->exclude(add => \@exclude) if @exclude;

    $u->unpack($archive);
    map { print STDERR "ERROR: $_\n" } @{$u->{error}} if $u->{error};
    $archive = shift;
    if (defined($archive))
      {
        if (defined $opt{logfile} and -f $opt{logfile})
	  {
            warn "File::Unpack($archive): overwriting previous logfile $opt{logfile} in 3 seconds. Press CTRL-C to abort.\n" if defined $archive;
	    sleep(3);
	  }
        # reload, for the next round. (new() opens the logfile, unpack() closese it.)
        $u = File::Unpack->new(%opt);
        $u->mime_helper_dir(@mime_helper_dirs);
      }
  }

# delete $u->{json};
# die "$0: " . Dumper $u;
