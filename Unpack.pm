#
# (C) 2010, jnw@cpan.org, all rights reserved.
# Distribute under the same license as Perl itself.
#
#
# sudo zypper -v in perl-Compress-Raw-Zlib
#  -> 'nothing to do'
# sudo zypper -v in 'perl-Compress-Raw-Zlib >= 2.027'
#  -> 'perl' providing 'perl-Compress-Raw-Zlib >= 2.027' is already installed.
# sudo zypper -v in --force perl-Compress-Raw-Zlib
#  -> works, 
# sudo zypper -v in --from 12 perl-Compress-Raw-Zlib
#  -> works, if d.l.p is repo #12.
#
# TODO: 
# evaluate File::Extract - Extract Text From Arbitrary File Types 
#       (HTML, PDF, Plain, RTF, Excel)
# call _run_mime_handler() with 'correct' destdir in cups-1.2.4/*/cups.jar
#	harmless.

package File::Unpack;

BEGIN
{
 eval 'use File::LibMagic;';		# only needed in mime(); mime() dies, if missing
 eval 'use File::MimeInfo::Magic;';	# only needed in mime(); okay, if missing.
 eval 'use Compress::Raw::Lzma;';	# only needed in mime(); for finding lzma.
 eval 'use Compress::Raw::Bzip2;';	# only needed in mime(); for finding second level types
 eval 'use Compress::Raw::Zlib;';	# only needed in mime(); for finding second level types
 eval 'use BSD::Resource;';		# setrlimit
 eval 'use Filesys::Statvfs;';		# statvfs();
}

use warnings;
use strict;
use Carp;
use File::Path;
use File::Temp;			# tempdir() in _run_mime_handler.
use File::Copy ();
use JSON;
use String::ShellQuote;		# used in _prep_configdir 
use IPC::Run;			# implements File::Unpack::run()
use Text::Sprintf::Named;	# used to parse @builtin_mime_handlers
use Cwd 'getcwd';		# run(), moves us there and back. 
use Data::Dumper;

=head1 NAME

File::Unpack - An aggressive archive file unpacker, based on mime-types

=head1 VERSION

Version 0.20

=cut

our $VERSION = '0.20';

$ENV{PATH} = '/usr/bin:/bin';
$ENV{SHELL} = '/bin/sh';
delete $ENV{ENV};

# Compress::Raw::Bunzip2 needs several 100k of input data, we special case this.
# Anything else works with 1024.
my $UNCOMP_BUFSZ = 1024;

my @builtin_mime_handlers = (
  # mimetype pattern          # suffix pattern      # command with redirects, as defined with IPC::Run::run
  [ 'application=%lzma',      qr{\.(xz|lz(ma)?)$}i, [qw(/usr/bin/lzcat)],        qw(< %(src)s > %(destfile)s) ],
  [ 'application=%lzma',      qr{\.(xz|lz(ma)?)$}i, [qw(/usr/bin/xz      -dc -f %(src)s)], qw(> %(destfile)s) ],
  [ 'application=%bzip2',     qr{\.bz2$}i,          [qw(/usr/bin/bunzip2 -dc -f %(src)s)], qw(> %(destfile)s) ],
  [ 'application=%gzip',      qr{\.gz$}i,           [qw(/usr/bin/gzip -dc -f %(src)s)], qw(> %(destfile)s) ],

  # xml.summary.Mono.Security.Authenticode is twice inside of monodoc-1.0.4.tar.gz/Mono.zip/ -> use -o
  [ 'application=zip',        qr{\.(zip|jar|sar)$}i,  [qw(/usr/bin/unzip -P no_pw -q -o %(src)s)] ],
  [ 'application=%zip',       qr{\.(zip|jar|sar)$}i,  [qw(/usr/bin/unzip -P no_pw -q -o %(src)s)] ],

  [ 'application=%tar',       qr{\.(tar|gem)$}i,      [\&_locate_tar,  qw(-xf %(src)s)] ],
  [ 'application=%tar+bzip2', qr{\.tar\.bz2$}i,       [\&_locate_tar, qw(-jxf %(src)s)] ],
  [ 'application=%tar+gzip',  qr{\.t(ar\.gz|gz)$}i,   [\&_locate_tar, qw(-zxf %(src)s)] ],
  [ 'application=%rpm',       qr{\.(src\.r|s|r)pm$}i, [qw(/usr/bin/rpm2cpio %(src)s)], '|', [\&_locate_cpio_i] ],
);

sub _locate_tar
{
  my $self = shift;
  return @{$self->{_locate_tar}} if defined $self->{_locate_tar};

  # cannot use tar -C %(destdir)s,  we rely on being chdir'ed inside already :-)
  # E: /bin/tar: /tmp/xxx/_VASn/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_: Cannot chdir: Permission denied

  my @tar = (-f '/bin/tar' ? '/bin/tar' : '/usr/bin/tar' );
  ## osc co loves to create directories with : in them. 
  ## Tell tar to accept such directories as directores.
  push @tar, "--force-local" 
    unless $self->run([@tar, "--force-local", "--help"], { out_err => '/dev/null' });
  push @tar, "--no-unquote"  
    unless $self->run([@tar, "--no-unquote", "--help"],  { out_err => '/dev/null'});

  $self->{_locate_tar} = \@tar;
  return @tar;
}

sub _locate_cpio_i
{
  my $self = shift;
  return @{$self->{_locate_cpio_i}} if defined $self->{_locate_cpio_i};

  my @cpio_i = ('/usr/bin/cpio', '-idm');
  $cpio_i[1] .= 'u' 
    unless run(['/usr/bin/cpio', '-idmu', '--usage'], {out_err => '/dev/null'});
  push @cpio_i, '--sparse'
    unless run([@cpio_i, '--sparse', '--usage'], {out_err => '/dev/null'});
  push @cpio_i, '--no-absolute-filenames'
    unless run([@cpio_i, '--no-absolute-filenames', '--usage'], {out_err => '/dev/null'});

  @{$self->{_locate_cpio_i}} = \@cpio_i;
  return @cpio_i;
}

=head1 SYNOPSIS

File::Unpack is an aggressive unpacker for archive files. We call it aggressive, 
because it recursivly descends into any freshly unpacked file, if it appears to be an archive itself.
It also uncompresses files where needed. The ultimate goal of File::Unpack is
to extract as much readable text (ascii or any other encoding) as possible.
Most of the currently known archive file formats are supported.

    use File::Unpack;

    my $log;
    my $u = File::Unpack->new(logfile => \$log);

    my $m = $u->mime('/etc/init.d/rc');
    print "$m->[0]; charset=$m->[1]\n"
    # text/x-shellscript; charset=us-ascii

    map { print "$_->{name}\n" } @{$u->mime_handler()};
    # application/%rpm
    # application/%tar+gzip
    # application/%tar+bzip2
    # ...

    $u->unpack("inputfile.tar.bz2");
    while ($log =~ m{^\s*"(.*?)":}g) # it's JSON.
      {
        print "$1\n"; 	# report all files unpacked
      }

    ...

Examines the contents of an archive file or directory by extensive mime-type
analysis. The contents is unpacked recursively to the given destination
directory; a listing of the unpacked files is reported through the built in
logging facility during unpacking. The mime-type handlers are customizable, as
well as exclude patterns.

=head1 SUBROUTINES/METHODS

=head2 new

my $u = new(destdir => '.', logfile => \*STDOUT, maxfilesize => '100M', verbose => 1);

Creates an unpacker instance. Destdir must be writable; all output files and directories 
are placed inside destdir. Subdirectories will be created in an attempt to reflect the 
structure of the input. Destdir defaults to the current directory; relative paths 
are resolved immediatly, so that chdir() after calling new is harmless.

The parameter logfile can be a reference to a scalar, a filename, or a filedescriptor.
The logfile starts with a JSON formatted prolog, where all lines start 
with printable characters.
For each file unpacked, a one line record is appended, started with a single 
whitespace ' ', and terminated by "\n".
Each record is formatted as a JSON " key: value\n" pair, where key is the filename, and value a hash including mime, size, and other information.
The logfile is terminated by an epilog, where each line starts with a printable character.
Per default, the logfile is sent to STDOUT. 

The parameter maxfilesize is a safeguard against compressed sparse files. Such files could 
easily fill up any available disk space when unpacked. Files hitting this limit will 
be silently truncated.  Check the logfile records or epilog to see if this has happened.
BSD::Resource is used manipulate RLIMIT_FSIZE.

=head2 exclude

exclude(add => ['.svn', '*.orig' ], del => '.svn', force => 1)

Defines the exclude-list for unpacking. This list is advisory for the mime-handlers. 
The exclude-list items are shell glob patterns, where '*' or '?' never match '/'.

You can use force to have any of these removed after unpacking.
Use (vcs => 1) to exclude a long list of known version control system directories, use (vcs => 0) to remove them.
The default is C<exclude(empty => 1)>, which is the same as (empty_file => 1, empty_dir => 1) -- having the obvious meaning.

(re => 1) returns the active exclude-list as a regexp pattern. 
Otherwise C<exclude> always returns the list as an array ref.

=cut

sub _glob_list_re
{
  my @re;
  return undef unless @_;
  for my $text (@_)
    {
      # Taken from pdb2perl:glob2re() and adapted, to not match slashes in wildcards.
      # This should be kept compatible with tar --exclude .

      $text =~ s{([\.\(\)\[\]\{\}])}{\\$1}g; ## protect magic re characters.
      $text =~ s{\*}{[^/]*}g;                  ## * -> [^/]*
      $text =~ s{\?}{[^/]}g;                   ## ? -> [^/]
      push @re, $text;
    }
  return '(/|^)(' . join('|', @re) . ')(/|$)';
}

sub _not_excluded
{
  my $self = shift;
  my ($dir, $file) = @_;

  return 1 unless my $re = $self->{exclude}{re};

  $dir ||= '';
  $dir .= '/' unless $dir =~ m{/$};
  $file = $dir . $file;

  return 0 if $file =~ m{$re};
  return 1; 
}

sub exclude 
{
  my $self = shift;
  my %opt = $#_ ? @_ : (add => $_[0]);
  
  # ADD to this list from: https://build.opensuse.org/project/show?project=devel%3Atools%3Ascm
  my @vcs = qw(SCCS RCS CVS .svn .git .hg .osc);

  $opt{add} = [ $opt{add} ] unless ref $opt{add};
  $opt{del} = [ $opt{del} ] unless ref $opt{del};

  push @{$opt{add}}, @vcs if defined $opt{vcs} and $opt{vcs};
  push @{$opt{del}}, @vcs if defined $opt{vcs} and !$opt{vcs};

  for my $a (@{$opt{add}})
    {
      $self->{exclude}{list}{$a}++ if defined $a;
    }
  
  for my $a (@{$opt{del}})
    {
      delete $self->{exclude}{list}{$a} if defined $a;
    }

  my @list = sort keys %{$self->{exclude}{list}};
  $self->{exclude}{re} = _glob_list_re(@list);

  $opt{empty_dir} = $opt{empty_file} = $opt{empty} if defined $opt{empty};

  for my $o qw(empty_file empty_dir force)
    {
      $self->{exclude}{$o} = $opt{$o} if defined $opt{$o};
    }

  return $opt{re} ? $self->{exclude}{re} : \@list;
}

=begin private 

=item log, logf

The C<log> method is used by C<unpack> to send text to the logfile.
The C<logf> method takes a filename and a hash, and logs a JSON formatted line.

=end private

=cut
sub log
{
  my $self = shift;
  if (my $fp = $self->{lfp})
    {
      print $fp @_;
    }
}

sub logf
{
  my ($self,$file,$hash) = @_;
  my $json = $self->{json} ||= JSON->new()->ascii(1);
  if (my $fp = $self->{lfp})
    {
      $self->log(qq[{ "oops": "logf used before prolog??",\n"unpacked_files":{\n])
        unless tell $fp; # }}
      my $str = $json->encode({$file => $hash});
      $str =~ s{^\{}{}s;
      $str =~ s{\}$}{}s;
      die "logf failed to encode newline char: $str\n" if $str =~ m{(\n|\r)};
      $self->log(" $str,\n");
    }
}

$SIG{'XFSZ'} = sub
{
  print STDERR "soft RLIMIT_FSIZE exceeded. SIGXFSZ recieved. Exiting\n";
  exit;
};

# if this returns 0, we test again and call it again, possibly.
# if this returns nonzero, we just continue.
sub _default_fs_warn()
{
  carp "Filesystem (@_) is almost full.\n $0 paused for 30 sec.\n";
  sleep(30);
  return 0;	
}

## returns 1, if enough space free.
## returns 0, if warn-method was called, and returned nonzero
## returns -1, if no warn method
## or does not return at all, and rechecks the status
##  with at least on second delay, if warn-method returns 0.
sub _fs_check
{
  my ($self, $needed_b, $needed_i, $needed_p) = @_;
  $needed_b = '1M' unless defined $needed_b;	# bytes
  $needed_i = 100  unless defined $needed_i;	# inodes
  $needed_p = 1.0  unless defined $needed_p;	# percent
  $needed_b = _bytes_unit($needed_b);

  open DIR, "<", $self->{destdir} or 
  opendir DIR, $self->{destdir} or return;
  ## fileno() does not work with opendir() handles.
  my $fd = fileno(DIR); return unless defined $fd;

  for (;;)
    {
      my $st = eval { [ fstatvfs($fd) ] };
      my $total_b = $st->[1] * $st->[2];	# f_frsize * f_blocks
      my $free_b  = $st->[0] * $st->[4];	# f_bsize * f_bavail
      my $free_i  = $st->[7];			# f_favail
      my $perc = 100.0 * ($total_b - $free_b) / ($total_b||1);

      return 1 if $free_b >= $needed_b && 
                  $free_i >= $needed_i && 
		  (100-$perc > $needed_p);
	
      return -1 unless $self->{fs_warn};
      my $w = $self->{fs_warn}->($self->{destdir}, $perc, $free_b, $free_i);
      return 0 if $w;
      sleep 1;
    }
}

sub new
{
  my $self = shift;
  my $class = ref($self) || $self;
  my %obj = (ref $_[0] eq 'HASH') ? %{$_[0]} : @_;

  $obj{verbose} = 1 unless defined $obj{verbose};
  $obj{destdir} ||= '.';
  $obj{logfile} ||= \*STDOUT;
  $obj{maxfilesize} = '100M' unless defined $obj{maxfilesize};
  $obj{maxfilesize} = _bytes_unit($obj{maxfilesize});

  mkpath($obj{destdir}); # abs_path is unreliable if destdir does not exist
  $obj{destdir} = Cwd::fast_abs_path($obj{destdir});
  # used in unpack() to jail mime_handlers deep inside destdir:
  $obj{dot_dot_safeguard} = 20 unless defined $obj{dot_dot_safeguard};
  $obj{jail_chmod0} ||= 0;

  carp "We are running as root: Malicious archives may clobber your filesystem.\n" unless $>;

  if (ref $obj{logfile} eq 'SCALAR' or !(ref $obj{logfile}))
    {
      open $obj{lfp}, ">", $obj{logfile} or croak "open logfile $obj{logfile} failed: $!\n";
    }
  else
    {
      $obj{lfp} = $obj{logfile};
    }

  if ($obj{maxfilesize})
    {
      eval { no strict; BSD::Resource::setrlimit(RLIMIT_FSIZE, $obj{maxfilesize}, RLIM_INFINITY); }
       or carp "WARNING maxfilesize=$obj{maxfilesize} ignored:\n $@\n Maybe package perl-BSD-Resource is not installed??\n\n";
    }

  $obj{minfree}{factor} = 10    unless defined $obj{minfree}{factor};
  $obj{minfree}{bytes}  = '1M'  unless defined $obj{minfree}{bytes};
  $obj{minfree}{percent} = '1%' unless defined $obj{minfree}{percent};
  minfree(\%obj, warning => $obj{fs_warn}||\&_default_fs_warn);

  $obj{exclude}{empty_dir} = 1  unless defined $obj{exclude}{empty_dir};
  $obj{exclude}{empty_file} = 1 unless defined $obj{exclude}{empty_file};

  for my $h (@builtin_mime_handlers)
    {
      mime_handler(\%obj, @$h);
    }

  return bless \%obj, $class;
}

sub DESTROY
{
  my $self = shift;
  if ($self->{lfp} and tell $self->{lfp})
    {
      # this should never happen. 
      # always delete $self->{lfp} manually, when done.
      ## {{
      $self->log(qq[\n}, "error":"unexpected destructor seen"};\n]);
      close $self->{lfp} if $self->{lfp} ne $self->{logfile};
      delete $self->{lfp};
    }
  if ($self->{configdir})
    {
      rmtree($self->{configdir});
      delete $self->{configdir};
    }
}

=head2 unpack

$u->unpack($archive, [$destdir])

Determines the contents of an archive and recursivly extracts its individual files.  
An archive may be the pathname of a file or directory. The extracted contents will be 
stored in "destdir/$subdir/$dest_name", where dest_name is the filename
component of $archive without any leading pathname components, and possibly
stripped or added suffix. (Subdir defaults to ''.) If $archive is a directory,
then dest_name will also be a directory. If archive is a file, the type of
dest_name depends on the type of packing: If the archive expands to multiple
files, dest_name will be a directory, otherwise it will be a file. If a file of
the same name already exists in the destination subdir, an additional subdir
component is created to avoid any conflicts.
For each extracted file, a record is written to the logfile.
When unpacking is finished, the logfile contains one valid JSON structure.
Unpack achieves this by writing suitable prolog and epilog lines to the logfile.

The actual unpacking is dispatched to mime-type specfic mime handlers,
selected using C<mime>. A mime-handler can either be built-in code, or an
external program (or shell-script) found in a directory registered with
C<mime_handler_dir>.

A mime-handler is called with 6 parameters:
source_path, destdir, destfile, mimetype, description, and config_dir. 
Note, that destination_path is a freshly created empty working directory, even
if the unpacker is expected to unpack only a single file. 

The config_dir contains unpack configuration in .sh, .js and possibly 
other formats. A mime-handler should use this information, but need not.  
All data passed into C<new> is reflected there, as well as the active exclude-list.
Using the config information can be used by a mime-handler to skip unwanted
work or otherwise optimize unpacking.

C<unpack> monitors the available filesystem space in destdir. If there is less space
than configured with C<minfree>, a warning can be printed and unpacking is
optionally paused. It also monitors the mime-handlers progress reading the archive 
at source_path and reports percentages to STDERR (if verbose is 1 or more).

After the mime-handler is finished, they system considers replacing the
directory with a file, under the following conditions:

=over

=item *

There is exactly one file in the directory.

=item *

The file name is identical with directory name, 
except for one changed or removed
suffix-word. (*.tar.gz -> *.tar; or *.tgz -> *.tar) 

=item *

The file must not already exist in the parent directory.

=back

A mime-handler trying to place files outside of the specified
destination_path may receive 'permission denied' conditions. 
# In this case, the handler 
# can ask to be rerun with write permission to a certain number of parent
# directories by printing one or multiple strings of the form "../../file" 
# and exiting with a nonzero status. Unpack will respond by creating the
# needed number of additional subdirectories, each named '_' (two in this
# example: "./_/_" ), and will call the handler again with this extended
# destination_path. 
C<unpack> prepares 20 empty subdirectory levels and chdirs the unpacker 
in there. This number can be adjusted using C<new(dot_dot_safeguard => 20)>.
A directory 20 levels up from the current working dir has mode 0 while 
the mime-handler runs. C<unpack> can optionally chmod(0) the parent of the subdirectory 
after it chdirs the unpacker inside. Use C<new(jail_chmod0 => 1)> for this, default 
is off.

These are special hacks to keep badly constructed 
tar balls, cpio, or zip archives at bay.

Please note, that all this helps against relative paths, but not against absolute 
paths in archives.
It is the responsibility of mime-handlers to not create absolute paths.

A missing mime-handler is skipped. A mime-handler is expected to return an
exit status of 0 upon success. If it runs into a problem, it should print lines
starting with the affected filenames to stderr.
Such errors are recorded in the log with the unpacked archive, and as far as
files were created, also with these files.

=cut

sub unpack
{
  ## as long as $archive is outside $self->{destdir}, we construct our destdir by
  ## replacing $self->{input_dir} with $self->{destdir}.
  ## This $self->{input_dir} must be created and kept constant at the earliest 
  ## possible call.
  ## When the $archive is inside $self->{destdir}, we do not use $self->{input_dir},
  ## we then use the current $in_dir as destdir.
  ## 
  ## Whenever an archive path outside $self->{destdir} is found, 
  ## it is first passed through Cwd::fast_abs_path before any other processing occurs.
  ##
  my ($self, $archive, $destdir) = @_;
  $destdir = $self->{destdir} unless defined $destdir;

  if (($self->{recursion_level}||0) > 1000)
    {
      push @{$self->{error}}, "unpack('$archive','$destdir'): recursion limit 1000";
      ## this is only an emergency stop.
      return;
    }

  if ($archive !~ m{^/} or $archive !~ m{^\Q$self-{destdir}\E/})
    {
      $archive = Cwd::fast_abs_path($archive) if -e $archive;
    }

  if ($self->{recursion_level}++ == 0)
    {
      $self->{json} ||= JSON->new()->ascii(1);

      $self->{input} = $archive;
      ($self->{input_dir}, $self->{input_file}) = ($1, $2) if $archive =~ m{^(.*)/([^/]*)$};

      my $s = $self->{json}->encode({destdir=>$self->{destdir}, pid=>$$, version=>$VERSION, 
		    input => $archive, start => scalar localtime});
      $s =~ s@}$@, "unpacked":{\n@;
      $self->log($s);
    }

  die "internal error" unless $self->{input_dir};

  my ($in_dir, $in_file) = ($1, $2) if $archive =~ m{^(.*/)([^/]*)$};
  $in_dir ||='/';
  $in_file||='';

  my $inside_destdir = 1;
  my $subdir = $in_dir; # remainder after stripping $orig_archive_prefix / $self->{destdir}
  unless ($subdir =~ s{^\Q$self->{destdir}\E/+}{})
    {
      $inside_destdir = 0;
      die "$archive path escaped. Neither inside original $self->{input_dir} nor inside destdir='$self->{destdir}'\n"
        unless $subdir =~ s{^\Q$self->{input_dir}\E/+}{};
    }

  print STDERR "unpack: r=$self->{recursion_level} in_dir=$in_dir, in_file=$in_file, destdir=$destdir\n" if $self->{verbose} > 1;

  my @missing_unpacker;

  if (-d $archive)
    {
      if (opendir DIR, $archive)
        {
          my @f = sort grep { $_ ne '.' && $_ ne '..' } readdir DIR;
	  closedir DIR;
	  print STDERR "dir = @f\n" if $self->{verbose} > 1;
	  for my $f (@f) 
	    {
	      next if $self->{exclude}{re} && $f =~ m{$self->{exclude}{re}};
	      my $new_in =  "$archive/$f";
	      ## if $archive is $inside_destdir, then $archive is normally indentical to $destdir.
	      ## ($inside_destdir means inside $self->{destdir}, actually)
	      $self->unpack($new_in, $destdir) unless -l $new_in;
	    }
	}
      else
        {
	  push @{$self->{error}}, "unpack dir ($archive) failed: $!";
	}
    }
  else
    {
      if ($self->_not_excluded($subdir, $in_file) and
          !defined($self->{done}{$archive}))
	{
	  my $m = $self->mime($archive);
	  my ($h, $more) = $self->find_mime_handler($m);
	  my $data = { mime => $m->[0] };
	  if ($more)
	    {
	      $data->{found} = $more;
	      push @missing_unpacker, @{$more->{missing}} if $more->{missing};
	    }

          if ($m->[0] eq 'text/plain' or !$h)
	    {
	      unless ($archive =~ m{^\Q$self->{destdir}\E})
		{
		  mkpath($destdir);
		  if (-e "$destdir/$in_file")
		    {
		      print STDERR "unpack copy in: $destdir/$in_file already exists, " if $self->{verbose};
		      $destdir = File::Temp::tempdir("_XXXX", DIR => $destdir);
		      print STDERR "using $destdir/$in_file instead.\n" if $self->{verbose};
		    }
		  $data->{error} = "copy($archive): $!" unless File::Copy::copy($archive, "$destdir/$in_file");
	          $self->logf("$destdir/$in_file" => $data);
		}
	      else
	        {
	          $self->logf($archive => $data);
		}
	    }
	  else
	    {
	      mkpath($destdir);
	      $self->{configdir} = $self->_prep_configdir() unless exists $self->{configdir};
	      my $new_name = $in_file;
	      
	      # Either shorten the name from e.g. foo.txt.bz2 to foo.txt or append 
	      # something: foo.pdf to foo.pdf._
	      $new_name .= "._" unless $h->{suff_re} and $new_name =~ s{$h->{suff_re}(\._)?$}{};

	      ## if consumer of logf wants to do progress indication himself, 
	      ## then tell him what we do before we start. (Our timer tick code may be analternative...)
	      #
	      # if ($archive =~ m{^\Q$self->{destdir}\E})
	      #   {
	      #     $self->logf($archive => { unpacking => $h->{fmt_p} });
	      #   }
	        
	      my $unpacked = $self->_run_mime_handler($h, $archive, $destdir, 
	      				$new_name, $m->[0], $m->[2], $self->{configdir});

	      if ($archive =~ m{^\Q$self->{destdir}\E})
	        {
	          $self->{done}{$archive} = $unpacked;

		  # to delete it, we should know if it was created during unpack.
		  $data->{cmd} = $h->{fmt_p};
		  $data->{unpacked} = $unpacked;
	          $self->logf($archive => $data);
		}

	      my $newdestdir = $unpacked;
	      $newdestdir =~ s{/+[^/]+}{} unless -d $newdestdir;	        # make sure it is a directory
	      $newdestdir = $destdir unless $newdestdir =~ m{^\Q$self->{destdir}\E/};	# make sure it does not escape
	      $self->unpack($unpacked, $newdestdir);
	    }
	}
    }

  if (--$self->{recursion_level} == 0)
    {
      my $epilog = {end => scalar localtime};
      $epilog->{missing_unpacker} = \@missing_unpacker if @missing_unpacker;
      my $s = $self->{json}->encode($epilog);
      $s =~ s@^{@},@;
      $self->log($s . ";\n");

      if ($self->{lfp} ne $self->{logfile})
        {
          close $self->{lfp} or carp "logfile write ($self->{logfile}) failed: $!\n";
	}
      delete $self->{lfp};
    }

  # FIXME: should return nonzero if we had any unrecoverable errors.
  return 0;
}

=head2 run

$u->run([argv0, ...], @redir, ... { init => sub ..., in, out, err, watch, every, prog, ... })

A general purpose fork-exec wrapper, based on IPC::Run. STDIN is closed, unless you specify
an in => as described in IPC::Run. STDERR and STDOUT are both printed to
STDOUT, prefixed with 'E: ' and 'O: ' respectively, unless you specify out =>,
err =>, or out_err => ... for both.  

Using redirection operators in @redir takes precedence over the above in/out/err 
redirections. See also L<IPC::Run>. If you use the options in/out/err, you should
restrict your redirection operators to the forms '<', '0<', '1>', '2>', or '>&' due
to limitations in the precedence logic. Piping via '|' is properly recognized, 
but background execution '&' may confuse the precedence logic.

This C<run> method is completly independent of the rest of File::Unpack. It works both
as a static function and as a method call.
It is used internally by C<unpack>, but is exported to be of use elsewhere.

Init is run after construction of redirects. Calling chdir() in init thus has no
effect on redirects with relative paths. 

Return value in scalar context is the first nonzero result code, if any. In list context 
all return values are returned.
=cut

sub run
{
  shift if ref $_[0] ne 'ARRAY';	# toss $self object handle.
  my (@cmd) = @_;
  my $opt = pop @cmd if ref $cmd[-1] eq 'HASH';

  # run the command with 
  # - STDIN closed, unless you specify an { in => ... }
  # - STDERR and STDOUT printed prefixed with 'E: ', 'O: ' to STDOUT, 
  #   unless you specify out =>, err =>, or out_err => ... for both.
  $opt->{in}  ||= \undef;
  $opt->{out} ||= $opt->{out_err};
  $opt->{err} ||= $opt->{out_err};
  $opt->{out} ||= sub { print "O: @_\n"; };
  $opt->{err} ||= sub { print "E: @_\n"; };

  my $has_i_redir = 0; 
  my $has_o_redir = 0;
  my $has_e_redir = 0;

  ## The ugly truth is, there might be multiple commands with pipes.
  ## We need to provide all of them with the proper redirects.
  ## A command that pipes somewhere else, has_o_redir outbound through the pipe.
  ## A command that is piped into, has_i_redir inbound from the pipe.
  my @run = ();
  push @run, debug => $opt->{debug} if $opt->{debug};


  for my $c (@cmd)
    {
      if (ref $c)
        {
          push @run, $c;
          # put init early, so that it is run, before any IO redirects access relative paths.
          push @run, init => $opt->{init} if $opt->{init};
          next;		# don't look into argvs, but
	}
      # look only into redirection operators
      $has_i_redir++ if $c =~ m{^0?<};
      $has_o_redir++ if $c =~ m{^1?>};
      $has_e_redir++ if $c =~ m{^(2>|>&$)};
      if ($c eq '|')
        {
          push @run, '0<', $opt->{in} unless $has_i_redir;
	  $has_i_redir = 'piped';
          push @run, "2>", $opt->{err} unless $has_e_redir;
	  $has_e_redir = $has_o_redir = 0;
	}
      push @run, $c;
    }

  push @run, '0<', $opt->{in}  unless $has_i_redir;
  push @run, "1>", $opt->{out} unless $has_o_redir;
  push @run, "2>", $opt->{err} unless $has_e_redir;

# die Dumper \@run if $cmd[0][0] eq '/usr/bin/rpm2cpio';

  my $t	= IPC::Run::timer($opt->{every}-0.6) if $opt->{every};
  push @run, $t if $t;

  my $h = eval { IPC::Run::start @run; };
  return wantarray ? (undef, $@) : undef unless $h;

  while ($h->pumpable)
    {
      # eval {} guards against 'process ended prematurely' errors.
      # This happens on very fast commands, despite pumpable().
      eval { $h->pump };
      if ($t && $t->is_expired)
        {
	  $opt->{prog}->($h, $opt);
	  $t->start($opt->{every});
	}
    }
  $h->finish;
  return wantarray ? $h->full_results : $h->result;
}

=head2 fmt_run_shellcmd

File::Unpack::fmt_run_shellcmd( $m->{argvv} )

Static function to pretty print the return value $m of method find_mime_handler();
It formats a command array used with run() as a properly escaped shell command string.

=cut 

sub fmt_run_shellcmd
{
  my @a = @_;
  @a = @{$a[0]{argvv}} if ref $a[0] eq 'HASH';
  my @r = ();
  push @r, ref() ? '('.shell_quote(@$_).')' : shell_quote($_) foreach @a;
  my $r = join ' ', @r;
  $r =~ s{^\((.*)\)$}{$1} unless $#a;	# parenthesis around a single cmd are unneeded.
  return $r;
}

## not a method, officially.
#
## Chdir in and out of a jail is done here, as IPC::Run::run({init}->())
## has bad timing for our purposes.
#
## fastjar extracts happily to ../../..
## this happens in cups-1.2.1/scripting/java/cups.jar
#
## FIXME:
# "/tmp/xxxx/cups-1.2.4-11.5.1.el5/cups-1.2.4/scripting/java/cups.jar":
#  {"cmd":"/usr/bin/unzip -P no_pw -q -o '%(src)s'",
#   "unpacked":"/tmp/xxxx/cups-1.2.4-11.5.1.el5/cups-1.2.4/_Knw_"}
# Two issues: 
#   a) _run_mime_handler in /tmp/xxxx/cups-1.2.4-11.5.1.el5/cups-1.2.4
#      should be /tmp/xxxx/cups-1.2.4-11.5.1.el5/cups-1.2.4/scripting/java
#   b) _Knw_ should not appear ...
#

sub _run_mime_handler
{
  my ($self, $h, @argv) = @_;
  my $src     = $argv[0];
  my $destdir = $argv[1];
  my $dot_dot_safeguard = $self->{dot_dot_safeguard}||0;
  $dot_dot_safeguard = 2 if $dot_dot_safeguard < 2;

  mkpath($destdir);
  my $jail_base = File::Temp::tempdir("_XXXX", DIR => $destdir);
  my $jail = $jail_base . ("/_" x $dot_dot_safeguard);
  mkpath($jail);

  my $args = 
    {
      src	=> $argv[0],	# abs_path()
      destdir	=> $jail,	# abs_path() - for now...
      destfile	=> $argv[2],	# filename() - a suggested name, simply based on src, in case the unpacker needs it.
      mime	=> $argv[3],
      descr	=> $argv[4],	# mime_descr
      configdir	=> $argv[5]	# abs_path()
    };
  
  my @cmd;
  for my $a (@{$h->{argvv}})
    {
      if (ref $a)
        {
	  my @c = ();
	  for my $b (@$a)
	    {
	      push @c, _subst_args($b, $args);
	    }
	  push @cmd, [@c];
	}
      else
        {
	  push @cmd, _subst_args($a, $args);
	}
    }
  print "_run_mime_handler in $destdir: " . fmt_run_shellcmd(@cmd) . "\n";

  my $cwd = getcwd() or carp "cannot fetch initial working directory, getcwd: $!";
  chdir $jail or die "chdir '$jail'";
  chmod 0, $jail_base if $self->{jail_chmod0};
  # Now have fully initialzed in the parent before forking. 
  #  This is needed, as all redirect operators are executed in the parent before forking.
  # init => sub { ... } is no longer needed. sigh, I really wanted to the init sub for the chdir.
  # But hey, mkpath() and rmtree() change the cwd so often, and restore it, so why shouldn't we?

  my @r = $self->run(@cmd, 
    { 
      debug => ($self->{verbose} > 2) ? $self->{verbose} - 2 : 0, 
      watch => $src, every => 5, fu_obj => $self, mime_handler => $h, 
      prog => sub { $_[1]{tick}++; print "T: tick_tick $_[1]{tick}\n"; },
    });
    
  chmod 0700, $jail_base if $self->{jail_chmod0};
  chdir $cwd or die "cannot chdir back to cwd: chdir($cwd): $!";

  # loop through all _: if it only contains one item , replace it with this item,
  # be it a file or dir. This uses $jail_tmp, an unused pathname.
  my $jail_tmp = File::Temp::tempdir("_XXXX", DIR => $destdir);
  rmdir $jail_tmp;

  # TODO: handle failure
  # - remove all, 
  # - retry with a fallback handler , if any.
  print STDERR "Non-Zero return value: $r[0]\n" if $r[0];

  # if only one file in $jail, move it up, and return 
  # the filename instead of the dirname here.
  # (We don't search for $args->{destfile}, it is the unpackers choice to use it or not.)
  my $wanted_name;
  for (my $i = 0; $i <= $dot_dot_safeguard; $i++)
    {
      opendir DIR, $jail_base or last;
      my @found = grep { $_ ne '.' and $_ ne '..' } readdir DIR;
      closedir DIR;
      print STDERR "dot_dot_safeguard=$dot_dot_safeguard, i=$i, found=$found[0]\n" if $self->{verbose} > 1;
      last if scalar @found != 1;
      $wanted_name = $found[0] if $i == $dot_dot_safeguard;
      last unless -d $jail_base . "/" . $found[0];
      rename $jail_base, $jail_tmp;
      rename $jail_tmp . "/" . $found[0], $jail_base;
      rmdir $jail_tmp or last;
    }

  print STDERR "Hmmm, unpacker did not use destname: $args->{destfile}\n" if $self->{verbose} and !defined $wanted_name;
  print STDERR "Hmmm, unpacker saw destname: $args->{destfile}, but used destname: $wanted_name\n" 
    if defined($wanted_name) and $wanted_name ne $args->{destfile};

  $wanted_name = $args->{destfile} unless defined $wanted_name;
  my $wanted_path = $destdir . "/" . $wanted_name if defined $wanted_name;

  my $unpacked = $jail_base;
  if (defined($wanted_name) and !-e $wanted_path)
    {
      if (-d $jail_base)
        {
	  ## find out, if the unpacker created exactly one file or one directory, 
	  ## in this case we can move one level further.
	  opendir DIR, $jail_base;
          my @found = grep { $_ ne '.' and $_ ne '..' } readdir DIR;
          closedir DIR;
	  if ($#found == 0 and $found[0] eq $wanted_name)
	    {
              rename "$jail_base/$found[0]", $wanted_path;
	      rmdir $jail_base;
	    }
	  else
	    {
              rename $jail_base, $wanted_path;
	    }
	}
      else
        {
          rename $jail_base, $wanted_path;
	}
      $unpacked = $wanted_path;
    }

  return $unpacked;
}

sub _prep_configdir
{
  my ($self) = @_;
  my $dir = "/tmp/file_unpack_$$/";
  mkpath($dir);
  my $j = $self->{json}->allow_nonref();

  open SH, ">", "$dir/config.sh";
  open JS, ">", "$dir/config.js";

  print JS "{\n";

  for my $group ('', 'minfree', 'exclude')
    {
      my $h_ref = ($group eq '') ? $self : $self->{$group};
      for my $k (sort keys %$h_ref)
        {
	  my $val = $h_ref->{$k};
	  next if $k eq 'recursion_level';
	  next if ref $val;		# we only take scalars.
	  my $name = ($group eq '') ? $k : "${group}_$k";
	  printf SH "%s=%s\n", shell_quote(uc "fu_$name"), shell_quote($val);
	  printf JS "%s:%s,\n", $j->encode($name), $j->encode($val);
	}
    }

  print SH "FU_VERSION=$VERSION\n";
  print JS qq["fu_version":"$VERSION"\n}\n];

  close SH;
  close JS;
  return $dir;
}


=head2 mime_handler_dir mime_handler

$u->mime_handler_dir($dir, ...)
$u->mime_handler($mime_name, $suffix_regexp, \@argv, @redir, ...)

Registers one or more directories where external mime-handler programs are found.
Multiple directories can be registered, They are searched in reverse order, i.e. 
last added takes precedence. Any external mime-handler takes precedence over built-in code.
An array ref to the new list of directories is returned.

Helpers are mapped to mime-types by their name. The name can be constructed
from the mimetype by replacing the '/' with a '=' character, and by using the
word 'ANY' as a wildcard component. The '=' character is interpreted as an
implicit '=ANY+' if needed.

 Examples:

  Mimetype                   handler names tried in sequence
  ----------------------------------------------------------
  image/png                  image=png 
                              image=ANY 
			       image
			        ANY=ANY
				 ANY

  application/vnd.oasis+zip  application=vnd.oasis+zip 
                              application=ANY+zip
                               application=ANYzip
			        application=zip
			         application=ANY
				      ...
  
A trailing '=ANY' is implicit, as shown by these examples. The rules for
determinig precedence are this:

=over 

=item *

Search in one directory is exhaused before the next is considered.
 
=item *

A matching name with wildcards has lower precedence than a matching name without.

=item *

A wildcard before the '=' sign lowers precedence more than one after it.

=back

The mapping takes place when C<mime_handler_dir> is called, later additions are 
not recognized. C<mime_handler> does not do any implicit expansions. Call it
multiple times with the same command and different names if needed.
The default argument list is "%(src)s %(destdir)s %(destfile)s %(mime)s %(descr)s %(configdir)s" --
this is applied, if no args are given and no redirections are given.

Both methods return an ARRAY-ref of all currently known mime handlers.

=cut 
my @def_mime_handler_fmt = qw(%(src)s %(destdir)s %(destfile)s %(mime)s %(descr)s %(configdir)s);

sub _subst_args
{
  my $f = Text::Sprintf::Named->new({fmt => $_[0]});
  return $f->format({args => $_[1]});
}

sub mime_handler
{
  my ($self, $name, $suff_re, @args) = @_;
  @args = ($name) unless @args;
  @args = ([@args]) unless ref $args[0];

  $name =~ s{(.*/)?(.*?)=(.*?)$}{$2/$3};

  push @{$args[0]}, @def_mime_handler_fmt unless $#{$args[0]} or defined $args[1];

  my $pat = "^\Q$name\E\$";
  $pat =~ s{\\%}{ANY}g;
  $pat =~ s{^\^ANY}{};
  $pat =~ s{ANY\$$}{};
  $pat =~ s{ANY}{\\b(\[\^\/\]\+)\\b}g;
  unshift @{$self->{mime_handler}}, 
    { 
      name => $name, pat => $pat, suff_re => $suff_re, 
      fmt_p => fmt_run_shellcmd(@args), argvv => \@args
    };

  delete $self->{mime_orcish};	# to be rebuilt in find_mime_handler()
  return $self->{mime_handler};
}

sub mime_handler_dir
{
  my ($self, @dirs) = @_;

  for my $d (@dirs)
    {
      my %h;
      if (opendir DIR, $d)
        {
	  %h = map { { a => "$d/$_" } } grep { -f "$d/$_" } readdir DIR;
	  closedir DIR;
	}
      else
        {
	  carp "Cannot opendir $d: $!, skipped\n";
	}

      # add =ANY suffix, if missing
      for my $h (keys %h)
        {
	  if ($h !~ m{[/=]})
	    {
	      my $h2 = $h . "=ANY";
	      $h{$h2} = $h{$h} unless defined $h{$h2};
	    }
	}

      # add expansion of = to =ANY+, if missing
      for my $h (keys %h)
        {
	  next if $h =~ m{=ANY+};
	  my $h2 = $h; $h2 =~ s{=}{=ANY+};
	  $h{$h2} = $h{$h} unless defined $h{$h2};
	}

      # calculate priorities
      for my $h (keys %h)
        {
	  my $n = 1000000;
	  my $p = 1000;
	  while ($h =~ m{(ANY|=)}g)
	    {
	      if ($1 eq '=')
	        {
		  $n = 1000;
		}
	      else
	        {
	          $p += $n;
		}
	    }
	  # longer length has prio over shorter length. Hmm, this is ineffective, isnt it?
	  $h{$h}{p} = $p - length($h);
	}

      # now push them, sorted by prio
      for my $h (sort { $h{$a}{p} <=> $h{$b}{p} } keys %h)
        {
	  $self->mime_handler($h, undef, $h{$h}{a});
	}
    }
  return $self->{mime_handler};
}

=head2 find_mime_handler

$u->find_mime_handler($mimetype)

Returns a mime-handler suitable for unpacking the given $mimetype.
If called in list context, a second return value indicates which 
mime handlers whould be suitable, but could not be found in the system.

=cut

sub find_mime_handler
{
  my ($self, $mimetype) = @_;
  $mimetype = $mimetype->[0] if ref $mimetype eq 'ARRAY';

  return $self->{mime_orcish}{$mimetype}
    if defined $self->{mime_orcish}{$mimetype} and 
            -f $self->{mime_orcish}{$mimetype}{argvv}[0][0];
  
  my $r = undef;
  for my $h (@{$self->{mime_handler}})
    {
      if ($mimetype =~ m{$h->{pat}})
        {
	  $self->_finalize_argvv($h);
	  unless (-f $h->{argvv}[0][0])
	    {
	      push @{$r->{missing}}, $h->{argvv}[0][0];
	      next;
	    }
	  $self->{mime_orcish}{$mimetype} = $h;
	  return wantarray ? ($h, $r) : $h;
	}
    }
  return wantarray ? (undef, $r) : undef;
}

#
# _finalize_argvv() executes a sub in 3 places:
# The argvv ptr itself can be a sub: 
#   this should return an array, where the
#   first element is the command (as an array-ref) and subsequent elements are
#   redirects. See run() for details.
# One of the argvv elements is a sub:
#   this should return the command as an array-ref, if it is argvv[0],
#   or return one or more redirects.
# One element of argvv[0] is a sub:
#   this should return one or more command names, options, arguments,
#
# Tricky part of the implementation is the in-place array expansion while iterating.
#
sub _finalize_argvv
{
  my ($self, $h) = @_;

  my $update_fmt_p = 0;
  if (ref $h->{argvv} eq 'CODE')
    {
      $h->{argvv} = [ $h->{argvv}->($self) ];
      $update_fmt_p++;
    }

  # If any part of LIST is an array, "foreach" will get very confused if you add or
  # remove elements within the loop body, for example with "splice".   So don't do
  # that.
  # Sigh, we want do do exactly that, a sub may replace itself by any number of elements. Use booring C-style loop.
  my $last = $#{$h->{argvv}};
  for (my $idx = 0; $idx <= $last; $idx++)
    {
      if (ref $h->{argvv}[$idx] eq 'CODE')
        {
	  my @r = $h->{argvv}[$idx]($self);
	  splice @{$h->{argvv}}, $idx, 1, @r;
	  $idx += $#r;
	  $last +=$#r;
          $update_fmt_p++;
	}
    }
  $last = $#{$h->{argvv}};
  for (my $idx = 0; $idx <= $last; $idx++)
    {
      next unless ref $h->{argvv}[$idx] eq 'ARRAY';
      my $last1 = $#{$h->{argvv}[$idx]};
      for (my $idx1 = 0; $idx1 <= $last1; $idx1++)
        {
	  if (ref $h->{argvv}[$idx][$idx1] eq 'CODE')
	    {
	      my @r = $h->{argvv}[$idx][$idx1]->($self);
	      splice @{$h->{argvv}[$idx]}, $idx1, 1, @r;
	      $idx1 += $#r;
	      $last1 +=$#r;
              $update_fmt_p++;
	    }
	}
    }

  $h->{fmt_p} = fmt_run_shellcmd($h) if $update_fmt_p;
}

=head2 minfree

$u->minfree(factor => 10, bytes => '100M', percent => '3%', warning => sub { .. })

Guard the filesystem (destdir) against becoming full during C<unpack>. 
Before unpacking each source archive, the free space is measured and compared against three conditions:

=over 

=item *

The archive size multiplied with the given factor must fit into the filesystem.

=item *

The given number of bytes in optional K, M, G, or T units must be free.

=item *

The filesystem must have at least the given free percentage. The '%' character is optional.
 
=back

The warning method is called with the following parameters: 
  &warning->($pathname, $full_percentage, $free_bytes, $free_inodes);
It is expected to print an appropriate warning message, and delay a few seconds.
It should return 0 to cause a retry. It should return nonzero to continue unpacking.
The default warning method prints a message to STDERR, waits 30 seconds, and returns 0.

The filesystem may still become full and unpacking may fail, if e.g. factor was chosen lower then 
the compression ratio of the unpacked archives.

=cut

sub _bytes_unit
{
  my ($text) = @_;
  return $1*1024                if $text =~ m{([\d\.]+)k}i;
  return $1*1024*1024           if $text =~ m{([\d\.]+)m}i;
  return $1*1024*1024*1024      if $text =~ m{([\d\.]+)g}i;
  return $1*1024*1024*1024*1024 if $text =~ m{([\d\.]+)t}i;
  return $text;
}

sub minfree
{
  my $self = shift;
  my %opt = @_;

  for my $i qw(factor bytes percent)
    {
      $self->{minfree}{$i} = $opt{$i} if defined $opt{$i};
      $self->{minfree}{$i} ||= 0;
    }
  $self->{minfree}{bytes} = _bytes_unit($self->{minfree}{bytes});
  $self->{minfree}{percent} =~ s{%$}{};
  $self->{fs_warn} = $opt{warning} if ref $opt{warning};
}

=head2 mime

$u->mime($filename)

$u->mime(file => $filename)

$u->mime(buf => "#!/bin ...", file => "what-was-read")

$u->mime(fd => \*STDIN, file => "what-was-opened")

Determines the mimetype (and optionally additional information) of a file.
The file can be specified by filename, by a provided buffer or an opened filedescriptor.
For the latter two casese, speifying the filename is optional, and used for diagnostics.

C<mime> uses Christos Zoulas' excellent libmagic exposed via File::LibMagic and the
shared-mime-info database from freedesktop.org exposed via
File::MimeInfo::Magic, if available.  Either one is sufficient, but having both
is better. LibMagic sometimes says 'text/x-pascal', although we have a F<.desktop>
file, or returns says 'text/plain', but has contradicting details in its description.

C<File::MimeInfo::Magic::magic> is consulted where the libmagic output is dubious.

This implementation also features multi-level mime-type recognition for efficient unpacking.
If we'd recognize a large bzipped tar ball only as bzip, we'd unpack a huge
temporary tar-file, consuming the same amount of disk space as its content,
which C<unpack> would extract in a second step.  The multi-level recognition
returns 'application/x-tar+bzip2' in this case, and allows for a mime-handler
to e.g. pipe the bzip2 contents into tar (which is exactly what 'tar jxvf'
does, making a very simple and efficient mime-handler).

C<mime> returns a 3 or 4 element arrayref with mimetype, charset, description, diff;
where diff is only present when both methods disagree.

Examples:
 
 [ 'text/x-perl', 'us-ascii', 'a /usr/bin/perl -w script text']

 [ 'text/x-mpegurl', 'utf-8', 'M3U playlist text', 
   [ 'text/plain', 'application/x-mpegurl']]

 [ 'application/x-tar+bzip2, 'binary', 
   "bzip2 compressed data, block size = 900k\nPOSIX tar archive (GNU)", ...]

=cut

sub mime 
{
  my ($self, @in) = @_;

  my %in = %{$in[0]}  if !$#in and ref $in[0] eq 'HASH';
  unshift @in, 'file' if !$#in and !ref $in[0];
  %in = @in if $#in > 0;

  my $flm = $self->{flm} ||= File::LibMagic->new();

  unless (defined $in{buf})
    {
      my $fd = $in{fd};
      unless ($fd)
        {
	  open $fd, "<", $in{file} or
	    return [ 'x-system/x-error', undef, "cannot open '$in{file}': $!" ];
	}

      my $f = $in{file}||'-';
      $in{buf} = '';
      my $pos = tell $fd;
      ##bzip2 below needs a long buffer, or it returns 0.
      my $len = read $fd, $in{buf}, $UNCOMP_BUFSZ;
      return [ 'x-system/x-error', undef, "read '$f' failed: $!" ] unless defined $len;
      return [ 'x-system/x-error', undef, "read '$f' failed: $len: $!" ] if $len < 0;
      return [ 'application/x-empty', undef, 'empty' ] if $len == 0;
      seek $fd, $pos, 0;

      close $fd unless $in{fd};
    }
  $in{file} = '-' unless defined $in{file};


  open my $fd, '<', \$in{buf};	# for use with File::MimeInfo::Magic

  ## flm can say 'cannot open \'IP\' (No such file or directory)'
  my $mime1 = $flm->checktype_contents($in{buf});
  return [ 'x-system/x-error', undef, $mime1 ] if $mime1 =~ m{^cannot open};

  my $enc; ($mime1, $enc) = ($1,$2) if $mime1 =~ m{^(.*?);\s*(.*)$};
  $enc =~ s{^charset=}{};
  my @r = ($mime1, $enc, $flm->describe_contents($in{buf}) );
  my $mime2;

  if ($mime1 =~ m{^text/x-(pascal|fortran)$})
    {
      # xterm.desktop
      # ['text/x-pascal; charset=utf-8','UTF-8 Unicode Pascal program text']
      # 'application/x-desktop'
      #
      # Times-Roman.afm
      # ['text/x-fortran; charset=us-ascii','ASCII font metrics']
      # 'application/x-font-afm'
      #
      # debian/rules
      # ['text/x-pascal; charset=us-ascii','a /usr/bin/make -f  script text']
      # 'text/x-makefile'
      if ($mime2 ||= eval { open my $fd,'<',\$in{buf}; File::MimeInfo::Magic::magic($fd); })
        {
	  $r[0] = "text/$1" if $mime2 =~ m{/(\S+)};
	}
    }
  elsif ($mime1 eq 'text/plain' and $r[2] =~ m{(PostScript|font)}i)
    {
      # IPA.pfa
      # ['text/plain; charset=us-ascii','PostScript Type 1 font text (OmegaSerifIPA 001.000)']
      # mime2 = 'application/x-font-type1'
      # $mime2 = eval { File::MimeInfo::Magic::mimetype($in{file}); };
      $mime2 ||= eval { open my $fd,'<',\$in{buf}; File::MimeInfo::Magic::magic($fd); };
      if ($mime2 and $mime2 =~ m{^(.*)/(.*)$})
        {
	  my ($a,$b) = ($1,$2);
	  $a = 'text' if $r[2] =~ m{\btext\b}i; 
	  $r[0] = "$a/$b";
	}
    }

  if ($r[0] eq 'text/plain' or 
      $r[0] eq 'application/octet-stream')
    {
      # hmm, are we sure? No, if the description contradicts:
      # 
      # bin/floor
      # ['text/x-pascal; charset=us-ascii','a /usr/bin/tclsh script text']
      # 'text/plain'
      $r[0] = "text/x-$2" if $r[2] =~ m{^a (\S*/)?([^/\s]+) .*script text$}i;
      if ($r[2] =~ m{\bimage\b})
        {
	  # ./opengl/test.tga
	  # ['application/octet-stream; charset=binary','Targa image data - RGB 128 x 128']
	  # 'image/x-tga'
          $mime2 ||= eval { open my $fd,'<',\$in{buf}; File::MimeInfo::Magic::magic($fd); };
	  $r[0] = $mime2 if $mime2 and $mime2 =~ m{^image/};
	}
    }

  if ($r[0] eq 'application/octet-stream')
    {
      # it can't get much worse, can it?
      ##
      # dotdot.tar.lzma
      # {'File::MimeInfo::Magic' => 'application/x-lzma-compressed-tar'} -- no, that was suffix based!
      # {'File::LibMagic' => ['application/octet-stream; charset=binary','data']}
      $mime2 ||= eval { open my $fd,'<',\$in{buf}; File::MimeInfo::Magic::magic($fd); };
      $r[0] = $mime2 if $mime2;
    }

  my $uncomp_buf = '';

  if ($r[0] eq 'application/octet-stream')
    {
      ## lzma is an extremly bad format. It has no magic.
      #
      # WARNING from Compress::unLZMA
      #  "This version only implements in-memory decompression (patches are welcomed).
      #   There is no way to recognize a valid LZMA encoded file with the SDK.
      #   So, in some cases, you can crash your script if you try to uncompress a
      #   non valid LZMA encoded file."
      # Does this also apply to us? 
      #
      # -- hmm, maybe we better leave it at calling lzcat.
      # Trade in "always a bit expensive" versus "sometimes crashing"...
      # 
#      my $lztest = `sh -c "/usr/bin/lzcat < $in{file} | head -c 1k > /dev/null" 2>&1`;
#      # -> /usr/bin/lzcat: (stdin): File format not recognized
#      if ($lztest !~ m{(not recognized|error)}i)
#        {
#	  $r[0] = 'application/x-lzma';
#	}

      if (10 < length $in{buf})
        {
	  no strict 'subs';	# Compress::Raw::Lzma::AloneDecoder, LZMA_OK, LZMA_STREAM_END

	  my $saved_input = $in{buf};
          my ($lz, $stat) = eval { new Compress::Raw::Lzma::AloneDecoder -Bufsize => $UNCOMP_BUFSZ, -LimitOutput => 1; };
	  if ($lz)
	    {
	      $stat = $lz->code($in{buf}, $uncomp_buf);
	      if ($stat == LZMA_OK or $stat == LZMA_STREAM_END)
	        {
		  $r[0] = "application/x-lzma";
		  $r[2] = "LZMA compressed data, no magic";
		}
	      $in{buf} = $saved_input;
	    }
	}
    }
  # printf STDERR "in-buf = %d bytes\n", length($in{buf});

  if ($r[0] =~ m{^application/(x-)?gzip$})
    {
      my ($gz, $stat) = eval { new Compress::Raw::Zlib::Inflate( -WindowBits => WANT_GZIP() ); };
      if ($gz)
        {
	  my $stat = $gz->inflate($in{buf}, $uncomp_buf);
	  # printf STDERR "stat=%s, uncomp=%d bytes \n", $stat, length($uncomp_buf);
	}
    }

  ## bzip2 is nice for stacked mime checking. It needs a huge input buffer that we do not normally provide.
  ## We only support it at the top of a stack, where we acquire enough additional input until bzip2 is happy.
  if ($r[0] =~ m{^application/(x-)?bzip2$} && !$in{recursion})
    {
      my $limitOutput = 1;
      my ($bz, $stat) = eval { new Compress::Raw::Bunzip2 0, 0, 0, 0, $limitOutput; };
      if ($bz)
        {
	  ## this only works if this is a first level call.
	  open IN, "<", $in{file};
	  seek IN, length($in{buf}), 0;
	  while (!length $uncomp_buf)
	    {
              my $stat = $bz->bzinflate($in{buf}, $uncomp_buf);
	      # $bz->bzflush($uncomp_buf);	# wishful thinking....
	      last if length($in{buf});   	# did not consume, strange.
	      last if length $stat;		# something wrong, or file ends.
	      last unless read IN, $in{buf}, 10*1024, length($in{buf});		# try to get more data
	    }
	  my $slurped = tell IN;	# likely to get ca. 800k yacc!
	  close IN;
          # use Data::Dumper; warn Dumper $stat, length($in{buf}), length($uncomp_buf), "slurped=$slurped";
	}
    }

  ## try to get at the second level mime type, for some well known linear compressors.
  while (length $uncomp_buf && $r[0] =~ m{^application/(x-)?([+\w]+)$})
    {
      my $compname = $2;
      my $next_uncomp_buf = '';

      # use Data::Dumper; printf STDERR "calling mime with buf=%d bytes, compname=$compname\n", length($uncomp_buf);

      my $m2 = $self->mime(buf => $uncomp_buf, file => "$in{file}+$compname", uncomp => \$next_uncomp_buf, recursion => 1);
      my ($a,$xminus,$b) = ($m2->[0] =~ m{^(.*)/(x-)?(.*)$});
      if ($a eq 'application')
        {
	  $r[0] = "application/x-$b+$compname"
	}
      else
        {
	  $r[0] = "application/x-$a-$b+$compname"
	}
      $r[2] .= "\n" . $m2->[2];
      $uncomp_buf = $next_uncomp_buf;
      # print Dumper "new: ", \@r, $m2, $compname, length($uncomp_buf);
    }

# use Data::Dumper;
# die Dumper \@r, "--------------------";

  if ($r[0] eq 'application/unknown+zip' and $r[2] =~ m{\btext\b}i)
    {
      # empty.odt
      # ['application/unknown+zip; charset=binary','Zip archive data, at least v2.0 to extract, mime type application/vnd OpenDocument Text']
      # application/vnd.oasis.opendocument.text
      if ($mime2 ||= eval { open my $fd,'<',\$in{buf}; File::MimeInfo::Magic::magic($fd); })
        {
          $mime2 .= '+zip' unless $mime2 =~ m{\+zip}i;
          $r[0] = $mime2 if $mime2 =~ m{^application/};
	}
    }
  $r[0] .= '+zip' if $r[0] =~ m{^application/vnd\.oasis\.opendocument\.text$};

  ${$in{uncomp}} = $uncomp_buf if ref $in{uncomp} eq 'SCALAR';
  $r[3] = [ $mime1, $mime2 ] if $mime1 ne $r[0] or ($mime2 and $mime2 ne $mime1);

  return \@r;
}

=head1 AUTHOR

Juergen Weigert, C<< <jnw at cpan.org> >>

=head1 BUGS

The implementation of C<mime> is an ugly hack. We suffer from the existance of
multiple file magic databases, and multiple conflicting implementations. With
perl we have at least 5 modules for this; here we use two.

The builtin list of mime-handlers is incomplete. Please submit your handler code.

Please report any bugs or feature requests to C<bug-file-unpack at rt.cpan.org>, or through
the web interface at L<http://rt.cpan.org/NoAuth/ReportBug.html?Queue=File-Unpack>.  I will be notified, and then you'll
automatically be notified of progress on your bug as I make changes.


=head1 RELATED MODULES

While designing File::Unpack, a range of other perl modules were examined. Many modules provide valuable service to File::Unpack and became dependencies or are recommended.
Others exposed drawbacks during closer examination and may find some of their
wheels re-invented here.

=head2 Used Modules

=over

=item File::LibMagic

This is the prefered mimetype engine. It disregards the suffix, recognizes more
types than any of the alternatives, and uses exactly the same engine as
/usr/bin/file in your openSUSE system. It also returns charset and description
information.  We crossreference the description with the mimetype to detect
weaknesses, and consult File::MimeInfo::Magic and some own logic, for e.g.
detecting LZMA compression which fails to provide any recognizable magic.
Required if you use C<mime>; not a hard requirement.

=item File::MimeInfo::Magic

Uses both magic information and file suffixes to determine the mimetype. Its
magic() function is used in a few cases, where File::LibMagic fails.  E.g. as
of June 2010, libmagic does not recognize 'image/x-targa'.
File::MimeInfo::Magic may be slower, but it features the shared-mime-info
database from freedesktop.org .  Recommended if you use C<mime>.

=item String::ShellQuote 

Used to call external mime-handlers. Required.

=item BSD::Resource

Used to reliably restrict the maximum file size. Recommended.

=item File::Path

mkpath(). Required.

=item Cwd

fast_abs_path(). Required.

=item JSON

Used for formatting the logfile. Required.

=back

=head2 Modules Not Used

=over

=item Archive::Extract

Archive::Extract tries first to determine what type of archive you are passing
it, by inspecting its suffix. It does not do this by using Mime magic.  Maybe
this module should use something like "File::Type" to determine the type,
rather than blindly trust the suffix. [quoted from perldoc]

Set $Archive::Extract::PREFER_BIN to 1, which will prefer the use of command 
line programs and won't consume so much memory. Default: use "Archive::Tar".

=item Archive::Zip

If you are just going to be extracting zips (and/or other archives) you are 
recommended to look at using Archive::Extract . [quoted from perldoc]
It is pure perl, so it's a lot slower then your '/usr/bin/zip'.

=item Archive::Tar

It is pure perl, so it's a lot slower then your "/bin/tar".
It is heavy on memory, all will be read into memory. [quoted from perldoc]

=item File::MMagic, File::MMagic::XS, File::Type

Compared to File::LibMagic and File::MimeInfo::Magic, these three are inferior.
They often say 'text/plain' or 'application/octet-stream' where the latter two report 
useful mimetypes.

=back

=head1 SUPPORT

You can find documentation for this module with the perldoc command.

    perldoc File::Unpack


You can also look for information at:

=over 4

=item * RT: CPAN's request tracker

L<http://rt.cpan.org/NoAuth/Bugs.html?Dist=File-Unpack>

=item * AnnoCPAN: Annotated CPAN documentation

L<http://annocpan.org/dist/File-Unpack>

=item * CPAN Ratings

L<http://cpanratings.perl.org/d/File-Unpack>

=item * Search CPAN

L<http://search.cpan.org/dist/File-Unpack/>

=back

=head1 SOURCE REPOSITORY

L<https://developer.berlios.de/projects/perl-file-unpck>

svn co L<https://svn.berlios.de/svnroot/repos/perl-file-unpck/trunk/File-Unpack>


=head1 ACKNOWLEDGEMENTS

Mime-type recognition relies heavily on libmagic by Christos Zoulas. I had long 
hesitated implementing File::Unpack, but set to work, when I dicovered
that File::LibMagic brings your library to perl. Thanks Christos. And thanks
for tcsh too.

=head1 LICENSE AND COPYRIGHT

Copyright 2010 Juergen Weigert.

This program is free software; you can redistribute it and/or modify it
under the terms of either: the GNU General Public License as published
by the Free Software Foundation; or the Artistic License.

See http://dev.perl.org/licenses/ for more information.


=cut

1; # End of File::Unpack
