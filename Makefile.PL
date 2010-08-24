use strict;
use warnings;
use ExtUtils::MakeMaker;

WriteMakefile(
    NAME                => 'File::Unpack',
    AUTHOR              => q{Juergen Weigert <jnw@cpan.org>},
    VERSION_FROM        => 'Unpack.pm',
    ABSTRACT_FROM       => 'Unpack.pm',
    ($ExtUtils::MakeMaker::VERSION >= 6.3002
      ? ('LICENSE'=> 'perl')
      : ()),
    PL_FILES            => {},
    # Lzma, Bzip2, Zlib are not strictly required. 
    # we need them only for multi-level mimetype recognition.
    PREREQ_PM => {
        'Test::More' => 0,
	'File::LibMagic' => 0,
	'File::MimeInfo::Magic' => 0,
	'Compress::Raw::Lzma' => 0,
	'Compress::Raw::Bzip2' => 0,
	'Compress::Raw::Zlib' => '2.024',
	'BSD::Resource' => 0,
	'Filesys::Statvfs' => 0,

	'File::Path' => 0,
	'File::Copy' => 0,
	'JSON' => 0,
	'String::ShellQuote' => 0,
	'IPC::Run' => 0, 
	'Text::Sprintf::Named' => 0,
    },
    dist                => { COMPRESS => 'bzip2 -9f', SUFFIX => 'bz2', },
    clean               => { FILES => 'File-Unpack-*' },
);
