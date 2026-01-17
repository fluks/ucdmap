use strict;
use warnings;
use File::ShareDir::Install;
use ExtUtils::MakeMaker;

install_share dist => 'data';

WriteMakefile(
    NAME => 'ucdmap',
    ABSTRACT => 'Unicode character map.',
    VERSION_FROM => 'bin/ucdmap.pl',
    LICENSE => 'gpl_3',
    AUTHOR => 'fluks <fluks.github@gmail.com>',
    SIGN => 1,
    PREREQ_PM => {
        Tk => 0,
        'File::ShareDir' => 0,
    },
    CONFIGURE_REQUIRES => {
        'ExtUtils::MakeMaker' => 0,
        'File::ShareDir::Install' => 0,
    },
    EXE_FILES => [
        'bin/ucdmap.pl'
    ],
);

package MY;
use File::ShareDir::Install qw(postamble);

sub postamble {
	return <<TARGET;
win_package:
	pp -M Tk::PNG -o ucdmap.exe -a data;lib\auto\share\dist\ucdmap --link libpng16-16__.dll --link zlib1__.dll -x -u --gui bin\ucdmap.pl
TARGET
}
