# $Id: Makefile.PL,v 1.4 1997-10-01 14:02:16+02 jv Exp $

use ExtUtils::MakeMaker 5.0;

require "./GetoptLong.pm";

WriteMakefile(
	NAME      => "Getopt::Long",
	DISTNAME  => 'GetoptLong',
	VERSION   => $Getopt::Long::VERSION,

	INSTALLDIRS => 'perl',
	PM 	  => { "newgetopt.pl"  => '$(INST_LIB)/newgetopt.pl',
		       "GetoptLong.pm" => '$(INST_LIB)/Getopt/Long.pm'
		     },
	dist      => { COMPRESS => 'gzip', SUFFIX => 'gz' },
 );
