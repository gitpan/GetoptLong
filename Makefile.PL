# $Id: Makefile.PL,v 1.9 1998-09-20 17:49:27+02 jv Exp $

use ExtUtils::MakeMaker 5.0;

require "./GetoptLong.pm";

WriteMakefile(
	NAME      => "Getopt::Long",
	DISTNAME  => 'GetoptLong',
	VERSION   => $Getopt::Long::VERSION,
	AUTHOR    => 'Johan Vromans (jvromans@squirrel.nl)',
	ABSTRACT  => 'extended processing of command line options', 
	INSTALLDIRS => 'perl',
	PM 	  => { "newgetopt.pl"  => '$(INST_LIB)/newgetopt.pl',
		       "GetoptLong.pm" => '$(INST_LIB)/Getopt/Long.pm'
		     },
	dist      => { COMPRESS => 'gzip', SUFFIX => 'gz', 
		     },
 );
