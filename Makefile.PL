# $Id: Makefile.PL,v 1.2 1996-07-10 16:18:50+02 jv Exp $

use ExtUtils::MakeMaker;

require "./GetoptLong.pm";

WriteMakefile(
	VERSION   => $Getopt::Long::VERSION,
	NAME      => "Getopt::Long",
	SKIP      => [qw(static dynamic)],
	linkext   => {LINKTYPE => ''},
	INSTALLDIRS => 'perl',
	PM 	  => { "newgetopt.pl"  => '$(INST_LIB)/newgetopt.pl',
		       "GetoptLong.pm" => '$(INST_LIB)/Getopt/Long.pm'
		     }

 );
