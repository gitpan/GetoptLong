# GetOpt::Long.pm -- Universal options parsing

package Getopt::Long;

# RCS Status      : $Id: Base.pl,v 2.23 1998-09-25 10:36:34+02 jv Exp jv $
# Author          : Johan Vromans
# Created On      : Tue Sep 11 15:00:12 1990
# Last Modified By: Johan Vromans
# Last Modified On: Sat Sep 26 18:41:00 1998
# Update Count    : 725
# Status          : Released

################ Copyright ################

# This program is Copyright 1990,1998 by Johan Vromans.
# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation; either version 2
# of the License, or (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# If you do not have a copy of the GNU General Public License write to
# the Free Software Foundation, Inc., 675 Mass Ave, Cambridge, 
# MA 02139, USA.

################ Module Preamble ################

use strict;

BEGIN {
    require 5.004;
    use Exporter ();
    use vars     qw($VERSION @ISA @EXPORT @EXPORT_OK %EXPORT_TAGS);
    $VERSION     = "2.92";

    @ISA         = qw(Exporter);
    @EXPORT      = qw(&GetOptions $REQUIRE_ORDER $PERMUTE $RETURN_IN_ORDER);
    %EXPORT_TAGS = qw();
    @EXPORT_OK   = qw();
    use AutoLoader qw(AUTOLOAD);
}

# User visible variables.
use vars @EXPORT, @EXPORT_OK;
use vars qw($error $debug $major_version $minor_version);
# Deprecated visible variables.
use vars qw($autoabbrev $getopt_compat $ignorecase $bundling $order
	    $passthrough);
# Official invisible variables.
use vars qw($_genprefix $_useprefix $_terminator);

# Public subroutines. 
sub Configure (@);
sub config (@) { Configure @_; }	# deprecated name
sub GetOptions;

################ Local Variables ################

################ Resident subroutines ################

sub ConfigDefaults () {
    # Handle POSIX compliancy.
    if ( defined $ENV{"POSIXLY_CORRECT"} ) {
	$_genprefix = "(--|-)";
	$autoabbrev = 0;		# no automatic abbrev of options
	$getopt_compat = 0;		# disallow '+' to start options
	$order = $REQUIRE_ORDER;
    }
    else {
	$_genprefix = "(--|-|\\+)";
	$autoabbrev = 1;		# automatic abbrev of options
	$getopt_compat = 1;		# allow '+' to start options
	$order = $PERMUTE;
    }
    # Other configurable settings.
    $debug = 0;			# for debugging
    $bundling = 0;		# bundling off by default
    $ignorecase = 1;		# ignore case when matching options
    $passthrough = 0;		# leave unrecognized options alone
    $_useprefix = 0;		# match options including the prefix
    $_terminator = '--';	# end of options
}

################ Initialization ################

# Values for $order. See GNU getopt.c for details.
($REQUIRE_ORDER, $PERMUTE, $RETURN_IN_ORDER) = (0..2);
# Version major/minor numbers.
($major_version, $minor_version) = $VERSION =~ /^(\d+)\.(\d+)/;

# Set defaults.
ConfigDefaults ();

################ Package return ################

1;

__END__

################ AutoLoading subroutines ################

# RCS Status      : $Id: Autoloads.pl,v 2.24 1998-09-25 10:36:34+02 jv Exp jv $
# Author          : Johan Vromans
# Created On      : Fri Mar 27 11:50:30 1998
# Last Modified By: Johan Vromans
# Last Modified On: Sat Sep 26 19:07:37 1998
# Update Count    : 589
# Status          : Released

sub GetOptions {
    my @optionlist = @_;

    # Getopt::Long 2.x compatibility.

    my $userlinkage;
    my $prefix;

    # Check for ref HASH as first argument. 
    # First argument may be an object. It's OK to use this as long
    # as it is really a hash underneath. 
    if ( ref ($optionlist[0]) and
	 "$optionlist[0]" =~ /^(?:.*\=)?HASH\([^\(]*\)$/ ) {
	$userlinkage = shift (@optionlist);
	print STDERR ("=> user linkage: $userlinkage\n") if $debug;
    }

    # See if the first element of the optionlist contains option
    # starter characters.
    if ( $optionlist[0] =~ /^\W+$/ ) {
	$prefix = shift (@optionlist);
	# Turn into regexp. Needs to be parenthesized!
	$prefix =~ s/(\W)/\\$1/g;
	$prefix = "([" . $prefix . "])";
    }

    # Create a new Getopt::Long object.
    my $p = new Getopt::Long 
      -Linkage => $userlinkage,
      -Package => (caller(0))[0];

    # Configure, if required.
    $p->configure ("prefix_pattern=$prefix") if defined $prefix;

    # Process the option controls.
    $p->define (@optionlist);

    # Parse the command line and return the result.
    return $p->parse;
}

sub new {
    my $class = shift;
    Croak (__PACKAGE__."::new requires a class as its first argument\n")
      unless defined $class && index(ref($class), '=');

    print STDERR (__PACKAGE__."::new(@_)\n") if $debug;
    my %atts = @_;

    my $self = {};
    bless $self, $class;

    # Fill the object state with mostly a copy from the current environment.
    $self->{_linkage}         = {};
    $self->{_userlinkage}     = undef;
    $self->{_package}         = (caller)[0];
    $self->{_genprefix}       = $_genprefix,
    $self->{_terminator}      = $_terminator;
    $self->{autoabbrev}       = $autoabbrev;
    $self->{bundling}         = $bundling;
    $self->{debug}            = $debug;
    $self->{error}            = '';
    $self->{getopt_compat}    = $getopt_compat;
    $self->{ignorecase}       = $ignorecase;
    $self->{order}            = $order;
    $self->{passthrough}      = $passthrough;
    $self->{_useprefix}       = $_useprefix;
    $self->{_state_}          = undef;
    $self->{_opnames}         = (); # option names

    # Process attributes.
    if ( (defined %atts) && %atts ) {
	my $att;
	$self->{_package} = $att
	  if defined ($att = ($atts{Package} || $atts{-Package}));
	$self->{_userlinkage} = $att
	  if defined ($att = ($atts{Linkage} || $atts{-Linkage}));
	$self->configure(@$att) 
	  if defined ($att = ($atts{Config} || $atts{-Config}));
	$self->define(@$att)
	  if defined ($att = ($atts{Spec} || $atts{-Spec}));
    }

    $self;
}

sub define {

    my $self = shift(@_);
    Croak (__PACKAGE__."::define requires an object as its first argument\n")
      unless defined $self && index(ref($self), '=');

    my @speclist = @_;	# local copy of the option descriptions

    local $error = $self->{error} = '';
    my $autoabbrev            = $self->{autoabbrev};
    my $ignorecase            = $self->{ignorecase};
    my $bundling              = $self->{bundling};
    my $debug                 = $self->{debug};
    my $linkage               = $self->{_linkage};
    my $userlinkage           = $self->{_userlinkage};
    my $pkg                   = $self->{_package};
    my $prefix                = $self->{_genprefix};
    my $useprefix             = $self->{_useprefix};

    print STDERR (__PACKAGE__." $Getopt::Long::VERSION ",
		  'define $Revision: 2.24 $ ',
		  "called from package \"$pkg\".",
		  "\n  ",
		  "Spec: (@speclist)",
		  "\n  ",
		  "autoabbrev=$autoabbrev,",
		  "bundling=$bundling,",
		  "getopt_compat=", $self->{getopt_compat}, ",",
		  "order=", $self->{order}, ",",
		  "\n  ",
		  "ignorecase=$ignorecase,",
		  "passthrough=", $self->{passthrough}, ",",
		  "useprefix=$useprefix,",
		  "prefix=\"$prefix\",",
		  "terminator=\"", $self->{_terminator}, "\".",
		  "\n")
	if $debug;

    unless ( defined $self->{_state_} ) {
	$self->{_state_} = [ {}, # table of arg.specs (long and abbrevs)
			     {}, # table of arg.specs (bundles)
			     {}, # alias table
			   ];
    }
    my ($opctl, $bopctl, $aliases) = @{$self->{_state_}};

    # Verify correctness of speclist.
LOOP:
    while ( @speclist > 0 ) {
	my $opt = shift (@speclist);
	my $oref;
	$oref = shift (@speclist) if @speclist && ref($speclist[0]);

	# Filter out the specials.
	if ( $opt eq '<>' ) {
	    if ( (defined $userlinkage)
		&& !defined $oref
		&& (exists $userlinkage->{$opt})
		&& ref($userlinkage->{$opt}) ) {
		$oref = $userlinkage->{$opt};
	    }
	    unless ( defined $oref && ref($oref) eq 'CODE' ) {
		$error .= "Option spec <> requires a reference to a subroutine\n";
		next;
	    }
	    $linkage->{'<>'} = $oref;
	    next;
	}

	# Empty arg means "-" option.
	$opt = "-" if $opt eq "";

	# Break the option arg into option specs and control string.
	my $o = $opt;
	my $c = ['~', '$'];
	if ( $opt =~ m{^
                      (.+)		# 1 option name and aliases
		      ([=:])		# 2 takes required/optional value
		      ([binfs])		# 3 option type
		      ([\$\@%])?	# 4 optional destination type
		      (\(.*\))?		# 5 optional value list
		      (\{.*\})?		# 6 optional repetition
		      $}x ) {
	    $o = $1;
	    my $mmin = $2 eq '=' ? 1 : 0;
	    my $type  = $3;
	    my $dst   = $4;
	    my $vlist = $5;
	    my $rept  = $6;

	    # Range specified?
	    if ( defined $rept ) {
		# Must have array destination.
		if ( defined $dst && $dst ne '@' ) {
		    $error .= "Option \"$opt\": ".
		      "repetition requires the destination to be \@";
		    next LOOP;
		}
		unless ( $rept =~ /^\{(\d+)?(,(\d?)?)?\}$/ ) {
		    $error .= "Option \"$opt\": ".
		      "illegal repetition \"$rept\"\n";
		    next LOOP;
		}
		my ($min, $max) = ($1, $3);
		$min = $mmin unless defined $min;
		# {x} -> {x,x}
		$max = $min unless defined $2;
		# min <= max?
		if (defined $max && $max < $min ) {
		    $error .= "Option \"$opt\": ".
		      "error in repetition \"$rept\" -- ".
		      "max must be >= min\n";
		    next LOOP;
		}
		$c = [$type, '@', $min, $max];
	    }
	    else {
		$c = [$type, $dst || '$', $mmin, 1];
	    }

	    # Value list specified?
	    if ( defined $vlist ) {
		if ( $vlist =~ /^\((-?\d+)\.\.(-?\d+)\)$/) {
		    if ( $type ne 'i' && $type ne 'n' ) {
			$error .= "Option \"$opt\": ".
			  "range spec requires ".
			  "the type to be \"i\" or \"n\"\n";
			next LOOP;
		    }
		    if ( $1 > $2 ) {
			$error .= "Option \"$opt\": ".
			  "range spec error -- min > max\n";
			next LOOP;
		    }
		    $type = $c->[0] = 'r'; # range
		    push (@$c, [$1, $2]);
		}
		else {
		    my @list = split (' ', substr($vlist,1,length($vlist)-2));
		    if ( $^W && @list <= 1 ) {
			warn ("Option \"$opt\": ".
			      "very few values in value list $vlist\n");
		    }
		    push (@$c, [@list]);
		}
	    }
	}
	elsif ( $opt =~ /^(.+)([!+])$/ ) {
	    $o = $1;
	    $c = [$2, '$'];
	}
	# Other errors will be caught later.

	# Handle alias names
	my @o =  split (/\|/, $o);
	my $linko;
	my $a;

	foreach $o ( @o ) {
	    my $starter;

	    if ( $o eq '-' ) {
		# empty -> '-' option
		$o = '';
	    }
	    else {
		# First, strip leading prefix so people can specify "--foo=i".
		($starter,$o) = ($1,$+)
		  if $prefix ne '()' && $o =~ /^$prefix+(.*)$/s;
		if ( $useprefix && !defined $starter ) {
		    $error .= "Missing option prefix in spec: \"$o\"\n";
		    next LOOP;
		}
		# Then check for valid form.
		if ( $o !~ /^(\?|\w+[-\w]*)$/ ||
		     ($o eq '?' && !defined $linko)) {
		    $error .= "Error in option spec: \"$o\"";
		    $error .= " of \"$opt\"" unless $o eq $opt;
		    $error .= "\n";
		    next LOOP;
		}
	    }
	    $starter = '' unless ($useprefix && defined $starter);

	    # The first of the list of aliases is the real name.
	    unless ( defined $linko ) {
		$linko = $starter.$o;
		# Force an alias if the option name is not locase.
		$a = $o unless $o eq lc($o);
		$o = lc ($o)
		  if $ignorecase > 1 
		    || ($ignorecase
			&& ($bundling ? length($o) > 1  : 1));
	    }

	    if ( $bundling && length($o) == 1 ) {
		$o = lc ($o) if $ignorecase > 1;
		if ( $c->[0] eq '!' ) {
		    $opctl->{$starter."no$o"} = [ @$c ];
		    warn ("Ignoring '!' modifier for short option $o\n");
		    $c->[0] = '~';
		}
		$opctl->{$starter.$o} = $bopctl->{$starter.$o} = $c;
	    }
	    else {
		$o = lc ($o) if $ignorecase;
		if ( $c->[0] eq '!' ) {
		    $opctl->{$starter."no$o"} = [ @$c ];
		    $c->[0] = '~';
		}
		$opctl->{$starter.$o} = $c;
	    }
	    if ( defined $a ) {
		# Note alias.
		$aliases->{$o} = $a;
	    }
	    else {
		# Set primary name.
		$a = $o;
	    }
	}
	$o = $linko;

	# If no linkage is supplied in the @speclist, copy it from
	# the userlinkage if available.
	if ( defined $userlinkage && !defined $oref ) {
	    if ( exists $userlinkage->{$o} && ref($userlinkage->{$o}) ) {
		print STDERR ("=> found userlinkage for \"$o\": ",
			      "$userlinkage->{$o}\n")
		  if $debug;
		$oref = $userlinkage->{$o};
	    }
	    else {
		# Do nothing. Being undefined will be handled later.
		next;
	    }
	}
	    
	# Copy the linkage. If omitted, link to global variable.
	if ( defined $oref ) {
	    print STDERR ("=> link \"$o\" to $oref\n")
	      if $debug;
	    if ( ref($oref) =~ /^(SCALAR|CODE)$/ ) {
		$linkage->{$o} = $oref;
	    }
	    elsif ( ref($oref) =~ /^(ARRAY)$/ ) {
		$linkage->{$o} = $oref;
		$opctl->{$o}->[1] = $bopctl->{$o}->[1] = '@';
	    }
	    elsif ( ref($oref) =~ /^(HASH)$/ ) {
		$linkage->{$o} = $oref;
		$opctl->{$o}->[1] = $bopctl->{$o}->[1] = '%';
	    }
	    else {
		$error .= "Option \"$opt\": invalid linkage\n";
	    }
	}
	else {
	    if ( 0 ) {
		# Prepare to link to global $opt_XXX variable.
		$linkage->{$o} = undef; # exists, but not defined
	    }
	    else {
		no strict 'refs';
		my $ov = $o;
		$ov = $+ 
		  if $useprefix && $prefix ne '()' && $ov =~ /^$prefix+(.*)/s;
		$ov =~ s/\W/_/g;
		my $dsttype;
		$dsttype = '$' unless defined ($dsttype = $c->[1]);
		print STDERR ("=> link \"$o\" to $dsttype$pkg",
			      "::opt_$ov\n") if $debug;
		if ( $dsttype eq '@' ) {
		    $linkage->{$o} = \@{"$pkg"."::opt_$ov"};
		}
		elsif ( $dsttype eq '%' ) {
		    $linkage->{$o} = \%{"$pkg"."::opt_$ov"};
		}
		else {
		    $linkage->{$o} = \${"$pkg"."::opt_$ov"};
		}
	    }
	}
    }

    # Bail out if errors found.
    die ($error) if $error;

    # Show the options tables if debugging.
    if ( $debug ) {
	my ($arrow, $k, $v);
	$arrow = "=> ";
	while ( ($k,$v) = each(%$opctl) ) {
	    print STDERR ($arrow, "\$opctl->{\"$k\"} = ", opctl ($v), "\n");
	    $arrow = "   ";
	}
	$arrow = "=> ";
	while ( ($k,$v) = each(%$bopctl) ) {
	    print STDERR ($arrow, "\$bopctl->{\"$k\"} = ", opctl ($v),  "\n");
	    $arrow = "   ";
	}
	$arrow = "=> ";
	while ( ($k,$v) = each(%$aliases) ) {
	    print STDERR ($arrow, "\$aliases->{\"$k\"} = \"$v\"\n");
	    $arrow = "   ";
	}
    }

    # Clear list of names, if any.
    $self->{_opnames} = undef;

    # Return self so calls can be chained.
    $self;
}

sub opctl ($) {
    my ($ctl) = @_;
    my $res = '[';
    foreach ( @$ctl ) {
	if ( ref($_) eq 'ARRAY' ) {
	    $res .= opctl ($_);
	}
	else {
	    $res .= defined $_ ? $_ : 'u';
	}
	$res .= ' ';
    }
    chop ($res);
    $res . ']';
}

sub parse {

    my ($self, $argv) = @_;
    Croak (__PACKAGE__."::parse requires an object as its first argument\n")
      unless defined $self && index(ref($self), '=');

    local $error = $self->{error} = 0;

    my $order                 = $self->{order};
    my $terminator            = $self->{_terminator};
    my $debug                 = $self->{debug};
    my ($opctl, $bopctl, $aliases) = @{$self->{_state_}};

    $argv = \@main::ARGV unless defined $argv;

    print STDERR (__PACKAGE__." $Getopt::Long::VERSION ",
		  'parse $Revision: 2.24 $ ',
		  "called from package \"", $self->{_package}, "\".",
		  "\n  ",
		  "ARGV: (@$argv)",
		  "\n  ",
		  "autoabbrev=", $self->{autoabbrev}, ",".
		  "bundling=", $self->{bundling}, ",",
		  "getopt_compat=", $self->{getopt_compat}, ",",
		  "order=", $self->{order}, ",",
		  "\n  ",
		  "ignorecase=", $self->{ignorecase}, ",",
		  "passthrough=", $self->{passthrough}, ",",
		  "useprefix=", $self->{_useprefix}, ",",
		  "prefix=\"", $self->{_genprefix}, "\",",
		  "terminator=\"", $self->{_terminator}, "\".",
		  "\n")
	if $debug;

    # Sort the possible long option names.
    $self->{_opnames} = [ sort(keys (%$opctl)) ] 
      if $self->{autoabbrev}  && !defined $self->{_opnames};

    my @ret = ();		# accum for non-options
    my $opt;			# current option
    my $optctl;			# control info of option
    my $argcnt = 0;		# nr. arguments assigned
    my $min = 0;		# min. arguments required
    my $max;			# max. arguments allowed

    # Process argument list
    while ( @$argv > 0 ) {

	#### Get next argument ####

	my $tryopt = shift (@$argv);
	print STDERR ("=> option \"", $tryopt, "\"\n") if $debug;

	#### Determine what we have ####

	# Consume regardless.
	if ( $argcnt > 0 && 
	     defined $optctl && $optctl->[0] eq 's' && 
	     $min > $argcnt &&
	     !defined $optctl->[4]
	   ) {
	    $self->deposit ($opt, $tryopt, '@', '', '');
	    $argcnt++;
	    next;
	}

	# Check for option list terminator.
	if ( $tryopt eq $terminator ) {

	    if ( $argcnt < $min ) {
		$error++;
		warn ("Option \"$opt\": ".
		      ((defined $max && $max > $min) ? "at least " : "").
		      "$min arguments required, only $argcnt provided\n");
	    }

	    # Finish. Push back accumulated arguments and return.
	    last;
	}

	# Consume conditionally.
	if ( $argcnt > 0 && ((defined $max) ? ($argcnt < $max) : ($argcnt < $min)) ) {

	    my ($ok, $val, $extra, $msg) =
	      $self->valid ($optctl, $tryopt);
	    print STDERR ("=> valid ('",$optctl->[0],"','$tryopt',$min) -> ",
			  "($ok,'$val','$extra')\n") if $debug;
	    if ( $ok && $extra eq '' ) {
		$self->deposit ($opt, $val, '@', '', '');
		$argcnt++;
		next;
	    }
	    if ( $argcnt < $min ) {
		$error++;
		warn ("Option \"$opt\": ".
		      ((defined $max && $max > $min) ? "at least " : "").
		      "$min arguments required, only $argcnt provided\n");
	    }
	}

	$argcnt = $min = 0;

	my $result;		# success status
	my $key;		# key (if hash type)
	my $arg;		# option argument

	($result, $opt, $arg, $optctl, $key) = 
	  $self->findoption ($tryopt, $argv);

	if ( $result > 0 ) {
	    my $type;
	    my $dsttype;
	    ($type, $dsttype, $min, $max) = @$optctl;
	    my $incr = $type eq '+';

	    $opt = $aliases->{$opt} if defined $aliases->{$opt};
	    $self->deposit ($opt, $arg, $dsttype, $incr, $key);
	    $argcnt = 1;
	    $min = $max = 1 unless defined $min;
	}

	# Ignore unknown options here. 
	elsif ( $result < 0 ) {
	}

	# Not an option. Save it if we $PERMUTE and don't have a <>.
	elsif ( $order == $PERMUTE ) {
	    # Try non-options call-back.
	    my $cb;
	    if ( (defined ($cb = $self->{_linkage}->{'<>'})) ) {
		$cb->($tryopt);
	    }
	    else {
		print STDERR ("=> saving \"$tryopt\" ",
			      "(not an option, may permute) ",
			      "ret = (@ret)\n") if $debug;
		push (@ret, $tryopt);
	    }
	    next;
	}

	# ...otherwise, terminate.
	else {
	    # Push this one back and exit.
	    unshift (@$argv, $tryopt);
	    last;
	}

    }

    # Finish.
    if ( $order == $PERMUTE && @ret > 0 ) {
	#  Push back accumulated arguments
	print STDERR ("=> restoring \"", join('" "', @ret), "\"\n")
	    if $debug;
	unshift (@$argv, @ret);
    }

    return ($error == 0);
}

# Option lookup.
sub findoption ($$$) {

    # returns (1, $opt, $arg, $opctl, $key) if okay,
    # returns (-1) if the option is not valid
    # returns (0) otherwise.

    my ($self, $opt, $argv) = @_;
    my $prefix                = $self->{_genprefix};
    my $useprefix             = $self->{_useprefix};
    my $autoabbrev            = $self->{autoabbrev};
    my $getopt_compat         = $self->{getopt_compat};
    my $ignorecase            = $self->{ignorecase};
    my $bundling              = $self->{bundling};
    my $passthrough           = $self->{passthrough};
    my $debug                 = $self->{debug};
    my $terminator            = $self->{_terminator};

    my $key;			# hash key for a hash option
    my $arg;

    print STDERR ("=> find \"$opt\", prefix=\"$prefix\"\n") if $debug;

    return (0) unless $prefix eq '()' || $opt =~ /^$prefix(.*)$/s;
    my $starter = '';
    unless ( $useprefix ) {
	$opt = $+;
	$starter = $1;
	print STDERR ("=> split \"$starter\"+\"$opt\"\n") if $debug;
    }

    my ($opctl, $bopctl, $aliases) = @{$self->{_state_}};
    my $optarg = undef;	# value supplied with --opt=value
    my $rest = undef;	# remainder from unbundling

    # If it is a long option, it may include the value.
    if (((length($starter) > 1) || ($getopt_compat && !$bundling))
	&& $opt =~ /^([^=]+)=(.*)$/s ) {
	$opt = $1;
	$optarg = $2;
	print STDERR ("=> option \"", $opt, 
		      "\", optarg = \"$optarg\"\n") if $debug;
    }

    #### Look it up ###

    my $tryopt = $opt;		# option to try
    my $optbl = $opctl;		# table to look it up (long names)
    my $c;			# points to opctl info
    my $type;			# option type
    my $dsttype = '';		# destination type
    my $argreq;			# option requires an argument 
    my $incr = 0;		# increment, if type eq '+'
    my $min;			# min #args, if repeat specified
    my $max;			# max #args, if repeat specified

    if ( $bundling && length($starter) == 1 ) {
	# Unbundle single letter option.
	$rest = substr ($tryopt, 1);
	$tryopt = substr ($tryopt, 0, 1);
	$tryopt = lc ($tryopt) if $ignorecase > 1;
	print STDERR ("=> $starter$tryopt unbundled from ",
		      "$starter$tryopt$rest\n") if $debug;
	$rest = undef unless $rest ne '';
	$optbl = $bopctl;	# look it up in the short names table

	# If bundling == 2, long options can override bundles.
	if ( $bundling == 2 and defined $rest and
	     defined ($c = $opctl->{$tryopt.$rest}) ) {
	    print STDERR ("=> $starter$tryopt rebundled to ",
			  "$starter$tryopt$rest\n") if $debug;
	    $tryopt .= $rest;
	    undef $rest;
	}
    } 

    # Try auto-abbreviation. No use to try ''.
    elsif ( $autoabbrev && $opt ne '' ) {
	# Downcase if allowed.
	$tryopt = $opt = lc ($opt) if $ignorecase;
	# Turn option name into pattern.
	my $pat = quotemeta ($opt);
	# Look up in option names.
	my @hits = grep (/^$pat/, @{$self->{_opnames}});
	print STDERR ("=> ", scalar(@hits), " hits (@hits) with \"$pat\" ",
		      "out of ", scalar(@{$self->{_opnames}}), "\n") if $debug;

	# Check for ambiguous results.
	unless ( (@hits <= 1) || (grep ($_ eq $opt, @hits) == 1) ) {
	    # See if all matches are for the same option.
	    my %hit;
	    foreach ( @hits ) {
		$_ = $aliases->{$_} if defined $aliases->{$_};
		$hit{$_} = 1;
	    }
	    # Now see if it really is ambiguous.
	    unless ( keys(%hit) == 1 ) {
		return (0) if $passthrough;
		warn ("Option \"$opt\": ambiguous (",
		      join(", ", @hits), ")\n");
		$error++;
		return (-1);
	    }
	    @hits = keys(%hit);
	}

	# Complete the option name, if appropriate.
	if ( @hits == 1 && $hits[0] ne $opt ) {
	    $tryopt = $hits[0];
	    $tryopt = lc ($tryopt) if $ignorecase;
	    print STDERR ("=> option \"$opt\" -> \"$tryopt\"\n")
		if $debug;
	}
    }

    # Map to all lowercase if ignoring case.
    elsif ( $ignorecase ) {
	$tryopt = lc ($opt);
    }

    # Check validity by fetching the info.
    $c = $optbl->{$tryopt} unless defined $c;
    unless  ( defined $c ) {
	return (0) if $passthrough;
	warn ("Option \"$opt\": unknown\n");
	$error++;
	return (-1);
    }

    # Apparently valid.
    $opt = $tryopt;

    if ( $debug ) {
	print STDERR ("=> found ", opctl($c), " for ", $opt, "\n");
    }
    ($type, $dsttype, $min, $max) = @$c;

    # If we already have a value, repeat must be 0, 1 or undefined.
    if ( (defined $optarg || defined $rest) && defined $min && $min > 1 ) {
	return (0) if $passthrough;
	my $msg = "Option \"$opt\": ";
	if ( !defined $max ) {
	    $msg .= "at least $min argument";
	    $msg .= "s" unless $min == 1;
	}
	elsif ( $min == $max ) {
	    $msg .= $min == 1 ? "an argument" : "$min arguments";
	}
	elsif ( !defined $min ) {
	    $msg .= "at most $max argument";
	    $msg .= "s" unless $max == 1;
	}
	else {
	    $msg .= "between $min and $max arguments";
	}
	$msg .= " required\n";
	warn ($msg);
	$error++;
	return (-1);
    }

    #### Determine argument status ####

    # If it is an option w/o argument, we're almost finished with it.
    if ( $type eq '~' || $type eq '!' || $type eq '+' ) {
	if ( defined $optarg ) {
	    return (0) if $passthrough;
	    warn ("Option \"$opt\": no argument allowed\n");
	    $error++;
	    unshift (@$argv, $starter.$rest) if defined $rest;
	    return (-1);
	}
	elsif ( $type eq '~' || $type eq '+' ) {
	    $arg = 1;		# supply explicit value
	    $incr = $type eq '+';
	}
	else {
	    substr ($opt, 0, 2) = ''; # strip NO prefix
	    $arg = 0;		# supply explicit value
	}
	unshift (@$argv, $starter.$rest) if defined $rest;
	return (1, $opt, $arg, $c, $key);
    }

    # Check if there is an option argument available.
    if ( defined $optarg ? ($optarg eq '') 
	 : !(defined $rest || @$argv > 0) ) {
	# Complain if this option needs an argument.
	if ( $min > 0 ) {
	    return (0) if $passthrough;
	    warn ("Option \"$opt\": argument required\n");
	    $error++;
	    return (-1);
	}
	else {
	    $arg = $type eq "s" ? '' : 0;
	}
	return (1, $opt, $arg, $c, $key);
    }
    
    # Get (possibly optional) argument.
    $arg = (defined $rest ? $rest
	    : (defined $optarg ? $optarg : shift (@$argv)));

    # Get key if this is a "name=value" pair for a hash option.
    $key = undef;
    if ( $dsttype eq '%' && defined $arg ) {
	($key, $arg) = ($arg =~ /^(.*)=(.*)$/s) ? ($1, $2) : ($arg, 1);
    }

    #### Check if the argument is valid for this option ####

    my ($ok, $val, $extra, $msg) =
      $self->valid ($c, $arg, 
		    (defined $optarg || defined $rest) ? '()' : $prefix);

    print STDERR ("=> valid ('",$c->[0],"','$arg',$min) -> ",
		  "($ok,'$val','$extra')\n") if $debug;
    if ( !$ok ) {
	if ( $min > 0 ) {
	    if ( $passthrough ) {
		unshift (@$argv, defined $rest ? $starter.$rest : $arg)
		  unless defined $optarg;
		return (0);
	    }
	    warn ("Option \"$opt\": invalid value \"$arg\"$msg\n");
	    $error++;
	    unshift (@$argv, $starter.$rest) if defined $rest;
	    return (-1);
	}
	else {
	    unshift (@$argv, defined $rest ? $starter.$rest : $arg)
	      if defined $rest || defined $arg;
	}
	return (1, $opt, $val, $c, $key);
    }
    elsif ( $arg eq $terminator && !defined $optarg && !defined $rest &&
	    !($type eq 's' && $min > 0)
	  ) {
	print STDERR ("=> reject '$arg'\n") if $debug;
	unshift (@$argv, $arg);
	$val = '';
	return (1, $opt, $val, $c, $key);
    }
    elsif ( defined $optarg && $extra ne '' ) {
	if ( $passthrough ) {
	    return (0);
	}
	warn ("Option \"$opt\": invalid value \"$arg\"$msg\n");
	$error++;
	return (-1);
    }
    else {
	if ( $bundling && $extra ne '' ) {
	    unshift (@$argv, $starter.$extra);
	}
	return (1, $opt, $val, $c, $key);
    }
}

sub valid ($$$$) {
    my ($self, $opctl, $arg, $prefix) = @_;
    # Check if the argument is valid for this option.
    # Returns (found, the value, the rest of the arg, an error message)

    my $type = $opctl->[0];
    my $argreq = $opctl->[2] > 0;
    my $vlist = $opctl->[4];

    return (0, '', '', '') unless defined $arg;

    if ( $type eq "s" ) {	# string
	return (0, '', $arg, '') 
	  if !$argreq && 
	    $prefix ne '()' && $arg =~ /^$prefix/ &&
	      ($arg ne '-' || defined ($self->{_state_}->[0]->{""}));
	return (1, $arg, '', '') 
	  if !defined $vlist || scalar (grep {$_ eq $arg} @$vlist) > 0;
	return (0, '', $arg, 
		" (expecting " . join (" ", @$vlist) . ")");
    }

    if ( $type eq "n" || $type eq "i" ) { # numeric/integer
	if ( $arg =~ /^(-?[0-9]+)(.*)$/s ) {
	    return (1,$1,defined $+?$+:'','')
	      if !defined $vlist || scalar (grep {$_ == $1} @$vlist) > 0;
	    return (0, 0, $arg, 
		    " (expecting " . join (" ", @$vlist) . ")");
	}
	return (0, 0, $arg, " (number expected)");
    }

    if ( $type eq "f" ) { # real number, int is also ok
	# We require at least one digit before a point or 'e',
	# and at least one digit following the point and 'e'.
	# [-]NN[.NN][eNN]
	if ( $arg =~ /^(-?[0-9.]+(\.[0-9]+)?([eE]-?[0-9]+)?)(.*)$/s ) {
	    return (1,$1,defined $+?$+:'','')
	      if !defined $vlist || scalar (grep {$_ == $1} @$vlist) > 0;
	    return (0, 0, $arg, 
		    " (expecting " . join (" ", @$vlist) . ")");
	}
	elsif ( $arg =~ /^(-?[0-9]+)(.*)$/s ) {
	    return (1,$1,defined $+?$+:'','')
	      if !defined $vlist || scalar (grep {$_ == $1} @$vlist) > 0;
	    return (0, 0, $arg, 
		    " (expecting " . join (" ", @$vlist) . ")");
	}
	return (0, 0.0, $arg, " (real number expected)");
    }

    if ( $type eq 'r' ) {
	if ( $arg =~ /^(-?[0-9]+)(.*)$/s ) {
	    return (1,$1,defined $+?$+:'','')
	      if $1 >= $vlist->[0] && $1 <= $vlist->[1];
	    return (0, 0, $arg, " (number out of range " .
		    $vlist->[0] . " .. " . $vlist->[1] . ")");
	}
	return (0, 0, $arg, " (number expected)");
    }
    
    if ( $type eq 'b' ) {
	if ( $arg =~ /^(0*1|0x0*1|on|yes)(.*)$/is ) {
	    return (1, 1, defined $+?$+:'', '')
	      if !defined $vlist || 
		scalar (grep {lc($_) eq lc($1)} @$vlist) > 0;
	    return (0, 0, $arg, 
		    " (expecting " . join (" ", @$vlist) . ")");
	}
	if ( $arg =~ /^(0+|0x0+|off|no)(.*)$/is ) {
	    return (1, 0, defined $+?$+:'', '')
	      if !defined $vlist || 
		scalar (grep {lc($_) eq lc($1)} @$vlist) > 0;
	    return (0, 0, $arg, 
		    " (expecting " . join (" ", @$vlist) . ")");
	}
	return (0, 0, $arg, " (expecting 1 0 on off yes no)");
    }
    
    Croak (__PACKAGE__."::valid -- internal error!\n");
}

sub deposit_linkage ($$$$$$$) {

    my ($self, $linkage, $opt, $arg, $dsttype, $incr, $key) = @_;

    print STDERR ("=> ref(\$L{$opt}) -> ",
		  ref($linkage), "\n") if $debug;

    if ( ref($linkage) eq 'SCALAR' ) {
	if ( $incr ) {
	    print STDERR ("=> \$\$L{$opt} += \"$arg\"\n")
	      if $debug;
	    if ( defined ${$linkage} ) {
		${$linkage} += $arg;
	    }
	    else {
		${$linkage} = $arg;
	    }
	}
	else {
	    print STDERR ("=> \$\$L{$opt} = \"$arg\"\n")
	      if $debug;
	    ${$linkage} = $arg;
	}
    }
    elsif ( ref($linkage) eq 'ARRAY' ) {
	print STDERR ("=> push(\@{\$L{$opt}, \"$arg\")\n")
	  if $debug;
	push (@{$linkage}, $arg);
    }
    elsif ( ref($linkage) eq 'HASH' ) {
	print STDERR ("=> \$\$L{$opt}->{$key} = \"$arg\"\n")
	  if $debug;
	$linkage->{$key} = $arg;
    }
    elsif ( ref($linkage) eq 'CODE' ) {
	print STDERR ("=> &L{$opt}(\"$opt\", \"$arg\")\n")
	  if $debug;
	$linkage->($opt, $arg);
    }
    else {
	Croak (__PACKAGE__."::deposit_linkage -- internal error!\n" .
	       "Invalid REF type \"", ref($linkage), "\" in linkage\n");
    }
    return;
}


sub deposit ($$$$$$) {

    my ($self, $opt, $arg, $dsttype, $incr, $key) = @_;
    my $linkage;

    if ( defined ($linkage = $self->{_linkage}->{$opt}) ) {
	$self->deposit_linkage ($linkage, $opt, $arg, $dsttype, $incr, $key);
	return;
    }

    # No entry in linkage means entry in userlinkage.
    $linkage = $self->{_userlinkage}->{$opt};

    if ( defined $linkage && ref($linkage) ) {
	$self->deposit_linkage ($linkage, $opt, $arg, $dsttype, $incr, $key);
	return;
    }

    # Nothing defined yet or not a ref. Store.
    $linkage = $self->{_userlinkage};
    if ( $dsttype eq '@' ) {
	print STDERR ("=>\$L{$opt} = [\"$arg\"]\n")
	  if $debug;
	$linkage->{$opt} = [$arg];
    }
    elsif ( $dsttype eq '%' ) {
	print STDERR ("=>\$L{$opt} = {$key => \"$arg\"}\n")
	  if $debug;
	$linkage->{$opt} = {$key => $arg};
    }
    elsif ( $dsttype eq '$' ) {
	if ( $incr ) {
	    print STDERR ("=> \$L{$opt} += \"$arg\"\n")
	      if $debug;
	    if ( defined $linkage->{$opt} ) {
		$linkage->{$opt} += $arg;
	    }
	    else {
		$linkage->{$opt} = $arg;
	    }
	}
	else {
	    print STDERR ("=>\$L{$opt} = \"$arg\"\n") if $debug;
	    $linkage->{$opt} = $arg;
	}
    }
    else {
	Croak (__PACKAGE__."::deposit -- internal error!\n");
    }
}

# Getopt::Long Configuration.
sub Configure (@) {
    my (@options) = @_;

    print STDERR (__PACKAGE__."::Configure (@options)\n") if $debug;

    my $opt;
    foreach $opt ( @options ) {
	my $try = lc ($opt);
	my $action = 1;
	if ( $try =~ /^no_?(.*)$/s ) {
	    $action = 0;
	    $try = $+;
	}
	if ( $try eq 'default' or $try eq 'defaults' ) {
	    ConfigDefaults () if $action;
	}
	elsif ( $try eq 'auto_abbrev' or $try eq 'autoabbrev' ) {
	    $autoabbrev = $action;
	}
	elsif ( $try eq 'getopt_compat' ) {
	    $getopt_compat = $action;
	}
	elsif ( $try eq 'ignorecase' or $try eq 'ignore_case' ) {
	    $ignorecase = $action;
	}
	elsif ( $try eq 'ignore_case_always' ) {
	    $ignorecase = $action ? 2 : 0;
	}
	elsif ( $try eq 'bundling' ) {
	    $bundling = $action;
	}
	elsif ( $try eq 'bundling_override' ) {
	    $bundling = $action ? 2 : 0;
	}
	elsif ( $try eq 'require_order' ) {
	    $order = $action ? $REQUIRE_ORDER : $PERMUTE;
	}
	elsif ( $try eq 'permute' ) {
	    $order = $action ? $PERMUTE : $REQUIRE_ORDER;
	}
	elsif ( $try eq 'pass_through' or $try eq 'passthrough' ) {
	    $passthrough = $action;
	}
	elsif ( $try =~ /^terminator=(.+)$/ ) {
	    $_terminator = $1;
	}
	elsif ( $try =~ /^prefix=(.+)$/ ) {
	    $_genprefix = $1;
	    # Turn into regexp. Needs to be parenthesized!
	    $_genprefix = "(" . quotemeta($_genprefix) . ")";
	    eval { '' =~ /$_genprefix/; };
	    Croak (__PACKAGE__.": invalid pattern \"$_genprefix\"\n") if $@;
	}
	elsif ( $try =~ /^prefix_pattern=(.+)$/ ) {
	    $_genprefix = $1;
	    # Parenthesize if needed.
	    $_genprefix = "(" . $_genprefix . ")" 
	      unless $_genprefix =~ /^\(.*\)$/;
	    eval { '' =~ /$_genprefix/; };
	    Croak (__PACKAGE__.": invalid pattern \"$_genprefix\"\n") if $@;
	}
	elsif ( $try eq 'prefix' && !$action ) { # noprefix
	    $_genprefix = '()';
	}
	elsif ( $try eq 'useprefix' || $try eq 'use_prefix' ) {
	    $_useprefix = $action;
	}
	elsif ( $try eq 'debug' ) {
	    $debug = $action;
	}
	else {
	    Croak (__PACKAGE__.": unknown config parameter \"$opt\"\n")
	}
    }
    Croak (__PACKAGE__.": cannot use \"noprefix\" with \"useprefix\"\n")
      if $_useprefix && $_genprefix eq '()';
}

sub configure (@) {

    my $self = shift(@_);
    
    Croak (__PACKAGE__."::configure ".
	   "requires an object as its first argument\n")
      unless defined $self && index(ref($self), '=');

    print STDERR (__PACKAGE__." $Getopt::Long::VERSION ",
		  'configure $Revision: 2.24 $ ',
		  "called from package \"", $self->{_package}, "\".",
		  "\n  ",
		  "Config: (@_)",
		  "\n  ",
		  "autoabbrev=", $self->{autoabbrev}, ",".
		  "bundling=", $self->{bundling}, ",",
		  "getopt_compat=", $self->{getopt_compat}, ",",
		  "order=", $self->{order}, ",",
		  "\n  ",
		  "ignorecase=", $self->{ignorecase}, ",",
		  "passthrough=", $self->{passthrough}, ",",
		  "useprefix=", $self->{_useprefix}, ",",
		  "prefix=\"", $self->{_genprefix}, "\",",
		  "terminator=\"", $self->{_terminator}, "\".",
		  "\n")
	if $debug;

    # Copy config info to global variables.
    local $_genprefix         = $self->{_genprefix};
    local $_terminator        = $self->{_terminator};
    local $_useprefix         = $self->{_useprefix};
    local $autoabbrev         = $self->{autoabbrev};
    local $bundling           = $self->{bundling};
    local $debug              = $self->{debug};
    local $getopt_compat      = $self->{getopt_compat};
    local $ignorecase         = $self->{ignorecase};
    local $order              = $self->{order};
    local $passthrough        = $self->{passthrough};

    # Configure the global variables.
    Configure (@_);

    # If we have state, some configs may not change.
    if ( defined $self->{_state_} && 
	 ($self->{_genprefix}      ne $_genprefix ||
	  $self->{_useprefix}      != $_useprefix ||
	  $self->{autoabbrev}      != $autoabbrev ||
	  $self->{bundling}        != $bundling ||
	  $self->{getopt_compat}   != $getopt_compat ||
	  $self->{ignorecase}      != $ignorecase) ) {
	Croak (__PACKAGE__.": too late to configure\n");
    }

    # Copy config info from global variables.
    $self->{_genprefix}       = $_genprefix;
    $self->{_terminator}      = $_terminator;
    $self->{_useprefix}       = $_useprefix;
    $self->{autoabbrev}       = $autoabbrev;
    $self->{bundling}         = $bundling;
    $self->{getopt_compat}    = $getopt_compat;
    $self->{ignorecase}       = $ignorecase;
    $self->{debug}            = $debug;
    $self->{order}            = $order;
    $self->{passthrough}      = $passthrough;

    # Return self so calls can be chained.
    $self;
}

# To prevent Carp from being loaded unnecessarily.
sub Croak (@) {
    require 'Carp.pm';
    $Carp::CarpLevel = 1;
    Carp::croak(@_);
};

################ Documentation ################

=head1 NAME

Getopt::Long - extended processing of command line options

=head1 SYNOPSIS

  use Getopt::Long;
  $result = GetOptions (...option-descriptions...);

  my $parser = new Getopt::Long;
  $parser->define (...option-descriptions...);
  $parser->parse;

=head1 DESCRIPTION

The Getopt::Long module implements an extended getopt function called
GetOptions(). This function adheres to the POSIX syntax for command
line options, with GNU extensions. In general, this means that options
have long names instead of single letters, and are introduced with a
double dash "--". Support for bundling of command line options, as was
the case with the more traditional single-letter approach, is provided
but not enabled by default. For example, the UNIX "ps" command can be
given the command line "option"

  -vax

which means the combination of B<-v>, B<-a> and B<-x>. With the new
syntax B<--vax> would be a single option, probably indicating a
computer architecture. 

Command line options can be used to set values. These values can be
specified in one of two ways:

  --size 24
  --size=24

GetOptions is called with a list of option-descriptions, each of which
consists of two elements: the option specifier and the option linkage.
The option specifier defines the name of the option and, optionally,
the value it can take. The option linkage is usually a reference to a
variable that will be set when the option is used. For example, the
following call to GetOptions:

  GetOptions("size=i" => \$offset);

will accept a command line option "size" that must have an integer
value. With a command line of "--size 24" this will cause the variable
$offset to get the value 24.

Alternatively, the first argument to GetOptions may be a reference to
a HASH describing the linkage for the options, or an object whose
class is based on a HASH. The following call is equivalent to the
example above:

  %optctl = ("size" => \$offset);
  GetOptions(\%optctl, "size=i");

Linkage may be specified using either of the above methods, or both.
Linkage specified in the argument list takes precedence over the
linkage specified in the HASH.

The command line options are taken from array @ARGV. Upon completion
of GetOptions, @ARGV will contain the rest (i.e. the non-options) of
the command line.
 
Each option specifier designates the name of the option, optionally
followed by an argument specifier.

Options that do not take arguments will have no argument specifier. 
The option variable will be set to 1 if the option is used.

For the other options, the values for argument specifiers are:

=over 8

=item !

Option does not take an argument and may be negated, i.e. prefixed by
"no". E.g. "foo!" will allow B<--foo> (with value 1) and B<--nofoo>
(with value 0).
The option variable will be set to 1, or 0 if negated.

=item +

Option does not take an argument and will be incremented by 1 every
time it appears on the command line. E.g. "more+", when used with
B<--more --more --more>, will set the option variable to 3 (provided
it was 0 or undefined at first).

The B<+> specifier is ignored if the option destination is not a SCALAR.

=item =s

Option takes a mandatory string argument.
This string will be assigned to the option variable.
Note that even if the string argument starts with B<-> or B<-->, it
will not be considered an option on itself.

=item :s

Option takes an optional string argument.
This string will be assigned to the option variable.
If omitted, it will be assigned "" (an empty string).
If the string argument starts with B<-> or B<-->, it
will be considered an option on itself.

=item =i

Option takes a mandatory integer argument.
This value will be assigned to the option variable.
Note that the value may start with B<-> to indicate a negative
value. 

=item :i

Option takes an optional integer argument.
This value will be assigned to the option variable.
If omitted, the value 0 will be assigned.
Note that the value may start with B<-> to indicate a negative
value.

=item =f

Option takes a mandatory real number argument.
This value will be assigned to the option variable.
Note that the value may start with B<-> to indicate a negative
value.

=item :f

Option takes an optional real number argument.
This value will be assigned to the option variable.
If omitted, the value 0.0 will be assigned.

=item =b

Option takes a mandatory boolean argument.
Allowed values are B<true>, B<false>, B<yes>, B<no>, B<1> and B<0>.
The truth value will be assigned to the option variable.

=item :b

Option takes an optional boolean argument.
Allowed values are B<true>, B<false>, B<yes>, B<no>, B<1> and B<0>.
The truth value will be assigned to the option variable.
If omitted, the value 0 (false) will be assigned.

=back

A lone dash B<-> is considered an option, the corresponding option
name is the empty string.

A double dash on itself B<--> signals end of the options list.

=head2 Value lists

Options that are specified to take a value, can have a list of valid
values associated. Upon definition, the valid values are provided as a
parenthesized, whitespace separated list. E.g. B<"color=s(red yellow blue)">
defines option B<color> to require a string value that is either
B<"red">, B<"yellow"> or B<"blue">.

For numeric options, the value list may contain a range, e.g. B<"dim=i(1..3)">.

=head2 Repetitions

Options can be specified to take multiple values. The minimum and
maximum number of values are specified by appending
B<{>I<min>B<,>I<max>B<}> to the option specification. Default
repetition is B<{1}> for an option with a mandatory value, and
B<{0,1}> for an option with an optional value. Note that B<{1}>
means the same as B<{1,1}>, but B<{1,}> means "one or more" values.

If an option is to take more than one value, its destination must be
an array.

If an option uses a repetition and a value list, the repetition must
follow the value list.

=head2 Linkage specification

The linkage specifier is optional. If no linkage is explicitly
specified but a ref HASH (or object instance that is a blessed HASH)
is passed, GetOptions will place the value in the HASH. For example:

  %optctl = ();
  GetOptions (\%optctl, "size=i");

will perform the equivalent of the assignment

  $optctl{"size"} = 24;

For array options, a reference to an array is used, e.g.:

  %optctl = ();
  GetOptions (\%optctl, "sizes=i@");

with command line "-sizes 24 -sizes 48" will perform the equivalent of
the assignment

  $optctl{"sizes"} = [24, 48];

For hash options (an option whose argument looks like "name=value"),
a reference to a hash is used, e.g.:

  %optctl = ();
  GetOptions (\%optctl, "define=s%");

with command line "--define foo=hello --define bar=world" will perform the
equivalent of the assignment

  $optctl{"define"} = {foo=>'hello', bar=>'world')

If no linkage is explicitly specified and no ref HASH is passed,
GetOptions will put the value in a global variable named after the
option, prefixed by "opt_". To yield a usable Perl variable,
characters that are not part of the syntax for variables are
translated to underscores. For example, "--fpp-struct-return" will set
the variable $opt_fpp_struct_return. Note that this variable resides
in the namespace of the calling program, not necessarily B<main>.
For example:

  GetOptions ("size=i", "sizes=i@");

with command line "-size 10 -sizes 24 -sizes 48" will perform the
equivalent of the assignments

  $opt_size = 10;
  @opt_sizes = (24, 48);

A lone dash B<-> is considered an option, the corresponding Perl
identifier is $opt_ .

The linkage specifier can be a reference to a scalar, a reference to
an array, a reference to a hash or a reference to a subroutine.

Note that, if your code is running under the recommended C<use strict
'vars'> pragma, it may be helpful to declare these package variables
via C<use vars> perhaps something like this:

  use vars qw/ $opt_size @opt_sizes $opt_bar /;

If a REF SCALAR is supplied, the new value is stored in the referenced
variable. If the option occurs more than once, the previous value is
overwritten. 

If a REF ARRAY is supplied, the new value is appended (pushed) to the
referenced array. 

If a REF HASH is supplied, the option value should look like "key" or
"key=value" (if the "=value" is omitted then a value of 1 is implied).
In this case, the element of the referenced hash with the key "key"
is assigned "value". 

If a REF CODE is supplied, the referenced subroutine is called with
two arguments: the option name and the option value.
The option name is always the true name, not an abbreviation or alias.

=head2 Aliases and abbreviations

The option name may actually be a list of option names, separated by
"|"s, e.g. "foo|bar|blech=s". In this example, "foo" is the true name
of this option. If no linkage is specified, options "foo", "bar" and
"blech" all will set $opt_foo. For convenience, the single character
"?" is allowed as an alias, e.g. "help|?".

Option names may be abbreviated to uniqueness, depending on
configuration option B<auto_abbrev>.

=head2 Non-option call-back routine

A special option specifier, E<lt>E<gt>, can be used to designate a subroutine
to handle non-option arguments. GetOptions will immediately call this
subroutine for every non-option it encounters in the options list.
This subroutine gets the name of the non-option passed.
This feature requires configuration option B<permute>, see section
CONFIGURATION OPTIONS.

See also the examples.

=head2 Option starters

On the command line, options can start with B<-> (traditional), B<-->
(POSIX) and B<+> (GNU, now being phased out). The latter is not
allowed if the environment variable B<POSIXLY_CORRECT> has been
defined.

Options that start with "--" may have an argument appended, separated
with an "=", e.g. "--foo=bar".

=head2 Return values and Errors

Configuration errors and errors in the option definitions are
signalled using C<die()> and will terminate the calling
program unless the call to C<Getopt::Long::GetOptions()> was embedded
in C<eval { ... }> or C<die()> was trapped using C<$SIG{__DIE__}>.

A return value of 1 (true) indicates success.

A return status of 0 (false) indicates that the function detected one
or more errors during option parsing. These errors are signalled using
C<warn()> and can be trapped with C<$SIG{__WARN__}>.

Errors that can't happen are signalled using C<Carp::croak()>.

=head1 COMPATIBILITY

Getopt::Long::GetOptions() is the successor of
B<newgetopt.pl> that came with Perl 4. It is fully upward compatible.
In fact, the Perl 5 version of newgetopt.pl is just a wrapper around
the module.

If an "@" sign is appended to the argument specifier, the option is
treated as an array. Value(s) are not set, but pushed into array
@opt_name. If explicit linkage is supplied, this must be a reference
to an ARRAY.

If an "%" sign is appended to the argument specifier, the option is
treated as a hash. Value(s) of the form "name=value" are set by
setting the element of the hash %opt_name with key "name" to "value"
(if the "=value" portion is omitted it defaults to 1). If explicit
linkage is supplied, this must be a reference to a HASH.

If configuration option B<getopt_compat> is set (see section
CONFIGURATION OPTIONS), options that start with "+" or "-" may also
include their arguments, e.g. "+foo=bar". This is for compatiblity
with older implementations of the GNU "getopt" routine.

If the first argument to GetOptions is a string consisting of only
non-alphanumeric characters, it is taken to specify the option starter
characters. Everything starting with one of these characters from the
starter will be considered an option. B<Using a starter argument is
strongly deprecated.>

For convenience, option specifiers may have a leading B<-> or B<-->,
so it is possible to write:

   GetOptions qw(-foo=s --bar=i --ar=s);

=head1 EXAMPLES

If the option specifier is "one:i" (i.e. takes an optional integer
argument), then the following situations are handled:

   -one -two		-> $opt_one = '', -two is next option
   -one -2		-> $opt_one = -2

Also, assume specifiers "foo=s" and "bar:s" :

   -bar -xxx		-> $opt_bar = '', '-xxx' is next option
   -foo -bar		-> $opt_foo = '-bar'
   -foo --		-> $opt_foo = '--'

In GNU or POSIX format, option names and values can be combined:

   +foo=blech		-> $opt_foo = 'blech'
   --bar=		-> $opt_bar = ''
   --bar=--		-> $opt_bar = '--'

Example of using variable references:

   $ret = GetOptions ('foo=s', \$foo, 'bar=i', 'ar=s', \@ar);

With command line options "-foo blech -bar 24 -ar xx -ar yy" 
this will result in:

   $foo = 'blech'
   $opt_bar = 24
   @ar = ('xx','yy')

Example of using the E<lt>E<gt> option specifier:

   @ARGV = qw(-foo 1 bar -foo 2 blech);
   GetOptions("foo=i", \$myfoo, "<>", \&mysub);

Results:

   mysub("bar") will be called (with $myfoo being 1)
   mysub("blech") will be called (with $myfoo being 2)

Compare this with:

   @ARGV = qw(-foo 1 bar -foo 2 blech);
   GetOptions("foo=i", \$myfoo);

This will leave the non-options in @ARGV:

   $myfoo -> 2
   @ARGV -> qw(bar blech)

=head1 CONFIGURATION OPTIONS

B<GetOptions> can be configured by calling subroutine
B<Getopt::Long::Configure>. This subroutine takes a list of quoted
strings, each specifying a configuration option to be set, e.g.
B<ignore_case>. Options can be reset by prefixing with B<no_>, e.g.
B<no_ignore_case>. Case does not matter. Multiple calls to B<Configure>
are possible.

Previous versions of Getopt::Long used variables for the purpose of
configuring. Although manipulating these variables still work, it
is strongly encouraged to use the new B<Configure> routine. Besides, it
is much easier.

The following options are available:

=over 12

=item default

This option causes all configuration options to be reset to their
default values.

=item auto_abbrev

Allow option names to be abbreviated to uniqueness.
Default is set unless environment variable
POSIXLY_CORRECT has been set, in which case B<auto_abbrev> is reset.

=item getopt_compat   

Allow '+' to start options.
Default is set unless environment variable
POSIXLY_CORRECT has been set, in which case B<getopt_compat> is reset.

=item require_order

Whether non-options are allowed to be mixed with
options.
Default is set unless environment variable
POSIXLY_CORRECT has been set, in which case b<require_order> is reset.

See also B<permute>, which is the opposite of B<require_order>.

=item permute

Whether non-options are allowed to be mixed with
options.
Default is set unless environment variable
POSIXLY_CORRECT has been set, in which case B<permute> is reset.
Note that B<permute> is the opposite of B<require_order>.

If B<permute> is set, this means that 

    -foo arg1 -bar arg2 arg3

is equivalent to

    -foo -bar arg1 arg2 arg3

If a non-option call-back routine is specified, @ARGV will always be
empty upon succesful return of GetOptions since all options have been
processed, except when B<--> is used:

    -foo arg1 -bar arg2 -- arg3

will call the call-back routine for arg1 and arg2, and terminate
leaving arg2 in @ARGV.

If B<require_order> is set, options processing
terminates when the first non-option is encountered.

    -foo arg1 -bar arg2 arg3

is equivalent to

    -foo -- arg1 -bar arg2 arg3

=item bundling (default: reset)

Setting this variable to a non-zero value will allow single-character
options to be bundled. To distinguish bundles from long option names,
long options must be introduced with B<--> and single-character
options (and bundles) with B<->. For example,

    ps -vax --vax

would be equivalent to

    ps -v -a -x --vax

provided "vax", "v", "a" and "x" have been defined to be valid
options. 

Bundled options can also include a value in the bundle; for strings
this value is the rest of the bundle, but integer and floating values
may be combined in the bundle, e.g.

    scale -h24w80

is equivalent to

    scale -h 24 -w 80

Note: resetting B<bundling> also resets B<bundling_override>.

=item bundling_override (default: reset)

If B<bundling_override> is set, bundling is enabled as with
B<bundling> but now long option names override option bundles. In the
above example, B<-vax> would be interpreted as the option "vax", not
the bundle "v", "a", "x".

Note: resetting B<bundling_override> also resets B<bundling>.

B<Note:> Using option bundling can easily lead to unexpected results,
especially when mixing long options and bundles. Caveat emptor.

=item ignore_case  (default: set)

If set, case is ignored when matching options.

Note: resetting B<ignore_case> also resets B<ignore_case_always>.

=item ignore_case_always (default: reset)

When bundling is in effect, case is ignored on single-character
options also. 

Note: resetting B<ignore_case_always> also resets B<ignore_case>.

=item pass_through (default: reset)

Unknown options are passed through in @ARGV instead of being flagged
as errors. This makes it possible to write wrapper scripts that
process only part of the user supplied options, and passes the
remaining options to some other program.

This can be very confusing, especially when B<permute> is also set.

=item prefix=I<value>

The string that starts options. See also B<prefix_pattern>.

=item prefix_pattern=(I<pattern>)

A Perl pattern that identifies the strings that introduce options.
Default is C<(--|-|\+)> unless environment variable
POSIXLY_CORRECT has been set, in which case it is C<(--|-)>.

=item use_prefix  (default: reset)

When set, options are looked up B<including the prefix>. This makes it
possible to define different actions for e.g. B<-a> and B<+a>.

When using B<use_prefix>, all options must use it.

=item no_prefix  (default: reset)

No prefix is used to introduce options. This can be useful to parse
option files that have lines containing just names and values.

=item terminator=I<value>  (default: "--")

A constant string to indicate the end of the options in @ARGV. 

=item debug (default: reset)

Enable copious debugging output.

=back

=head1 OTHER INFORMATION

=head2 Autoloading

As of version 2.17, Getopt::Long uses autoloading. This substantially
reduces the resources required to 'use Getopt::Long' (about 100 lines
of over 1300 total).

A bug in Perl 5.005 causes autoloading modules to fail if used with
autouse. E.g.

  use autouse 'Getopt::Long' => qw(GetOptions);

There's no need to use autouse with Getopt::Long.

=head2 Object Orientation

  my $parser = new Getopt::Long Config  => [qw(default ignore_case)],
                                Spec    => [qw(foo=s bar=i(4,,7) blech!)],
                                Package => "foo",
                                Linkage => \%myhash;
  $parser->define qw(foo=s bar=i(4,,7) blech!);
  $parser->configure qw(default ignore_case);
  $parser->parse (\@ARGV);

Note that configure() sets instance configuration, while Configure
sets the class configuration. Upon instantiation, the class
configuration is copied into the instance.

Call-back routines may safely use the class variable B<$error> since
it will reflect the instance's value.

=over 4

=item *

You cannot pass a linkage HASH ref as the first argument to method
define(), neither can you set the option prefix with a non-word first
argument. 

=back

=head2 Option Prefix

If the option prefix is a pattern that allows for one-character as
well as multi-character length prefixes, then

=over 4

=item *

The multi-character prefixes can be used to include a value in the
option, e.g. B<--foo=bar>.

=item *

In the case of bundling, the single-letter prefixes introduce the
single-character options (e.g. B<-a>, B<-b>) while the multi-character
prefixes introduce the long options (e.g. B<--length>).

=back

=head2 Miscellaneous

If the linkage argument for an integer option is a ref HASH, 

  -xx aa=2             $link->{aa} = 2       as expected
  -xx aa               $link->{aa} = 1       default arg, okay
  -xx 2                $link->{2} = 1        !!

=head1 OTHER USEFUL VARIABLES

=over 12

=item $Getopt::Long::VERSION

The version number of this Getopt::Long implementation in the format
C<major>.C<minor>. This can be used to have Exporter check the
version, e.g.

    use Getopt::Long 3.00;

You can inspect $Getopt::Long::major_version and
$Getopt::Long::minor_version for the individual components.

=item $Getopt::Long::error

Internal error flag. May be incremented from a call-back routine to
cause options parsing to fail.

=back

=head1 AUTHOR

Johan Vromans E<lt>jvromans@squirrel.nlE<gt>

=head1 COPYRIGHT AND DISCLAIMER

This program is Copyright 1990,1998 by Johan Vromans.
This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

If you do not have a copy of the GNU General Public License write to
the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
Boston, MA 02111-1307 USA.

=cut
