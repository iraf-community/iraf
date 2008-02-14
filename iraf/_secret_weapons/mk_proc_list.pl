#!/usr/bin/perl

use strict;

my $Dir;
my @Files;
my $i;
my $Num_files;

if ( $#ARGV < 0 ) {
    print "specify directory.\n";
    exit 1;
}

$Dir = $ARGV[0];

#print $Dir . "\n";

@Files = ();
$i = 0;

open(PFH,"find $Dir | grep -v '\\\.svn' | grep '\\.x\$' |");
while(<PFH>) {
    s/\n//g;
    $Files[$i] = $_;
    #print $Files[$i] . "\n";
    $i ++;
}
close(PFH);
$Num_files = $i;

for ( $i=0 ; $i < $Num_files ; $i++ ) {
    my $proc_name;
    my $args_str;
    my @args = ();
    my @arg_types = ();
    my $num_args;
    my $flag_decl = 0;
    #printf("$i $Files[$i]\n");
    open(FH,"cat $Files[$i] |");
    while(<FH>) {
	s/\#.*//g;
	if ( /[a-z]*[\s\t]*procedure[\s\t]+[a-z0-9_]+[\s\t]*\(/ ) {
	    my $j;
	    $proc_name = $_;
	    $proc_name =~ s/\(.*\n//;
	    $args_str = $_;
	    $args_str =~ s/.*\(//;
	    $args_str =~ s/\).*\n//;
	    #print $proc_name . "\n";
	    #print $args_str . "\n";
	    @args = split /[\s\t,]+/, $args_str;
	    @arg_types = ();
	    $num_args = $#args + 1;
	    for ( $j=0 ; $j < $num_args ; $j++ ) {
		$arg_types[$j] = "";
		#print "$j:" . $args[$j] . "\n";
	    }
	    if ( $proc_name =~ /[a-z]+/ ) {
		if ( $args_str =~ /[a-z]+/ ) {
		    $flag_decl = 1;
		}
	    }
	}
	elsif ( /[\s\t]*begin[\s\t]*\n/ ) {
	    if ( $flag_decl != 0 ) {
		my $j;
		$flag_decl = 0;
		print $proc_name . "( ";
		for ( $j=0 ; $j < $num_args ; $j++ ) {
		    print "$j:" . $arg_types[$j] . " " . $args[$j];
		    if ( $j < $num_args - 1 ) {
			print ", ";
		    }
		}
		print " )\n";
	    }
	}
	else {
	    if ( $flag_decl != 0 ) {
		my $j;
		#print "::" . $_ . "\n";
		for ( $j=0 ; $j < $num_args ; $j++ ) {
		    if ( $arg_types[$j] eq "" ) {
			my $s = $args[$j];
			if ( /[^a-zA-Z_]$s[^a-zA-Z_]/ ) {
			    #print ":::" . $_ . "\n";
			    $arg_types[$j] = $_;
			    $arg_types[$j] =~ s/^[\s\t]//;
			    $arg_types[$j] =~ s/[\s\t]+.*\n//;
			}
		    }
		}
	    }
	}
    }
    close(FH);
}

