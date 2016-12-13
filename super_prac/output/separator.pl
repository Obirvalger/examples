#!/usr/bin/env perl
#===============================================================================
#
#         FILE: separator.pl
#
#        USAGE: ./separator.pl  
#
#  DESCRIPTION: 
#
#      OPTIONS: ---
# REQUIREMENTS: ---
#         BUGS: ---
#        NOTES: ---
#       AUTHOR: Gordeev Mikhail (), gordmisha@gmail.com
# ORGANIZATION: 
#      VERSION: 1.0
#      CREATED: 11.12.2016 13:05:50
#     REVISION: ---
#===============================================================================

use v5.14;
use warnings;
use autodie;

open(my $pfh, '>', 'p.dat');
open(my $phi_fh, '>', 'phi.dat');

while (<>) {
    chomp;
    next if /[a-z]/;
    my @a = split /,/;
    say "@a";
    say $pfh "@a[0,1,2]";
    say $phi_fh "@a[0,1,3]";
}
