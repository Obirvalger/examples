#!/usr/bin/perl -Ilib
#===============================================================================
#
#         FILE: hypot.pl
#
#        USAGE: ./hypot.pl  
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
#      CREATED: 13.12.2016 15:02:12
#     REVISION: ---
#===============================================================================

use v5.14;
use warnings;
use Polynomial;
use AOP;
use Math::Prime::Util qw(primes znprimroot is_primitive_root);

for my $k (@{primes 60, 100}) {
    my $k_1 = $k-1;
    my $ok = 1;
    for my $e (2..$k-2) {
        my $av = AOP->new(
            k => $k, 
            gens => ["$e*x^$k_1 + (x+1)^$k_1","x^$k_1 + $e*(x+1)^$k_1"]
        );

        unless ($av->is_any_group_complex) {
            $ok = 0;
            print "$k ";
            say 'lol';
        }
    }
    say "Ok $k" if $ok;
}

