#!/usr/bin/perl -Ilib
use v5.14;
use warnings;
use Polynomial;
use AOP;
use Math::Prime::Util qw(primes znprimroot is_primitive_root);

my $primes;
if ($ARGV[1]) {
    $primes = primes @ARGV;
} else {
    $primes = primes 5, $ARGV[0] || 5;
}

#say $ARGV[0] || 5;
#say join("\n", @primes);

for my $k (@$primes) {
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
        } else {
            say "k = $k e = $e";
        }
    }
    say "Ok $k" if $ok;
}

