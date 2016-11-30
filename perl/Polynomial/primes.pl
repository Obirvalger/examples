#!/usr/bin/env perl
use strict;
use warnings;
use feature qw(say);

use Math::Prime::Util qw(is_prime next_prime);
use Getopt::Long;

my $n = $ARGV[0] // 2;

$n = next_prime($n) unless is_prime $n;

for  (;;) {
    say $n;
    $n = next_prime($n);
    last if defined $ARGV[1] && $n >= $ARGV[1];
}
