#! /usr/bin/perl -Ilib

use strict;
use warnings;
use feature qw(say);

use VPolynomial;
use SPolynomial;
use AOP;
use Math::Prime::Util qw(binomial znprimroot is_primitive_root);
use Getopt::Long;

my $k = 5;
my $c;

Getopt::Long::Configure ("bundling");
GetOptions (
    'k=i' => \$k,
    'c=i' => \$c,
#    'v'   => \$v,
#    't'   => \$t,
);

$c //= -1*znprimroot($k) % $k;
my $k_1 = $k - 1;

my $a = AOP->new(k => 5,
    gens => ["${c}x^$k_1 + (x+1)^$k_1","${c}(x+1)^$k_1 + (x+2)^$k_1"]);
say $a;
