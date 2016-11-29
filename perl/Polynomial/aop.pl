#! /usr/bin/perl -Ilib

use strict;
use warnings;
use feature qw(say);

use Polynomial;
use AOP;
use Math::Prime::Util qw(binomial znprimroot is_primitive_root);
use Getopt::Long;

my $k = 5;
my $c;
my $e;
my $csv;

Getopt::Long::Configure ("bundling");
GetOptions (
    'k=i' => \$k,
    'c=i' => \$c,
    'e=i' => \$e,
    'csv' => \$csv,
#    'v'   => \$v,
#    't'   => \$t,
);

$c //= znprimroot($k);
$e //= -1*znprimroot($k) % $k;
my $k_1 = $k - 1;

my $hs =  "h*x^$k_1 + $k_1*t*x^$k_1 + t*(x+$k_1)^$k_1";
my $ts = "t*x^$k_1 + -$c*h*x^$k_1 + $c*h*(x+$k_1)^$k_1";

my $h = Polynomial->new(k => $k, 
    str => "h*x^$k_1 + $k_1*t*x^$k_1 + t*(x+$k_1)^$k_1");
my $t = Polynomial->new(k => $k, 
    str => "t*x^$k_1 + -$c*h*x^$k_1 + $c*h*(x+$k_1)^$k_1");

#my $a = AOP->new(k => 5,
#    gens => ["$e*x^$k_1 + (x+1)^$k_1","$e*(x+1)^$k_1 + (x+2)^$k_1"]);

my $a = AOP->new(k => $k, gens => [$hs, $ts]);
if ($csv) {
    say $a->to_csv;
} else {
    say $a;
}
