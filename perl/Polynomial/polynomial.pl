#! /usr/bin/perl -Ilib

use strict;
use warnings;
use feature "say";
use Polynomial qw(binom_mod);
use Data::Printer;
use Math::Prime::Util qw(binomial znprimroot is_primitive_root);
use Getopt::Long;

my $k = 5;
my $c;
my $e;
my $d = 0;
my $str;

Getopt::Long::Configure ("bundling");
GetOptions (
    'k=i'   => \$k,
    'c=i'   => \$c,
    'e=i'   => \$e,
    'd=i'   => \$d,
    'str=s' => \$str,
);

my $k_1 = $k-1;

$c //= znprimroot($k);
$e //= -znprimroot($k) % $k;
$str //= "-t*x^$k_1";

#my $h = Polynomial->new(k => $k,
#    str => "h*x^$k_1 + $k_1*t*x^$k_1 + t*(x+$k_1)^$k_1");
#my $t = Polynomial->new(k => $k,
#    str => "t*x^$k_1 + -$c*h*x^$k_1 + $c*h*(x+$k_1)^$k_1");

my $h = Polynomial->new(k => $k,
    str =>"$e*x^$k_1 + (x+1)^$k_1");
my $t = Polynomial->new(k => $k,
    str => "x^$k_1 + $e*(x+1)^$k_1");
#say $str;

my $f = Polynomial->new(k => $k, str => "$e*x^$k_1 + (x+1)^$k_1");
my $g = Polynomial->new(k => $k, str => "x^$k_1 + $e*(x+1)^$k_1");
say Polynomial->new(k =>  $k, str => "$e*x^$k_1 + (x-1)^$k_1");
#warn $f->init_str;
#warn $g->init_str;
#warn Polynomial->new(k => $k, str => "(x+1)^$k_1");

#for my $d (0..$k-1) {
    #say $f->polarize($d)->to_csv;
#}
#say '';
#for my $d (0..$k-1) {
    #say $g->polarize($d)->to_csv;
#}
