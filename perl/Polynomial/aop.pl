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
my $v;
my $s;
my $csv;

Getopt::Long::Configure ("bundling");
GetOptions (
    'k=i' => \$k,
    'c=i' => \$c,
    'e=i' => \$e,
    'csv' => \$csv,
    'v=s'   => \$v,
    's=s'   => \$s,
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

my $av = AOP->new(k => $k, gens => ["$e*x^$k_1 + (x+1)^$k_1","$e*(x+1)^$k_1 + (x+2)^$k_1"]);

my $as = AOP->new(k => $k, gens => [$hs, $ts]);

if ($s) {
    open(my $fh, '>', $s);
    say $fh $as->fprint(del=>';');
}

if ($v) {
    open(my $fh, '>', $v);
    say $fh $av->to_csv;
}

say $as->to_csv(one_function => 'f');
#$as->fmap(sub {say $_[0]});
#$as->fmap(sub {print $_[0]->fprint(del => '', only_funcs => 1)}, sub {print ';'});

#say $as->min_len(1) == $k ? "Ok all" : "Not ok all";
#say $av->is_any_group_complex(1) ? "Ok group" : "Not ok group";

#say "\\documentclass[a4paper, 12pt]{extarticle}\n\\begin{document}";

#say $as->show(
#    sep => "\\\\ \n", 
#    around => ['$$\begin{array}{l}','\end{array}$$'],
#    poly_show => {mul => '', tex => 1}
#);

#say '\end{document}';
#$h->polynomial;
#say $h->funcs;
#say $as->polynomials->[0][0];
#say $as->polynomials->[0][0]->funcs;
#say $as->polynomials->[0][0]->fprint;
#say $as->polynomials->[0][1];
