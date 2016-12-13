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

#my $av = AOP->new(k => $k,
    #gens => ["$e*x^$k_1 + (x+1)^$k_1","$e*(x+1)^$k_1 + (x+2)^$k_1"]);
my $av = AOP->new(
    k => $k, 
    gens => ["$e*x^$k_1 + (x+1)^$k_1","x^$k_1 + $e*(x+1)^$k_1"]
);

#say join("\n", @{$av->gens});

my $as = AOP->new(k => $k, gens => [$hs, $ts]);

if ($s) {
    open(my $fh, '>', $s);
    say $fh $as->fprint(del=>';');
}

if ($v) {
    open(my $fh, '>', $v);
    say $fh $av->to_csv;
}

say $av->to_csv;
#say $as->to_tex_table(only_functions => 'f');
#say $as->to_tex_table;

#say $as->to_csv;
#warn $as->min_len(1) == $k ? "Ok all" : "Not ok all";
#say $av->is_any_group_complex(0) ? "Ok group" : "Not ok group";

