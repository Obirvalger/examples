#! /usr/bin/perl -Ilib

use strict;
use warnings;
use feature "say";
use Polynomial qw(add_mul generate);
use Data::Printer;
use Math::Prime::Util qw(binomial znprimroot is_primitive_root);
use Getopt::Long;

our $k = 5;
our $c = 2;

Getopt::Long::Configure ("bundling");
GetOptions (
    'k=i' => \$k,
    'c=i' => \$c
);

my @funcs = generate("1.g;1.h","1.h;$c.g", $k);
say add_mul($funcs[0], $funcs[1], 2);
#for my $f (@funcs) { say $f };
#say $funcs[0];
#say $funcs[0]->polarize(1);


__END__

my @funcs = (
    "g*x^4 + h*x^3 + g*x^2 + h*x + g",
    "h*x^4 + 2*g*x^3 + h*x^2 + 2*g*x + h",
    "(g + h)*x^4 + (2*g + h)*x^3 + (g + h)*x^2 + (2*g + h)*x + (g + h)",
    "(g + 2*h)*x^4 + (4*g + h)*x^3 + (g + 2*h)*x^2 + (4*g + h)*x + (g + 2*h)",
    "(g + 3*h)*x^4 + (g + h)*x^3 + (g + 3*h)*x^2 + (g + h)*x + (g + 3*h)",
    "(g + 4*h)*x^4 + (3*g + h)*x^3 + (g + 4*h)*x^2 + (3*g + h)*x + (g + 4*h)",
);

my @polyns;
for my $fun (@funcs) {
    push @polyns, Polynomial->new(k => 5, poly => $fun);
}

my $f0 = Polynomial->new(
    k => 5,
    poly => 'g*x^4 + h*x^3 + g*x^2 + h*x + g',
);

my $f1 = Polynomial->new(
    k => 5,
    poly => 'h*x^4 + 2*g*x^3 + h*x^2 + 2*g*x + h',
);

my $f2 = Polynomial->new(
    k => 5,
    poly => '(g + h)*x^4 + (2*g + h)*x^3 + (g + h)*x^2 + (2*g + h)*x + (g + h)'
);

say $f0;
say $f1;
say $polyns[5];
say add_mul($f0,$f1,4);

#for my $i (0..4) {
#    say $f1;
#    say $f1->polarize($i);
#    say '';
#}

