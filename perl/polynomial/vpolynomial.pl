#! /usr/bin/perl -Ilib

use strict;
use warnings;
use feature "say";
use VPolynomial qw(modulo_sum test_group_len show_polynomial);
use Data::Printer;
use Math::Prime::Util qw(binomial znprimroot is_primitive_root);
use Getopt::Long;
use Data::Printer;

my $k = 5;
my $c;
my $v = 0;
my $t = 0;

Getopt::Long::Configure ("bundling");
GetOptions (
    'k=i' => \$k,
    'c=i' => \$c,
    'v'   => \$v,
    't'   => \$t,
);

$c //= -1*znprimroot($k) % $k;
my $k_1 = $k-1;
my $f = VPolynomial->new(k => $k, str => "${c}x^$k_1 + (x+1)^$k_1");
my $g = VPolynomial->new(k => $k, str => "${c}(x+1)^$k_1 + (x+2)^$k_1");

say $f->init_str;
say $f;
#say VPolynomial->new(k => $k, str => 0);
#say show_polynomial(poly => $f, del => ';');

__END__

if (0) {
    for my $d (0..$k-1) {say $f->polarize($d)};
    say '';
    for my $d (0..$k-1) {say $g->polarize($d)};
    say '';
}

if ($t or $v) {
    for my $d (0..$k-1) {
        if ($t) {
            say $f->polarize($d)->csv;
            say $g->polarize($d)->csv;
            for my $c (1..$k-1) {
                my $h = ($f + $g->mul($c))->polarize($d);
                say $h->csv;
            }
            say '';
        }

#        say $f->_str;
        warn $f->polarize($d), "\n" if $v;
        warn $g->polarize($d), "\n\n" if $v;
    };
}

test_group_len($f, $g);


__END__

my @funcs = generate("1.g;1.h","1.h;$c.g", $k);

is_all_complex(@funcs);

sub is_all_complex {
    my @funcs = @_;
    for my $d (0..$k-1) {
        for my $f (@funcs) {
            if ((my $n = $f->polarize($d)->len) < $k) {
                say "Only $n summands in\n$f"; 
                return 0;
            }
        }
    }
    say "Ok";
}
