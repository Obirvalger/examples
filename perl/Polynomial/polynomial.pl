#! /usr/bin/perl -Ilib

use strict;
use warnings;
use feature "say";
use Polynomial qw(generate show_polynomial binom_mod);
use Data::Printer;
use Math::Prime::Util qw(binomial znprimroot is_primitive_root);
use Getopt::Long;

my $k = 5;
my $c;
my $d = 0;
my $str = 'f*x^4 + -g*x^4';

Getopt::Long::Configure ("bundling");
GetOptions (
    'k=i'   => \$k,
    'c=i'   => \$c,
    'd=i'   => \$d,
    'str=s' => \$str,
);

my $k_1 = $k-1;

$c //= znprimroot($k);

my $h = Polynomial->new(k => $k, 
    str => "h*x^$k_1 + $k_1*t*x^$k_1 + t*(x+$k_1)^$k_1");
my $t = Polynomial->new(k => $k, 
    str => "t*x^$k_1 + -$c*h*x^$k_1 + $c*h*(x+$k_1)^$k_1");
#say $str;

my $f = Polynomial->new(k => $k, str => $str);
#my $g = Polynomial->new(k => $k, str => 'x^3 + x');
#say $f;
say $t;
say $t->polarize(2);
#say $h;

__END__

say $funcs[0];
say $funcs[1];

is_all_complex(@funcs);

sub is_all_complex {
    my @funcs = @_;
    for my $d (0..$k-1) {
        for my $f (@funcs) {
#            say $f->polarize($d);
            if ((my $n = $f->polarize($d)->len) < $k) {
                say "Only $n summands in\n$f"; 
                return 0;
            }
        }
#        say '';
    }
    say "Ok";
}
