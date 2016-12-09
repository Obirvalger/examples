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

#my $f = Polynomial->new(k => $k, str => $str);
#my $g = Polynomial->new(k => $k, str => 'x^3 + x');
#say $t->polarize(2);
my $f = $t + 2*$h;
for my $d (0..$k-1) {
    say $h->polarize($d)->to_csv;
}
say '';
for my $d (0..$k-1) {
    say $t->polarize($d)->to_csv;
}
#say $f;
#say $f->show(one_function => 'f', );
#say $f->show(only_functions => 'f', tex => 1);
#say $t->fprint(only_funcs=>1, del=>'');
#say $t->init_str;
#say $t;
#say $t->polarize(2);
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
