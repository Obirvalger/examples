#! /usr/bin/perl -Ilib

use strict;
use warnings;
use feature "say";
use SPolynomial qw(add_mul generate_all my_generate);
use Data::Printer;
use Math::Prime::Util qw(binomial znprimroot is_primitive_root);
use Getopt::Long;

our $k = 5;
our $c;

Getopt::Long::Configure ("bundling");
GetOptions (
    'k=i' => \$k,
    'c=i' => \$c
);

$c //= znprimroot($k);

#say $c;

#say Polynomial->new(k => $k, 
#my @funcs = generate_all("1.g;1.h","1.h;$c.g", $k);
my @funcs = my_generate("1.g;1.h","1.h;$c.g", $k);
#my @funcs = Polynomial::generate_all("1.g;1.h","1.h;$c.g", $k);

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
