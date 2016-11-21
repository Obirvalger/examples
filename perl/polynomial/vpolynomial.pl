#! /usr/bin/perl -Ilib

use strict;
use warnings;
use feature "say";
use VPolynomial qw(modulo_sum);
use Data::Printer;
use Math::Prime::Util qw(binomial znprimroot is_primitive_root);
use Getopt::Long;
use Data::Printer;

our $k = 5;
our $c;

Getopt::Long::Configure ("bundling");
GetOptions (
    'k=i' => \$k,
    'c=i' => \$c
);

$c //= znprimroot($k);

#say modulo_sum();
say VPolynomial->new(k => 5, str => '2x^2 + x^3');
say VPolynomial->new(k => 5, str => '0');
say VPolynomial->new(k => 5, d => 1, str => '2x^4 + (x+1)^4');


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
