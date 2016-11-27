#! /usr/bin/perl -Ilib

use strict;
use warnings;
use feature "say";
use SPolynomial qw(generate show_polynomial);
use Data::Printer;
use Math::Prime::Util qw(binomial znprimroot is_primitive_root);
use Getopt::Long;

my $k = 5;
my $c;
my $d = 0;
my $str = 'fx^4 + gx^3 + fx^2 + gx + f';

Getopt::Long::Configure ("bundling");
GetOptions (
    'k=i'   => \$k,
    'c=i'   => \$c,
    'd=i'   => \$d,
    'str=s' => \$str,
);

$c //= znprimroot($k);

#say $str;

my $f = SPolynomial->new(k => $k, str => $str);
#say $f;
say $f->polarize($d);

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
