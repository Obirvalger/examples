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
my $str = 'fx^4 + gx^3 + fx^2 + gx + f';

Getopt::Long::Configure ("bundling");
GetOptions (
    'k=i'   => \$k,
    'c=i'   => \$c,
    'str=s' => \$str,
);

$c //= znprimroot($k);

#say $str;

say SPolynomial->new(k => $k, str => $str);
#my @funcs = generate_all("1.g;1.h","1.h;$c.g", $k);
#my @funcs = my_generate("1.g;1.h","1.h;$c.g", $k);
#my @funcs = Polynomial::generate_all("1.g;1.h","1.h;$c.g", $k);

#my $f = generate(k => $k, gen => '1*g;1*h', type => 1);
#say $f;
#say $f->to_csv;

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
