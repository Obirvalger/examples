use strict;
use warnings;
use Polynomial;
use Test::More 'no_plan';
use Data::Printer;
use Math::Prime::Util qw(binomial znprimroot is_primitive_root powmod);

my $k = 5;

my $f = Polynomial->new(k => $k, d => 1, str => '(x+1)^4');

is($f->polarize(0), "x^4 + 4*x^3 + x^2 + 4*x + 1");
is($f->polarize(1), "(x+1)^4");
is($f->polarize(2), "(x+2)^4 + (x+2)^3 + (x+2)^2 + (x+2) + 1");
is($f->polarize(3), "(x+3)^4 + 2*(x+3)^3 + 4*(x+3)^2 + 3*(x+3) + 1");
is($f->polarize(4), "(x+4)^4 + 3*(x+4)^3 + 4*(x+4)^2 + 2*(x+4) + 1");


=begin  BlockComment  # BlockCommentNo_1
for my $k (3,5,7,11) {
    for my $d (0..$k-1) {
        my @summands;
        for my $i (reverse (0..$k-1)) {
            push @summands, -binomial($k-1, $i) * powmod(-$d,$i,$k) % $k;
        }
        print join(' + ', @summands), "\n";
    }
}
=end    BlockComment  # BlockCommentNo_1
=cut

