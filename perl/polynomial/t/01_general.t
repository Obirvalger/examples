use Test::More tests => 3;

use_ok('Polynomial');
can_ok('Polynomial', qw(new polarize));

my $k = 5;
my $g = Polynomial->new(
    k => 5,
    poly => 'g*x^4 + h*x^3 + g*x^2 + h*x + g',
);
my $f = Polynomial->new(
    k => 5,
    poly => 'h*x^4 + 2*g*x^3 + h*x^2 + 2*g*x + h',
);
is($f->mul(1), $f, "multiplication by 1 should be identity");
