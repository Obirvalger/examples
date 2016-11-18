use strict;
use warnings;
use Polynomial qw(generate);
use Test::More tests => 3;
use Data::Printer;

my $k = 5;
my  @funcs = generate("1.g;1.h","1.h;2.g", $k);

is($funcs[0]->polarize(1),
    'g*(x+1)^4 + (g + h)*(x+1)^3 + (2*g + 2*h)*(x+1)^2 + (4*g + 4*h)*(x+1) + (3*g + 3*h)',
    'k=$k f=2 p=2',
);
    
is($funcs[1]->polarize(2),
    'h*(x+2)^4 + (2*g + 2*h)*(x+2)^3 + 3*g*(x+2)^2 + (g + 4*h)*(x+2) + h',
    'k=$k f=2 p=2',
);

is($funcs[4]->polarize(3),
    '(g + 3*h)*(x+3)^4 + 4*g*(x+3)^3 + (g + h)*(x+3)^2 + (4*g + h)*(x+3) + (g + 3*h)',
    'k=$k f=2 p=2',
);

