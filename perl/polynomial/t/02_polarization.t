use Polynomial;
use Test::More 'no_plan';

my $k = 5;
my @funcs = (
    "g*x^4 + h*x^3 + g*x^2 + h*x + g",
    "h*x^4 + 2*g*x^3 + h*x^2 + 2*g*x + h",
    "(g + h)*x^4 + (2*g + h)*x^3 + (g + h)*x^2 + (2*g + h)*x + (g + h)",
    "(g + 2*h)*x^4 + (4*g + h)*x^3 + (g + 2*h)*x^2 + (4*g + h)*x + (g + 2*h)",
    "(g + 3*h)*x^4 + (g + h)*x^3 + (g + 3*h)*x^2 + (g + h)*x + (g + 3*h)",
    "(g + 4*h)*x^4 + (3*g + h)*x^3 + (g + 4*h)*x^2 + (3*g + h)*x + (g + 4*h)",
);

is(Polynomial->new(k => 5, poly => $funcs[0])->polarize(1),
    'g*(x+1)^4 + (g + h)*(x+1)^3 + (2*g + 2*h)*(x+1)^2 + (4*g + 4*h)*(x+1) + (3*g + 3*h)',
    'k=5 f=2 p=2',
);
    
is(Polynomial->new(k => 5, poly => $funcs[1])->polarize(2),
    'h*(x+2)^4 + (2*g + 2*h)*(x+2)^3 + 3*g*(x+2)^2 + (g + 4*h)*(x+2) + h',
    'k=5 f=2 p=2',
);

is(Polynomial->new(k => 5, poly => $funcs[4])->polarize(3),
    '(g + 3*h)*(x+3)^4 + 4*g*(x+3)^3 + (g + h)*(x+3)^2 + (4*g + h)*(x+3) + (g + 3*h)',
    'k=5 f=2 p=2',
);
