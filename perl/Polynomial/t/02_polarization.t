use strict;
use warnings;
use Polynomial;
use Math::Prime::Util qw(binomial znprimroot is_primitive_root);
use Test::More tests => 10;
use Data::Printer;

# NOTE Begin Vector
Vector: do {
    my $k = 5;
    my $f = Polynomial->new(k => $k, d => 1, str => '(x+1)^4');

    is($f->polarize(0), "x^4 + 4*x^3 + x^2 + 4*x + 1");
    is($f->polarize(1), "(x+1)^4");
    is($f->polarize(2), "(x+2)^4 + (x+2)^3 + (x+2)^2 + (x+2) + 1");
    is($f->polarize(3), "(x+3)^4 + 2*(x+3)^3 + 4*(x+3)^2 + 3*(x+3) + 1");
    is($f->polarize(4), "(x+4)^4 + 3*(x+4)^3 + 4*(x+4)^2 + 2*(x+4) + 1");
};

# NOTE Begin Symbolic
Symbolic: do {
    my $k = 5;
    my $k_1 = $k-1;
    my $c = znprimroot($k);
    #my  @funcs = generate_all("1.g;1.h","1.h;2.g", $k);
    #my $f1 = generate(gen => "1*h;2*g", k => $k, type => 2);
    my $h = Polynomial->new(k => $k, 
        str => 'h*x^4 + t*x^3 + t*x^2 + t*x + t');
    my $t = Polynomial->new(k => $k, 
        str => 't*x^4 + 2*h*x^3 + 2*h*x^2 + 2*h*x + 2*h');

    is($h->polarize(1),
        'h*(x+1)^4 + (h + t)*(x+1)^3 + (h + 3*t)*(x+1)^2 + (h + 2*t)*(x+1) + h',
    );

    is($t->polarize(2),
        't*(x+2)^4 + (2*h + 2*t)*(x+2)^3 + 4*t*(x+2)^2 + (3*h + 3*t)*(x+2) + t'
    );

    my $f0 = Polynomial->new(k => $k, str => 'gx^4 + hx^3 + gx^2 + hx + g');
    my $f1 = Polynomial->new(k => $k, str => 'hx^4 + 2gx^3 + hx^2 + 2gx + h');
    my $f4 = $f0 + 3*$f1;

    is($f0->polarize(1),
        'g*(x+1)^4 + (g + h)*(x+1)^3 + (2*g + 2*h)*(x+1)^2 + (4*g + 4*h)*(x+1) + (3*g + 3*h)',
        "k=$k f=2 p=2",
    );
        
    is($f1->polarize(2),
        'h*(x+2)^4 + (2*g + 2*h)*(x+2)^3 + 3*g*(x+2)^2 + (g + 4*h)*(x+2) + h',
        "k=$k f=2 p=2",
    );

    is($f4->polarize(3),
        '(g + 3*h)*(x+3)^4 + 4*g*(x+3)^3 + (g + h)*(x+3)^2 + (4*g + h)*(x+3) + (g + 3*h)',
        "k=$k f=2 p=2",
    );
};
