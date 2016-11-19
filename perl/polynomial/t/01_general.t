use Test::More tests => 34;

use_ok('Polynomial');
can_ok('Polynomial', qw(new polarize mul clone len));

for my $k (5, 7, 11, 97) { # prime numbers 5, 7 are used, 11 first > 10, 97 big
    my $g = Polynomial->new(k => $k, gen => '1.g;1.h');
    my $f = Polynomial->new(k => $k, gen => '1.h;-1.g');
    my $h; # use as tmp polynomial later

    is($f, Polynomial->new(k => $k, gen => "1.h;@{[$k-1]}.g"), 
        "k = $k gen should work modulo k");

    $h = $f->clone;
    $h += $h;
    isnt($h, $f, "k = $k clone should work");

    $h = $g->clone;
    is($f->len, $k, "k = $k len should work for complex functions");
    
    $h->vector->[1]->{h} = 0;
    $h->vector->[2]->{g} = 0;
    is($h->len, $k-2, "k = $k len should work for non complex functions");

    is($f->mul(1), $f, "k = $k multiplication by 1 should be identity");
    is($g->mul($k+$k+1), $g, "k = $k multiplication should be modulo k");

    $h = $g->clone;
    for (1..$k-1) { $h += $g };
    is($g->mul(3), $h + $g+$g+$g,
        "k = $k multiplication by 3 should equal 3 times (modulo k) addition");

    is(add_mul($f, $g, $k+2), $f + $g->mul(2), "k = $k add_mul should work");
}

