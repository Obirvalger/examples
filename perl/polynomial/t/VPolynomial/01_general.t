use strict;
use warnings;
use Math::Prime::Util qw(znprimroot);
use Test::More 'no_plan';

use_ok('VPolynomial', qw());
can_ok('VPolynomial', qw(new polarize mul clone len));

# syntax: "minimum{k};polynomial"
my @funcs = (
    '2;0', '2;1', '3;2', '5;4', '11;10',                              # 0
    '2;x', '3;2*x + 1', '5;4*x + 2', '11;10*x',                       # 1
    '3;x^2', '3;2*x^2 + 1', '5;4*x^2 + x', '11;6*x^2 + 5*x + 10',     # 2
    '5;4*x^3 + 1', '5;4*x^4 + x^3', '11;6*x^10 + 5*x^7 + 2*x^3 + 10', # many
);

for my $k (5,11) {
    for my $d (0..$k-1) {
        for my $nf (@funcs) {
            my ($n, $f) = split /;/, $nf;
            $f =~ s/x/(x+$d)/g if $d > 0;
            if ($k >= $n) {
                my $s = VPolynomial->new(k => $k, d => $d, str => $f);
                is($s, $f, "cycle k = $k d = $d f = $f");
            }
        }
    }
}

my $k = 67;
for my $d (0,1,5,10,21,48) {
#    my $f = '8*x^21 + 6*x^15 + 12*x^3 + 7';
    my $f = '60*x^64 + 6*x^55 + 12*x^30 + 7';
    $f =~ s/x/(x+$d)/g if $d > 0;
    my $s = VPolynomial->new(k => $k, d => $d, str => $f);
    is($s, $f, "big test k = $k d = $d");
}

for my $k (5,11) {
    for my $d (0..$k-1) {
        my $f = '2*x^4 + x^3 + 4*x + 2';
        $f =~ s/x/(x+$d)/g if $d > 0;
        is(VPolynomial->new(k => $k, d => $d, str => $f)->len, 4,
            "k = $k d = $d len should work");
    }
}

for my $k (3,5,11,13,19) {
    for my $d (0..$k-1) {
        my $e = -2 % $k;
        my $k_1 = $k - 1;

        my $f = "$e*x^$k_1 + (x+1)^$k_1";
        my $l = VPolynomial->new(k => $k, d => $d, str => $f)->len;
        ok($l >= $k-1, "$f len = $l");

        my $g = "$e*(x+1)^$k_1 + (x+2)^$k_1";
        $l = VPolynomial->new(k => $k, d => $d, str => $g)->len;
        ok($l >= $k-1, "$g len = $l");
    }
}



__END__

for my $k (5, 7, 11, 97) { # prime numbers 5, 7 are used, 11 first > 10, 97 big
    my $g = SPolynomial->new(k => $k, gen => '1.g;1.h');
    my $f = SPolynomial->new(k => $k, gen => '1.h;-1.g');
    my $h; # use as tmp polynomial later

    is($f, SPolynomial->new(k => $k, gen => "1.h;@{[$k-1]}.g"), 
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

