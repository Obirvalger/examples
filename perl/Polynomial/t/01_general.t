use strict;
use warnings;
use Polynomial;
use Math::Prime::Util qw(binomial znprimroot is_primitive_root);
use Data::Printer;
use Test::More 'no_plan';

use_ok('Polynomial', qw());
can_ok('Polynomial', qw(new polarize clone len zeros));

# NOTE Begin Vector
Vector: do {
    # syntax: "minimum{k};polynomial"
    my @funcs = (
        #'2;0', 
        '2;1', '3;2', '5;4', '11;10',                              # 0
        '2;x', '3;2*x + 1', '5;4*x + 2', '11;10*x',                      # 1
        '3;x^2', '3;2*x^2 + 1', '5;4*x^2 + x', '11;6*x^2 + 5*x + 10',    # 2
        '5;4*x^3 + 1', '5;4*x^4 + x^3','11;6*x^10 + 5*x^7 + 2*x^3 + 10', # many
    );

    for my $k (5,11) {
        for my $d (0..$k-1) {
            for my $nf (@funcs) {
                my ($n, $f) = split /;/, $nf;
                $f =~ s/x/(x+$d)/g if $d > 0;
                if ($k >= $n) {
                    my $s = Polynomial->new(k => $k, d => $d, str => $f);
                    is($s, $f, "cycle vec k = $k d = $d f = $f");
                }
            }
        }
    }

    for my $k (5,11) {
        for my $d (0..$k-1) {
            my $f = '2*x^4 + x^3 + 4*x + 2';
            $f =~ s/x/(x+$d)/g if $d > 0;
            is(Polynomial->new(k => $k, d => $d, str => $f)->len, 4,
                "k = $k d = $d len should work");
        }
    }

    for my $k (3,5,11,13,19) {
        my $k_1 = $k - 1;

        is_deeply (Polynomial->new(k => $k, 
                str => "(x-1)^$k_1 + -x^2 + -x")->zeros, [1,2]);
        
        for my $d (0..$k-1) {
            my $e = -2 % $k;
            my $f = "$e*x^$k_1 + (x+1)^$k_1";
            my $l = Polynomial->new(k => $k, d => $d, str => $f)->len;
            ok($l >= $k-1, "$f len = $l");

            my $g = "$e*(x+1)^$k_1 + (x+2)^$k_1";
            $l = Polynomial->new(k => $k, d => $d, str => $g)->len;
            ok($l >= $k-1, "$g len = $l");
        }
    }
};
# NOTE Begin Symbolic
Symbolic: do {
    my $use_big_test = 1;

    #big k neg test
    for my $k (qw(17 23 41 67 97 163)) {
        my $k_1 = $k-1;
    #    ok(1);
        is(Polynomial->new(k => $k, str => "-fx"), "$k_1*f*x",
            "Big negative powmod");
    }

    for my $k (5, 7, 11, 67) { # prime numbers 5, 7 are used, 11 first > 10, 67 big
        my $k_1 = $k-1;

        is(Polynomial->new(k => $k, str => "f*x + g*x"), "(f + g)*x");
        is(Polynomial->new(k => $k, str => "f*x + 2*g*x"), "(f + 2*g)*x");
        is(Polynomial->new(k => $k, str => "f*x + $k*g*x"), "f*x");
        is(Polynomial->new(k => $k, str => "f*x + -g*x"), "(f + $k_1*g)*x");

        

        my $c = znprimroot($k);
        #    my $g = Polynomial->new(k => $k, gen => '1.g;1.h');
        #    my $f = Polynomial->new(k => $k, gen => '1.h;-1.g');
        my $s = Polynomial->new(k => $k, 
            str => "s*x^$k_1 + $k_1*t*x^$k_1 + t*(x+$k_1)^$k_1");
        my $t = Polynomial->new(k => $k, 
            str => "t*x^$k_1 + -$c*s*x^$k_1 + $c*s*(x+$k_1)^$k_1");

        #    my $g = generate(k => $k, gen => '1*g;1*h', type => 2);
        #    my $f = generate(k => $k, gen => '1*h;-1*g', type => 2);
        my $h; # use as tmp polynomial later

        #    is($f, generate(k => $k, gen => "1*h;@{[$k-1]}*g", type => 2), 
        #        "k = $k gen should work modulo k");

        $h = $t->clone;
        $h += $h;
        isnt($h, $t, "k = $k clone should work");

        $h = $s->clone;
        is($t->len, $k, "k = $k len should work for complex functions");
        
        $h = Polynomial->new(k => $k, str => 'g*x^2 + f*x');
        is($h->len, 2, "k = $k len should work for non complex functions");

        is(1*$t, $t, "k = $k multiplication by 1 should be identity");
        is($s * ($k+$k+1), $s, "k = $k multiplication should be modulo k");

        $h = $s->clone;
        for (1..$k-1) { $h += $s };
        is(3*$s, $h + $s+$s+$s,
            "k = $k multiplication by 3 should equal 3 times (modulo k) addition");

        # is(add_mul($t, $s, $k+2), $t + $s->mul(2), "k = $k add_mul should work");
    }

    # big
    subtest 'Big test' => sub {
        plan 'skip_all' unless $use_big_test;
        my $k = 127;
        my $k_1 = $k - 1;
        my $q = Polynomial->new(k => $k, 
            str => "s*x^$k_1 + $k_1*t*x^$k_1 + t*(x+$k_1)^$k_1");
        is(5*$q, $q + $q + $q + $q + $q);
    };

    my @funcs = (
        #'2;0', 
        '2;f0', '5;(f + 4*g + 3*h + s + 2*t)', '11;(10*g + h)',              # 0
        '2;f0*x', '3;2*f*x + g', '11;(10*h + s + 5*t)*x',                    # 1
        '3;g0*x^2', '5;4*f*x^2 + q*x + p', '11;6*s*x^2 + 5*f*x + 10*t',      # 2

        '5;x^4 + 4*f1*x^3 + f20*x^2 + 4*x + (3*f300 + g)', # mixed 
        '11;(3*h + 4*t)*x^10 + 5*g0*x^7 + 2*x^3 + 10',     # mixed 
    );

    for my $k (5,11,17) {
        for my $d (0..$k-1) {
            for my $nf (@funcs) {
                my ($n, $f) = split /;/, $nf;
                $f =~ s/x/(x+$d)/g if $d > 0;
                if ($k >= $n) {
#                diag $f;
                    my $s = Polynomial->new(k => $k, d => $d, str => $f);
                    is($s, $f, "cycle sym k = $k d = $d f = $f");
                }
            }
        }
    }
};
