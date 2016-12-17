#! /usr/bin/perl

use strict;
use warnings;
#use Math::BigRat;
use Memoize;
use feature qw(say);
use Data::Dumper;
use Getopt::Long;

our $k = 5;
our $c = 2;
my $test = 1; # run testing functions?
our $verbose_tests = 0; # notify about passed tests?

# strings, representing operators (regexp form)
my $in_plus_s  = q/ <+> /;
my $out_plus_s = q/ >+< /;
my $c_mul_s    = q/./;
my $x_mul_s    = q/*/;

# regular expressions from strings
my $in_plus_r  = qr/\Q$in_plus_s\E/;
my $out_plus_r = qr/\Q$out_plus_s\E/;
my $c_mul_r    = qr/\Q$c_mul_s\E/;
my $x_mul_r    = qr/\Q$x_mul_s\E/;

Getopt::Long::Configure ("bundling");
GetOptions (
    'k=i' => \$k,
    'c=i' => \$c
);

do {
    local $k = 5;
    my $test = 1;
    local $verbose_tests = 1;
    my $f0 = "g*x^4 + h*x^3 + g*x^2 + h*x + g";
    my $f1 = "h*x^4 + 2*g*x^3 + h*x^2 + 2*g*x + h";
    my $f2 = "(g + h)*x^4 + (2*g + h)*x^3 + (g + h)*x^2 + (2*g + h)*x + (g + h)";
    my $f3="(g + 2*h)*x^4 + (4*g + h)*x^3 + (g + 2*h)*x^2 + (4*g + h)*x + (g + 2*h)";
    my $f4="(g + 3*h)*x^4 + (g + h)*x^3 + (g + 3*h)*x^2 + (g + h)*x + (g + 3*h)";
    my $f5="(g + 4*h)*x^4 + (3*g + h)*x^3 + (g + 4*h)*x^2 + (3*g + h)*x + (g + 4*h)";

    my @functions = ($f0, $f1, $f2, $f3, $f4, $f5);
    run_tests(@functions) if $test;
    test_generate(@functions) if $test;
} if 0;

#my $c = primitive_root($k);
my @funcs = generate("1.g;1.h","1.h;$c.g", $k);
run_tests(@funcs);
say "Ok";
#say primitive_root($k);
#say Dumper(@funcs);
if (0) {
    for my $f (@funcs) {
        for my $d (0..$k-1) {
            say polar($f,$d);
        }
        say '';
    }
}

#say $funcs[0];
#say polar($funcs[0], 1);
#say polar($funcs[2]);
if (0) {
    latex_preambule();
    latex_table(@funcs);
    latex_polarizations(@funcs);
    latex_end();
}
#say conv($funcs7[1]);

sub primitive_root {
    my $k = shift;
    my $res;
    ELEM: 
    for my $i (2..$k-1) {
        for my $e (2..$k-2) {
            next ELEM if $i ** $e  % $k == 1;
        }
        $res = $i;
        last ELEM;
    }

    return $res;
}

sub latex_preambule {
    say
'\documentclass[a4paper, 10pt]{extarticle}
\usepackage[utf8]{inputenc}
\usepackage[russian]{babel}
\usepackage{array}
\usepackage{diagbox}
\newcolumntype{C}{>{$}c<{$}}

\begin{document}';
}

sub latex_end { say '\end{document}' }
sub latex_table {
    my @funcs = @_;
    say '\begin{table}';
    say '\centering';
    say '\begin{tabular}{|c|', 'C|' x $k, '}';
    say '\hline';
    say '\diagbox{$f$}{$d$} & ' . join(' & ', (0..$k-1)) . ' \\\\ \hline';

    while (my ($i,$f) = each @funcs) {
        print "\$f_$i\$ & ";
        for my $d (0..$k-1) {
            print ' & ' if $d;
            print conv(polar($f, $d));
        }
        
        say " \\\\ \\hline";
    }
    say '\end{tabular}';
    say '\end{table}';
}

sub latex_polarizations {
    for my $fun (@_) {
        #say $fun;
        say '$$\begin{array}{l}';
        for my $d (0..$k-1) {
            my $ps = out_format(polarize(in_format($fun), $d));
            $ps =~ s/\*//g;
            #my $ns = split($out_plus_r, $ps);
            #die "$ns; $ps" if $ns != 5;
            #say "d = $d";
            say "@{[out_format($ps)]} \\\\";
        }
        say '\end{array}$$';
        #say "\n";
    }
}

sub invert {
    my $n = shift;
    @{[grep {$_ * $n % $k == 1} (1..$k-1)]}[0];
}
memoize('invert');

sub conv {
    my @s = map {@{[split($x_mul_r,$_)]}[0]}
            split($out_plus_r, in_format(shift));

    for my $f (@s) {
        if ($f =~ /\(.*?\)/) {
            $f =~ s/\(|\)//g;
            my @gh = split($in_plus_r,$f);
            #say Dumper(@gh);
            my ($gc, $gf) = split($c_mul_r, $gh[0]);
            my ($hc, $hf) = split($c_mul_r, $gh[1]);
            #my $tmp = invert $gc;
            #say $tmp;
            #say "$gc $hc @{[invert($gc)]} @{[invert($gc) * $hc]}";
            my $t = invert($gc) * $hc % $k + 1;
            $f = "f_$t";
        } else {
            $f = @{[split($c_mul_r, $f)]}[1];
            if ($f eq 'g') {
                $f = 'f_0';
            } else {
                $f = 'f_1';
            }
        }
    }
    join("", @s);
}

# say ($funcs7[2]);
#latex_polarizations(@funcs7);
#my $f0 = $funcs7[0];
#my $f1 = $funcs7[1];
#my $f3 = $funcs7[3];

#say polar($f0, 2), "\n";
#say polar($f1, 2), "\n";
#say polar(add_mul($f0, $f1, 2),2), "\n";
#say out_format(polarize(in_format($f3), 2));

sub generate {
    my @f = split /;/, shift;
    my @g = split /;/, shift;
    my $k = shift;
    my $fn = '';
    my $gn = '';
    my @res;
    
    $fn =  out_format(in_format(join($out_plus_s,
        map {"@f[$_%2]*x^@{[$k-1-$_]}"} (0..$k-1))));
    $gn =  out_format(in_format(join($out_plus_s,
        map {"@g[$_%2]*x^@{[$k-1-$_]}"} (0..$k-1))));
    
    push @res, $fn, $gn;   
    for my $i (1..$k-1) {
        push @res, plus($fn, const_multiply($i, $gn));
    }
    
    return @res;
}

sub is_complex {
    my $res = 1;

    for my $fun (@_) {
        for my $d (1..$k-1) {
            my $ps = polarize(in_format($fun), $d);
            my $ns = split($out_plus_r, $ps);
            if ($ns != $k) {
                say "Only $ns summands in\n@{[out_format($ps)]}";
                $res = 0;
                last;
            }
        }
    }

    $res;
}

sub add_mul {
    my ($f, $g, $c) = @_;
    $c //= 1;
    plus($f, const_multiply($c, $g));
}

sub plus {
    my @a = @_;
    out_format(collect(join($out_plus_s,
        map {expand(in_format($_))} @a)));
}

sub const_multiply {
    my ($c, $s) = @_;
    $s = expand(in_format($s));
    $s =~ s/(\d+)\./($1*$c)%$k . '.'/ge;
    out_format(collect($s));
}

sub in_format {
    $_ = shift;
    
    # experemental
    # s/(\s*)(([a-wyz0-9\*]+\s*\+\s*)+)(\s*[a-wyz0-9\*]*$)/$1($2$4)/

    # plus
    s/\(([\w\d\.\*]+?)\s*\+\s*([\w\d\.\*]+?)\)/($1Plus$2)/g;
    s/(\s\+\s)/$out_plus_s/g;
    s/([\w\d\(\)])\+([\w\d\(\)])/$1$out_plus_s$2/g;
    s/Plus/$in_plus_s/g;

    # multiply
    s/(\d+)\*([a-z]+)/$1$c_mul_s$2/g;
    s/(\d+)([a-z]+)/$1$c_mul_s$2/g;
    s/([^\*\.])([a-z]+)/${1}1$c_mul_s$2/g;
    s/^([a-z]+)/1$c_mul_s$1/;

    # coefficients mod k
    s/((?:-|\d)+)\./$1%$k . '.'/ge;

    # power
    s/([a-z]+)$/$1*x^0/;
    s/\)$/)*x^0/g;
    s/x([^\^])/x^1$1/g;
    s/x$/x^1/;

    $_;
}

sub out_format {
    $_ = shift;
    s/$in_plus_r/ + /g;
    s/$out_plus_r/ + /g;
    s/1$c_mul_r//g;
    s/$c_mul_r/*/g;
    s/\(x(\s*)\+(\s*)(\d+)\)\^1\b/(x$1+$2$3)/g;
    s/\*\(x\s*\+\s*\d+\)\^0//g;
    s/x\^1\b/x/g;
    s/\*x\^0//g;

    $_;
}

sub polar {
    my ($s, $d) = @_;
    out_format(polarize(in_format($s),$d));
}

sub polarize {
    sub powM {
        my ($x, $d, $e) = @_;
        my @summands;

        for (my $i = 0; $i <= $e+0; $i++) {
            my $c = (binomial($e, $i) * $d ** $i) % $k; # const
            my $p = $e - $i; # power
            my $xs = "x^$p";
            my $cs = "$c$c_mul_s";
            my $ss = $cs . $xs;
            $ss = "" if $c == 0;
            push @summands, $ss if $ss;
        }

        my $res = join($in_plus_s, @summands);
        return "($res)"
    }

    my ($s, $d) = @_;
    $s =~ s/(x)\^(.)/powM($1, -$d % $k, $2)/ge;
    my $res = collect(expand($s));
    $res =~ s/x/(x+$d)/g if $d != 0;
    $res;
}

sub collect {
    sub sort_powers {
        my ($a, $b) = @_;
        my ($tmp1, $ad) = split(/\^/, $a);
        my ($tmp2, $bd) = split(/\^/, $b);
        $ad <=> $bd;
    }

    my $dp = 0; # debugging print
    my $s = shift;
    my %powers;
    my @summands;

    for my $summand (split($out_plus_r, $s)) {
        my ($cf, $x_pow) = split($x_mul_r, $summand);
        my ($cs, $fs) = split($c_mul_r, $cf);
        ${$powers{$x_pow}}{$fs} += $cs;
        ${$powers{$x_pow}}{$fs} %= $k;
    }

    print Dumper(%powers) if $dp;
    
    for my $x_pow (sort {sort_powers($b, $a)} keys %powers) {
        my @functions = sort {$a cmp $b} keys %{$powers{$x_pow}};
        @functions = grep {${$powers{$x_pow}}{$_} != 0} @functions;
        if (@functions == 0) {
            # do nothing
        } elsif (@functions == 1) {
            push @summands,
                "${$powers{$x_pow}}{$functions[0]}.$functions[0]*$x_pow"
        } else {
            push @summands, 
                qq[(@{[join($in_plus_s, (map {"${$powers{$x_pow}}{$_}.$_"}
                    @functions))]})$x_mul_s$x_pow];
        }
    }
    join $out_plus_s, @summands;
}

sub expand { 
    sub expand_one {
        my $dp = 0; # debugging print
        my $s = shift;
        say $s if $dp; 
        $s =~ s/\(|\)//g;
        my @summands;
        my @multipliers = split($x_mul_r, $s);
        # multiplication of summs
        my @mos = map {[split($in_plus_r, $_)]} @multipliers;
        print Dumper(@mos) if $dp;

        for my $fun (@{$mos[0]}) {
            my ($cf, $fs) = split($c_mul_r, $fun);
            for my $var (@{$mos[1]}) {
                my ($cx, $xs);
                if ($var =~ /^\d+\./) {
                    ($cx, $xs) = split($c_mul_r, $var);
                } else {
                    $cx = 1;
                    $xs = $var;
                }
                my $cs = $cf * $cx % $k;
                push @summands, "$cs$c_mul_s$fs$x_mul_s$xs";
            }
        }
        
        my $res = join $out_plus_s, @summands;
        say "$res\n" if $dp;
        return $res;
    }

    return (join $out_plus_s, ((map {expand_one($_)}
        (split($out_plus_r, shift)))));
}

sub outer_summands {
    my $s = shift;
    #$s =~ s/\((.*?)\+(.*?)\)/($1plus$2)/g;
    my @summands = split($out_plus_r, $s);
    @summands;
}

sub binomial { 
    my( $n, $r ) = @_;
    return unless defined $n && 
        $n =~ /^\d+$/ && 
        defined $r && 
        $r =~ /^\d+$/;
    
    my $product = 1;
    while( $r > 0 ) {
        $product *= $n--;
        $product /= $r--;
    }
    return $product;
}

sub run_tests {
    test_zero_polarization(@_) if $test;
    test_in_out_identity(@_) if $test;
    test_arithmetic_operations(@_) if $test;
    test_complex(@_) if $test;
}

sub test_complex {
    for my $fun (@_) {
        for my $d (1..$k-1) {
            my $ps = polarize(in_format($fun), $d);
            my $ns = split($out_plus_r, $ps);
            my $fail = "Fail test_complex\n";
            die $fail . "Only $ns summands in\n@{[out_format($ps)]}"
                if $ns != $k;
            }
        }
    
    say "Ok test_complex" if $verbose_tests;
}

sub test_in_out_identity {
    for my $fs (@_) {
        my $in1 = in_format($fs);
        my $in2 = in_format(in_format($fs));
        my $out1 = out_format(in_format($fs));
        my $out2 = out_format(out_format(in_format($fs)));
        my $fail = "Fail test_in_out_identity\n";

        die $fail . "Formulas:\n$fs\n$in1\n$in2\nin_format is not identity operation" 
            if $in1 ne $in2;
        die $fail .
            "Formulas:\n$fs\n$out1\n$out2\nout_format is not identity operation"
                if $out1 ne $out2;
    }

    say "Ok test_in_out_identity" if $verbose_tests;
}

sub test_arithmetic_operations {
    for my $fs (@_) {
        my $times0 = const_multiply(0, $fs);
        my $times1 = const_multiply(1, $fs);
        my $times2 = const_multiply(2, $fs);
        my $sum = plus($fs, $fs);
        my $sum0 = plus($fs, $times0);
        my $fail = "Fail test_arithmetic_operations\n";

        die $fail . "Multiplication by 2 does not equal to addition to itself"
            if $sum ne $times2;
        die $fail . "Multiplication by 1 does not identity" if $times1 ne $fs;
        die $fail . "Multiplication by 0 does not return empty string"
            if $times0 ne '';
        die $fail . 
            "Addition an empty string (multiplied by zero formula) does not identity"
                if $sum0 ne $fs;
    }

    say "Ok test_arithmetic_operations" if $verbose_tests;
}

sub test_zero_polarization {
    for my $fs (@_) {
        my $out = out_format(polarize(in_format($fs), 0));
        my $fail = "Fail test_zero_polarization\n";

        die $fail . "Zero polarization:\n$out\ndoes not equal to formula:\n$fs"
            if $fs ne $out;
    }

    say "Ok test_zero_polarization" if $verbose_tests;
}

sub test_generate {
    my @funcs5 = generate('1.g;1.h','1.h;2.g', 5);
    for my $i (0..$#_) {
        my $fail = "Fail test_generate\n";

        die $fail . "The generate function works wrong!"
            if $_[$i] ne $funcs5[$i];
    }

    say "Ok test_generate" if $verbose_tests;
}
