package VPolynomial;
use Moose;
use feature 'say';
use Data::Printer;
use Data::Dumper;
use Math::Prime::Util qw(binomial znprimroot is_primitive_root powmod);
use Storable 'dclone';
use Carp;

Moose::Exporter->setup_import_methods(
    as_is => [qw(modulo_sum)],
);

has 'k' => (
    is      => 'rw',
    isa     => 'Int',
    default => 5,
    #required => 1,
);

has 'd' => (
    is      => 'rw',
    isa     => 'Int',
    default => 0,
);

has 'str' => (
    is      => 'ro',
    isa     => 'Str',
#    required => 1,
);

has 'polynomial' => (
    is       => 'rw',
#    init_arg => undef,
    isa      => 'ArrayRef[Int]',
    lazy     => 1,
    builder  => '_build_polynomial',
); 


use overload '""' => \&_print;

use overload '+=' => \&_overload_add_eq, fallback => 1;

sub _overload_add_eq {
#    say '+=';
    my ($self, $other) = @_;
    croak "k must be equal in summands" unless $self->k == $other->k;
    my $spolynomial = $self->polynomial;
    my $opolynomial = $other->polynomial;
    while (my ($i, $c)  = each @$opolynomial) {
        ($spolynomial->[$i] += $c) %= $self->k;
    }
    return $self;
}

use overload '+' => '_overload_add';#, fallback => 1;
sub _overload_add {
#    say '+';
    my ($self, $other) = @_;
    my $tmp = dclone $self;
    $tmp += $other;
    return $tmp;
}

sub len {
    my $self = shift;
    return 0 + grep {$_ > 0} @{$self->polynomial};
}

sub clone {
    my $self = shift;
    my $res = dclone $self;
    return $res;
}

sub mul {

=begin  BlockComment  # BlockCommentNo_1

    my ($self, $c) = @_;
    my $tmp = dclone $self;
    for my $coeff (@{$tmp->polynomial}) {
        for my $f (keys %$coeff) {
#            p $coeff;
#            say "$f $$coeff{$f}";
            $$coeff{$f} *= $c;
            $$coeff{$f} %= $tmp->k;
        }
    }
    
    return $tmp;

=end    BlockComment  # BlockCommentNo_1

=cut

}

sub polarize {
    my ($self, $d) = @_;
    my $k = $self->k;
    $d %= $k;
    my $a = $self->polynomial;
    my $res;
    for (my $pow = 0; $pow < $k; ++$pow) {
        my $c = 0;
        for (my $i = $pow; $i < $k; ++$i) {
            $c += $a->[$i] * binomial($i, $pow) * powmod(-$d,$i-$pow,$k);
        }
        $c %= $k;
        $res->[$pow] = $c;
    }

    return SPolynomial->new(polynomial => $res, d => $d, k => $k);
}

sub _print {
    my $self = shift;
    my $d = $self->d;
    my @res;
    return 0 if ($self->k == grep {$_ == 0} @{$self->polynomial});
    while (my ($i,$c) = each @{$self->polynomial}) {
        if ($c > 0) {
            my $coeff = $c == 1 ? '' : $i > 0 ? "$c*" : $c;
            $coeff = 1 if $c == 1 && $i == 0;

            if ($i > 1) {
                if ($d > 0) {
                    $coeff .= "(x+$d)^$i";
                } else {
                    $coeff .= "x^$i";
                }
            } elsif ($i == 1) {
                if ($d > 0) {
                    $coeff .= "(x+$d)";
                } else {
                    $coeff .= "x";
                }
            }

            unshift @res, $coeff; 
        }
    }

    join(' + ', @res);
}

sub _build_polynomial {
    my $self = shift;
    my $s = $self->str =~ tr/ //dr;
    my $k = $self->k;
    my $d = $self->d;
    my $polynomial;
    my $vectors;

    my $number = qr/(?:-|\+)?\s*\d+/;
    my $var = qr/x/;
    my $varst = qr/\($var\s*(?<d>$number)\)|\((?<d>$number)\s*\+\s*$var\)|$var/;
    my $expst = qr/$varst\^(?<pow>$number)/;
    my $summand = qr/$expst|$varst/;
    my $summandst = qr{
        (?:(?<c>$number)\*?)? $summand |
        \+ $summand                    |
        (?<l> (?<c>$number) )
    }x;

    while ($s =~ /$summandst/g) {
        my %h = %+;
#        p %h;
        $h{d} //= 0;
        $h{d} =~ s/\s//g;
        $h{d} += 0;
        $h{c} //= 1; 
        $h{c} =~ s/\s//g;
        $h{c} += 0;
        $h{pow} //= 1;
        if (defined $h{l}) {
            $h{pow} = 0;
            delete $h{l};
        }

#        p %h;

        $vectors->[$h{d}][$h{pow}] = $h{c};
    }
    for my $d (0..$k-1) {
        for my $i (0..$k-1) {
            $vectors->[$d][$i] //= 0;
        }
    }

    while (my ($i, $x) = each @$vectors) {
        $vectors->[$i] = polar($k, $x, $d-$i);
    }

    $polynomial = modulo_sum($k, @$vectors);
#    p $polynomial;
#    $vectors->[0];
#    return $polynomial;
}

__PACKAGE__->meta->make_immutable;
no Moose;

sub modulo_sum {
    my $k = shift;
    my $res;
    for my $v (@_) {
        while (my ($i, $c) = each  @$v) {
            ($res->[$i] += $c) %= $k;
        }
    }

    $res;
}

sub make_poly {
    my ($k, $fun, $d) = @_;
    my @f = @$fun;
    my @poly;
    $poly[0] = $f[-$d];
    for my $j (1..$k-2) {
        my $val = 0;
        for my $i (1..$k-1) {
            $val += $i**($k-1-$j) * $f[$i-$d];
        }
        $poly[$j] = (-1 * $val) % $k;
    }
    $poly[$k-1] -= $f[$_-$d] for (0..$k-1);
    $poly[$k-1] %= $k;
    \@poly;
}

sub polar {
    my ($k, $a, $d) = @_;
    $d %= $k;
    my $res;
    for (my $pow = 0; $pow < $k; ++$pow) {
        my $c = 0;
        for (my $i = $pow; $i < $k; ++$i) {
            $c += $a->[$i] * binomial($i, $pow) * powmod(-$d,$i-$pow,$k);
        }
        $c %= $k;
        $res->[$pow] = $c;
    }
#    p $res;
    
    $res;
}

__END__

sub add_mul {
    my ($p1, $p2, $c) = @_;
    $c //= 1;

    return $p1 + $p2->mul($c);
}

sub generate {
    my ($g, $h, $k) = @_;
    my @res;
    my $f0 = SPolynomial->new(k => $k, gen => $g);
    my $f1 = SPolynomial->new(k => $k, gen => $h);
    
    push @res, $f0, $f1;

    for my $c (1..$k-1) {
        push @res, add_mul($f0,$f1,$c);
    }

    @res;
}

