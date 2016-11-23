package VPolynomial;
use Moose;
use feature 'say';
use Data::Printer;
use Data::Dumper;
use Math::Prime::Util qw(binomial znprimroot is_primitive_root powmod);
use Storable 'dclone';
use Carp;

Moose::Exporter->setup_import_methods(
    as_is => [qw(modulo_sum test_group_len is_group_len)],
);

has 'k' => (
    is      => 'rw',
    isa     => 'Int',
    default => 5,
    #required => 1,
);

has 'd' => (
    is      => 'ro',
    isa     => 'Int',
    default => 0,
);

has 'str' => (
    is      => 'bare',
    reader  => '_str',
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
    my $res = polar($k, $self->polynomial, $d - $self->d);

    return VPolynomial->new(polynomial => $res, d => $d, k => $k);
}

sub csv {
    my $self = shift;
    my $d = $self->d;
    my @res;
    return 0 if ($self->k == grep {$_ == 0} @{$self->polynomial});
    while (my ($pow,$c) = each @{$self->polynomial}) {
        my $coeff = show_var(d => $d, c => $c, pow => $pow);
        unshift @res, $coeff; 
    }

    join(';', @res);
}

sub _print {
    my $self = shift;
    my $d = $self->d;
    my @res;
    return 0 if ($self->k == grep {$_ == 0} @{$self->polynomial});
    while (my ($pow,$c) = each @{$self->polynomial}) {
        my $coeff = show_var(d => $d, c => $c, pow => $pow);
        unshift @res, $coeff if $coeff; 
    }

    join(' + ', @res);
}

sub _build_polynomial {
    my $self = shift;
    my $s = $self->_str; 
    my $k = $self->k;
    my $d = $self->d;
    my $polynomial;
    my $vectors;

    my $number = qr/(?:-|\+)?\d+/;
    my $var = qr/(?<x>x)/;
    my $varst = qr{
            \($var(?<d>$number)\)
        |
            \((?<d>$number)\+$var\)
        |
            $var
    }x;
    my $expst = qr/$varst\^(?<pow>$number)/;
    my $summand = qr/$expst|$varst/;
    my $summandst = qr{
            (?: (?<c>$number) \*? )? $summand 
        |
            (?<c>$number)
    }x;

    $s =~ tr/ //d;
    $s =~ s/(\+|-)(?=(?:\(|$var))/${1}1/g;

    croak "Bad string" unless $s =~ /^($summandst)+$/;

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
        $h{pow} = 0 unless defined $h{x};
        delete $h{x};

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
}

__PACKAGE__->meta->make_immutable;
no Moose;

sub show_var {
    my %h = @_;
    my $res = "";
    my ($c, $d, $i) = @h{qw(c d pow)};
    $c //= 1;
    $d //= 0;
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

        $res = $coeff; 
    }

    return $res;
}

sub test_group_len {
    group_len(1, @_);
}

sub is_group_len {
    group_len(0, @_);
}

sub group_len {
    my $v = shift;
    my $k = $_[0]->k;
    croak "k must be equal in summands" unless @_ == grep {$_->k == $k} @_;
#    p @vectors;
    for my $d (0..$k-1) {
        my @vectors = map {$_->polarize($d)->polynomial} @_;
        for my $pow (0..$k-1) {
            my $s = 0;
            while (my ($i, $v) = each @vectors) {
                $s = 1 if $v->[$pow];
            }
            unless ($s) {
                warn "Failed: d = $d pow = $pow\n" if $v;
                return 0 unless $s;
            }
        }
    }
    
    warn "Ok\n" if $v;
    return 1;
}

sub polar {
    my ($k, $vector, $d) = @_;
    $d %= $k;
    my $res;
    for (my $pow = 0; $pow < $k; ++$pow) {
        my $c = 0;
        for (my $i = $pow; $i < $k; ++$i) {
            $c += $vector->[$i] * binomial($i, $pow) * powmod(-$d,$i-$pow,$k);
        }
        $c %= $k;
        $res->[$pow] = $c;
    }
#    p $res;
    
    $res;
}

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

