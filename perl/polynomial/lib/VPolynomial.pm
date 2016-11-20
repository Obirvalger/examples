package VPolynomial;
use Moose;
use feature 'say';
use Data::Printer;
use Data::Dumper;
use Math::Prime::Util qw(binomial znprimroot is_primitive_root powmod);
use Storable 'dclone';
use Carp;

#Moose::Exporter->setup_import_methods(
#    as_is => [qw(add_mul generate)],
#);

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

=begin  BlockComment  # BlockCommentNo_4

#    say '+=';
    my ($self, $other) = @_;
    croak "k must be equal in summands" unless $self->k == $other->k;
    my $spolynomial = $self->polynomial;
    my $opolynomial = $other->polynomial;
    while (my ($i, $coeff)  = each @$opolynomial) {
        for my $f (keys %$coeff) {
            $spolynomial->[$i]{$f} += $coeff->{$f};
            $spolynomial->[$i]{$f} %= $self->k;
        }
    }
    return $self;

=end    BlockComment  # BlockCommentNo_4

=cut

}

use overload '+' => '_overload_add';#, fallback => 1;
sub _overload_add {

=begin  BlockComment  # BlockCommentNo_3

#    say '+';
    my ($self, $other) = @_;
    my $tmp = dclone $self;
    $tmp += $other;
    return $tmp;

=end    BlockComment  # BlockCommentNo_3

=cut

}

sub len {

=begin  BlockComment  # BlockCommentNo_2

    my $self = shift;
    my $sum = 0;

    for my $h (@{$self->polynomial}) {
        $sum += (grep {$h->{$_} > 0} keys %$h) > 0;
    }

    return $sum;

=end    BlockComment  # BlockCommentNo_2

=cut

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

=begin  BlockComment  # BlockCommentNo_5

    my ($self, $d) = @_;
    my $k = $self->k;
    $d %= $k;
    my $a = $self->polynomial;
    my $res;
    for (my $pow = 0; $pow < $k; ++$pow) {
        my $h = $a->[$pow];
        for my $f (@{$self->funcs}) {
            my $c = 0;
            for (my $i = $pow; $i < $k; ++$i) {
                $c += $a->[$i]->{$f} * binomial($i, $pow) *
                    powmod(-$d,$i-$pow,$k);
            }
            $c %= $k;
            $res->[$pow]->{$f} = $c;
        }
    }

    return SPolynomial->new(polynomial => $res, d => $d, k => $k);

=end    BlockComment  # BlockCommentNo_5

=cut

}

sub _print {
    my $self = shift;
    my $d = $self->d;
    my @res;
    while (my ($i,$c) = each @{$self->polynomial}) {
        if ($c > 0) {
            my $coeff = $c == 1 ? '' : $i > 0 ? "$c*" : $c;

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
    my $s = $self->str;
    my $polynomial;
    
    my $number = qr/(?:-|\+)?\s*\d+/;
    my $var = qr/x/;
    my $varst = qr/\($var\s*(?<d>$number)\)|\((?<d>$number)\s*\+\s*$var\)|$var/;
    my $expst = qr/$varst\^(?<pow>$number)/;
    my $summand = qr/$expst|$varst|(?<l>\d+)/;
    my $summandst = qr/(?:(?<c>$number)\*?)?$summand/;

    while ($s =~ /$summandst/g) {
        my %h = %+;
        $h{d} //= 0;
        $h{d} =~ s/\s//g;
        $h{d} += 0;
        $h{c} //= 1; 
        $h{c} =~ s/\s//g;
        $h{c} += 0;
        if ($h{l}) {
            $h{pow} = 0;
        } else {
            $h{pow} //= 1;
        }
        p %h;
    }

    return $polynomial;
}

__PACKAGE__->meta->make_immutable;
no Moose;

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

