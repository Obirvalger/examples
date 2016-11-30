package AOP;

use Moose;
use Moose::Util::TypeConstraints;
use MyTypes;
use Polynomial;

use feature 'say';
use Data::Printer;
use Data::Dumper;
use Math::Prime::Util qw(binomial znprimroot is_primitive_root powmod);
use Storable 'dclone';
use Carp;

has k => (
    is      => 'rw',
    isa     => 'Prime',
#    default => 5,
    required => 1,
);

subtype 'Funcs',
    as 'ArrayRef[Polynomial]',
    where {@$_ == 2};

has functions => (
    is  => 'ro',
    isa => 'Funcs',
);

subtype 'Gens',
    as 'ArrayRef[Str]',
    where {@$_ == 2};

has gens => (
    is => 'ro',
    isa => 'Gens',
);

has 'polynomials' => (
    is       => 'ro',
    writer   => '_polynomials',
    isa      => 'ArrayRef[ArrayRef[Polynomial]]',
#    isa      => 'ArrayRef[Polynomial]',
    init_arg => undef,
    lazy     => 1,
    builder  => '_build_polynomials',
);

use overload '""' => \&_print;

sub _print {
    my $self = shift;
    my $res = '';
    for my $d (0..$self->k) {
        $res .= join("\n", @{$self->polynomials->[$d]}) . "\n\n";
    }

    return $res;
}

sub to_csv {
    my $self = shift;
    my $res = '';
    for my $d (0..$self->k) {
        $res .= join("\n", map {$_->to_csv} @{$self->polynomials->[$d]}) .
            "\n\n";
    }

    return $res;
}

sub is_any_group_complex {
    my $self = shift;
    my $k = $self->k;
    my $res = 1;

    for my $d (0..$#{$self->polynomials}) {
        my ($un, $is);

        for my $p (@{$self->polynomials->[$d]}) {
            ($un, $is) = union_insect($un, $p->zeros);
            $res = 0 if @$is;
            warn "Not complex d $d\n" if $_[0] and @$is;
        }
    }

    return $res;
}

sub is_all_complex {
    my $self = shift;
    my $k = $self->k;
    my $res = 1;

    for my $d (0..$#{$self->polynomials}) {
        while (my ($i, $p) = each @{$self->polynomials->[$d]}) {
            my $l = $p->len;
            unless ($l == $k) {
                warn "Not complex d $d i $i\n" if $_[0];;
                $res = 0;
            }
        }
    }

    return $res;
}

sub min_len {
    my $self = shift;
    my $k = $self->k;
    my $min_len = $self->polynomials->[0][0]->len;

    for my $d (0..$#{$self->polynomials}) {
        for my $p (@{$self->polynomials->[$d]}) {
            my $l = $p->len;
            $min_len = $l < $min_len ? $l : $min_len;
        }
    }

    return $min_len;
}

sub _build_polynomials {
    my $self = shift;
    my $k = $self->k;
    my $g = Polynomial->new(k => $k, str => $self->gens->[0]);
    my $h = Polynomial->new(k => $k, str => $self->gens->[1]);
    
    my @polys = linear_combs($g, $h);
    my @res;

    for my $d (0..$k) {
        push @res, [map {$_->polarize($d)} @polys];
    }

    return \@res;
}

__PACKAGE__->meta->make_immutable;
no Moose;

sub union_insect {
    my (%union, %isect);
    for my $e (@{$_[0]}, @{$_[1]}) {$union{$e}++ && $isect{$e}++}

    return ([keys %union], [keys %isect]);
}

sub linear_combs {
    my ($g, $h) = @_;
    my $k = $g->k;
    push @_, $g + $h->mul($_) for 1..$k-1;
    @_;
}
