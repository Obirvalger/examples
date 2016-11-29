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

# TODO polarizations
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

sub linear_combs {
    my ($g, $h) = @_;
    my $k = $g->k;
    push @_, $g + $h->mul($_) for 1..$k-1;
    @_;
}
