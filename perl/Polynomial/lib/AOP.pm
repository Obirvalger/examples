package AOP;

use Moose;
use Moose::Util::TypeConstraints;
use MyTypes;
use SPolynomial;
use VPolynomial;

use feature 'say';
use Data::Printer;
use Data::Dumper;
use Math::Prime::Util qw(binomial znprimroot is_primitive_root powmod);
use Storable 'dclone';
use Carp;

has 't' => (
    is  => 'ro',
    isa => 'PolynomialClass',
); 

has k => (
    is      => 'rw',
    isa     => 'Prime',
#    default => 5,
    required => 1,
);

subtype 'Funcs',
    as 'ArrayRef[VPolynomial | SPolynomial]',
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
    isa      => 'ArrayRef[VPolynomial | SPolynomial]',
    init_arg => undef,
    lazy     => 1,
    builder  => '_build_polynomials',
);

use overload '""' => \&_print;

sub _print {
    my $self = shift;
    join "\n", @{$self->polynomials};
}

# TODO polarizations
sub _build_polynomials {
    my $self = shift;
    my $k = $self->k;
    my $g = VPolynomial->new(k => $k, str => $self->gens->[0]);
    my $h = VPolynomial->new(k => $k, str => $self->gens->[1]);
    
    [linear_combs($g, $h)];
}

__PACKAGE__->meta->make_immutable;
no Moose;

sub linear_combs {
    my ($g, $h) = @_;
    my $k = $g->k;
    push @_, $g + $h->mul($_) for 1..$k-1;
    @_;
}
