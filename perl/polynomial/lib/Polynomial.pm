package Polynomial;
use Moose;
use feature 'say';
use Data::Printer;
use Data::Dumper;
use Math::Prime::Util qw(binomial znprimroot is_primitive_root powmod);
use Storable 'dclone';
use Carp;

Moose::Exporter->setup_import_methods(
    as_is => [qw(add_mul generate)],
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

has 'poly' => (
    is      => 'ro',
    isa     => 'Str',
#    required => 1,
);

has 'gen' => (
    is => 'rw',
    isa => 'Str',
);

has 'funcs' => (
    is  => 'ro',
    isa => 'ArrayRef[Str]',
    default => sub {['g', 'h']},
);

has 'vector' => (
    is       => 'rw',
#    init_arg => undef,
    isa      => 'ArrayRef[HashRef[Int]]',
    lazy     => 1,
    builder  => '_build_vector',
); 


use overload '""' => \&_print;

use overload '+=' => \&_overload_add_eq, fallback => 1;

sub _overload_add_eq {
#    say '+=';
    my ($self, $other) = @_;
    croak "k must be equal in summands" unless $self->k == $other->k;
    my $svector = $self->vector;
    my $ovector = $other->vector;
    while (my ($i, $coeff)  = each @$ovector) {
        for my $f (keys %$coeff) {
            $svector->[$i]{$f} += $coeff->{$f};
            $svector->[$i]{$f} %= $self->k;
        }
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
    my $sum = 0;

    for my $h (@{$self->vector}) {
        $sum += (grep {$h->{$_} > 0} keys %$h) > 0;
    }

    return $sum;
}

sub clone {
    my $self = shift;
    my $res = dclone $self;
    return $res;
}

sub mul {
    my ($self, $c) = @_;
    my $tmp = dclone $self;
    for my $coeff (@{$tmp->vector}) {
        for my $f (keys %$coeff) {
#            p $coeff;
#            say "$f $$coeff{$f}";
            $$coeff{$f} *= $c;
            $$coeff{$f} %= $tmp->k;
        }
    }
    
    return $tmp;
}

sub polarize {
    my ($self, $d) = @_;
    my $k = $self->k;
    $d %= $k;
    my $a = $self->vector;
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

    return Polynomial->new(vector => $res, d => $d, k => $k);
}

sub _print {
    my $self = shift;
    my $d = $self->d;
    my @res;
    while (my ($i,$s) = each @{$self->vector}) {
        my @keys = sort grep {$s->{$_} > 0} keys %{$s};

        if (@keys) {
            my @coeff;
            for my $f (@keys) {
                if ($s->{$f} == 1) {
                    push @coeff, $f;
                } else {
                    push @coeff, "$s->{$f}*$f";
                }
            }
            
            my $coeff = join(' + ', @coeff);
            $coeff = "($coeff)" if @coeff > 1;

            if ($i > 1) {
                if ($d > 0) {
                    $coeff .= "*(x+$d)^$i";
                } else {
                    $coeff .= "*x^$i";
                }
            } elsif ($i == 1) {
                if ($d > 0) {
                    $coeff .= "*(x+$d)";
                } else {
                    $coeff .= "*x";
                }
            }

            unshift @res, $coeff; 
        }
    }

    join(' + ', @res);
    #say join(' + ', @res);
#    return $self;
}

sub _build_vector {
    my $self = shift;
    my $vector;

    if ($self->gen) {
        my @gen = split /;/, $self->gen;
        my $n = @gen;
        for my $x (@gen) {
            my ($c, $f) = split(/\./, $x);
            unshift @$vector, {$f => $c % $self->k};
        }

        for (my $i = $n; $i < $self->k; ++$i) {
#            p $vector;
            unshift @$vector, {%{$vector->[$n-1]}};
        }

    } else {
        my $s = $self->poly;
        
        # powers
        $s =~ s/([a-z]+)$/$1*x^0/;
        $s =~ s/\)$/)*x^0/g;
        $s =~ s/x([^\^])/x^1$1/g;
        $s =~ s/x$/x^1/;

        # multiply
        $s =~ s/(\d+)\*([a-z]+)/$1.$2/g;
        $s =~ s/(\d+)([a-z]+)/$1.$2/g;
        $s =~ s/([^\*\.])([a-z]+)/${1}1.$2/g;
        $s =~ s/^([a-z]+)/1.$1/;
        
        # change inner + to Plus
        $s =~ s/\(([\w\d\.\*]+?)\s*\+\s*([\w\d\.\*]+?)\)/($1Plus$2)/g;
        
        # remove some elements to helping parsing   
        $s =~ s/\(|\)|\*x//g; 

        my @summands = map {s/Plus/+/r;} split(/\s*\+\s*/, $s);
        for my $s (@summands) {
            my ($coeff, $pow) = split(/\^/, $s);
            for my $cf (split(/\+/, $coeff)) {
                my ($c, $f) = split(/\./, $cf);
                $vector->[$pow]{$f} = $c;
            }
        }
    }

    for my $coeff (@$vector) {
        for my $f (@{$self->funcs}) {
            $coeff->{$f} //= 0;
        }
    }

    return $vector;
}

__PACKAGE__->meta->make_immutable;
no Moose;

sub add_mul {
    my ($p1, $p2, $c) = @_;
    $c //= 1;

    return $p1 + $p2->mul($c);
}

sub generate {
    my ($g, $h, $k) = @_;
    my @res;
    my $f0 = Polynomial->new(k => $k, gen => $g);
    my $f1 = Polynomial->new(k => $k, gen => $h);
    
    push @res, $f0, $f1;

    for my $c (1..$k-1) {
        push @res, add_mul($f0,$f1,$c);
    }

    @res;
}

