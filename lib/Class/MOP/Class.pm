package Class::MOP::Class;

use strict;
use warnings;

use Class::MOP::Instance;
use Class::MOP::Method::Wrapped;
use Class::MOP::Method::Accessor;
use Class::MOP::Method::Constructor;

use Carp         'confess';
use Scalar::Util 'blessed', 'reftype', 'weaken';
use Sub::Name    'subname';
use Devel::GlobalDestruction 'in_global_destruction';

our $VERSION   = '0.93';
$VERSION = eval $VERSION;
our $AUTHORITY = 'cpan:STEVAN';

use base 'Class::MOP::Module';

# Creation

sub initialize {
    my $class = shift;

    my $package_name;
    
    if ( @_ % 2 ) {
        $package_name = shift;
    } else {
        my %options = @_;
        $package_name = $options{package};
    }

    ($package_name && !ref($package_name))
        || confess "You must pass a package name and it cannot be blessed";

    return Class::MOP::get_metaclass_by_name($package_name)
        || $class->_construct_class_instance(package => $package_name, @_);
}

# NOTE: (meta-circularity)
# this is a special form of _construct_instance
# (see below), which is used to construct class
# meta-object instances for any Class::MOP::*
# class. All other classes will use the more
# normal &construct_instance.
sub _construct_class_instance {
    my $class        = shift;
    my $options      = @_ == 1 ? $_[0] : {@_};
    my $package_name = $options->{package};
    (defined $package_name && $package_name)
        || confess "You must pass a package name";
    # NOTE:
    # return the metaclass if we have it cached,
    # and it is still defined (it has not been
    # reaped by DESTROY yet, which can happen
    # annoyingly enough during global destruction)

    if (defined(my $meta = Class::MOP::get_metaclass_by_name($package_name))) {
        return $meta;
    }

    # NOTE:
    # we need to deal with the possibility
    # of class immutability here, and then
    # get the name of the class appropriately
    $class = (ref($class)
                    ? ($class->is_immutable
                        ? $class->_get_mutable_metaclass_name()
                        : ref($class))
                    : $class);

    # now create the metaclass
    my $meta;
    if ($class eq 'Class::MOP::Class') {
        $meta = $class->_new($options);
    }
    else {
        # NOTE:
        # it is safe to use meta here because
        # class will always be a subclass of
        # Class::MOP::Class, which defines meta
        $meta = $class->meta->_construct_instance($options)
    }

    # and check the metaclass compatibility
    $meta->_check_metaclass_compatibility();  

    Class::MOP::store_metaclass_by_name($package_name, $meta);

    # NOTE:
    # we need to weaken any anon classes
    # so that they can call DESTROY properly
    Class::MOP::weaken_metaclass($package_name) if $meta->is_anon_class;

    $meta;
}

sub _new {
    my $class = shift;

    return Class::MOP::Class->initialize($class)->new_object(@_)
        if $class ne __PACKAGE__;

    my $options = @_ == 1 ? $_[0] : {@_};

    return bless {
        # inherited from Class::MOP::Package
        'package' => $options->{package},

        # NOTE:
        # since the following attributes will
        # actually be loaded from the symbol
        # table, and actually bypass the instance
        # entirely, we can just leave these things
        # listed here for reference, because they
        # should not actually have a value associated
        # with the slot.
        'namespace' => \undef,
        'methods'   => {},

        # inherited from Class::MOP::Module
        'version'   => \undef,
        'authority' => \undef,

        # defined in Class::MOP::Class
        'superclasses' => \undef,

        'attributes' => {},
        'attribute_metaclass' =>
            ( $options->{'attribute_metaclass'} || 'Class::MOP::Attribute' ),
        'method_metaclass' =>
            ( $options->{'method_metaclass'} || 'Class::MOP::Method' ),
        'wrapped_method_metaclass' => (
            $options->{'wrapped_method_metaclass'}
                || 'Class::MOP::Method::Wrapped'
        ),
        'instance_metaclass' =>
            ( $options->{'instance_metaclass'} || 'Class::MOP::Instance' ),
        'immutable_trait' => (
            $options->{'immutable_trait'}
                || 'Class::MOP::Class::Immutable::Trait'
        ),
        'constructor_name' => ( $options->{constructor_name} || 'new' ),
        'constructor_class' => (
            $options->{constructor_class} || 'Class::MOP::Method::Constructor'
        ),
        'destructor_class' => $options->{destructor_class},
    }, $class;
}

sub reset_package_cache_flag  { (shift)->{'_package_cache_flag'} = undef } 
sub update_package_cache_flag {
    my $self = shift;
    # NOTE:
    # we can manually update the cache number 
    # since we are actually adding the method
    # to our cache as well. This avoids us 
    # having to regenerate the method_map.
    # - SL    
    $self->{'_package_cache_flag'} = Class::MOP::check_package_cache_flag($self->name);    
}

sub _check_metaclass_compatibility {
    my $self = shift;

    # this is always okay ...
    return if ref($self)                eq 'Class::MOP::Class'   &&
              $self->instance_metaclass eq 'Class::MOP::Instance';

    my @class_list = $self->linearized_isa;
    shift @class_list; # shift off $self->name

    foreach my $superclass_name (@class_list) {
        my $super_meta = Class::MOP::get_metaclass_by_name($superclass_name) || next;

        # NOTE:
        # we need to deal with the possibility
        # of class immutability here, and then
        # get the name of the class appropriately
        my $super_meta_type
            = $super_meta->is_immutable
            ? $super_meta->_get_mutable_metaclass_name()
            : ref($super_meta);

        ($self->isa($super_meta_type))
            || confess "The metaclass of " . $self->name . " ("
                       . (ref($self)) . ")" .  " is not compatible with the " .
                       "metaclass of its superclass, ".$superclass_name . " ("
                       . ($super_meta_type) . ")";
        # NOTE:
        # we also need to check that instance metaclasses
        # are compatibile in the same the class.
        ($self->instance_metaclass->isa($super_meta->instance_metaclass))
            || confess "The instance metaclass for " . $self->name . " (" . ($self->instance_metaclass) . ")" .
                       " is not compatible with the " .
                       "instance metaclass of its superclass, " . $superclass_name . " (" . ($super_meta->instance_metaclass) . ")";
    }
}

## ANON classes

{
    # NOTE:
    # this should be sufficient, if you have a
    # use case where it is not, write a test and
    # I will change it.
    my $ANON_CLASS_SERIAL = 0;

    # NOTE:
    # we need a sufficiently annoying prefix
    # this should suffice for now, this is
    # used in a couple of places below, so
    # need to put it up here for now.
    my $ANON_CLASS_PREFIX = 'Class::MOP::Class::__ANON__::SERIAL::';

    sub is_anon_class {
        my $self = shift;
        no warnings 'uninitialized';
        $self->name =~ /^$ANON_CLASS_PREFIX/o;
    }

    sub create_anon_class {
        my ($class, %options) = @_;
        my $package_name = $ANON_CLASS_PREFIX . ++$ANON_CLASS_SERIAL;
        return $class->create($package_name, %options);
    }

    # NOTE:
    # this will only get called for
    # anon-classes, all other calls
    # are assumed to occur during
    # global destruction and so don't
    # really need to be handled explicitly
    sub DESTROY {
        my $self = shift;

        return if in_global_destruction(); # it'll happen soon anyway and this just makes things more complicated

        no warnings 'uninitialized';
        my $name = $self->name;
        return unless $name =~ /^$ANON_CLASS_PREFIX/o;
        # Moose does a weird thing where it replaces the metaclass for
        # class when fixing metaclass incompatibility. In that case,
        # we don't want to clean out the namespace now. We can detect
        # that because Moose will explicitly update the singleton
        # cache in Class::MOP.
        my $current_meta = Class::MOP::get_metaclass_by_name($name);
        return if $current_meta ne $self;

        my ($serial_id) = ($name =~ /^$ANON_CLASS_PREFIX(\d+)/o);
        no strict 'refs';
        @{$name . '::ISA'} = ();
        %{$name . '::'}    = ();
        delete ${$ANON_CLASS_PREFIX}{$serial_id . '::'};

        Class::MOP::remove_metaclass_by_name($name);
    }

}

# creating classes with MOP ...

sub create {
    my ( $class, @args ) = @_;

    unshift @args, 'package' if @args % 2 == 1;

    my (%options) = @args;
    my $package_name = $options{package};

    (ref $options{superclasses} eq 'ARRAY')
        || confess "You must pass an ARRAY ref of superclasses"
            if exists $options{superclasses};
            
    (ref $options{attributes} eq 'ARRAY')
        || confess "You must pass an ARRAY ref of attributes"
            if exists $options{attributes};      
            
    (ref $options{methods} eq 'HASH')
        || confess "You must pass a HASH ref of methods"
            if exists $options{methods};                  

    my (%initialize_options) = @args;
    delete @initialize_options{qw(
        package
        superclasses
        attributes
        methods
        version
        authority
    )};
    my $meta = $class->initialize( $package_name => %initialize_options );

    $meta->_instantiate_module( $options{version}, $options{authority} );

    # FIXME totally lame
    $meta->add_method('meta' => sub {
        $class->initialize(ref($_[0]) || $_[0]);
    });

    $meta->superclasses(@{$options{superclasses}})
        if exists $options{superclasses};
    # NOTE:
    # process attributes first, so that they can
    # install accessors, but locally defined methods
    # can then overwrite them. It is maybe a little odd, but
    # I think this should be the order of things.
    if (exists $options{attributes}) {
        foreach my $attr (@{$options{attributes}}) {
            $meta->add_attribute($attr);
        }
    }
    if (exists $options{methods}) {
        foreach my $method_name (keys %{$options{methods}}) {
            $meta->add_method($method_name, $options{methods}->{$method_name});
        }
    }
    return $meta;
}

## Attribute readers

# NOTE:
# all these attribute readers will be bootstrapped
# away in the Class::MOP bootstrap section

sub get_attribute_map        { $_[0]->{'attributes'}                  }
sub attribute_metaclass      { $_[0]->{'attribute_metaclass'}         }
sub instance_metaclass       { $_[0]->{'instance_metaclass'}          }
sub immutable_trait          { $_[0]->{'immutable_trait'}             }
sub constructor_class        { $_[0]->{'constructor_class'}           }
sub constructor_name         { $_[0]->{'constructor_nam