#!/usr/bin/env perl
use strict;
use warnings;
use Storable qw/nstore/;
use File::Basename;
use XML::Simple;

# Convert and filter unicode XML file to network order Perl Storable file.
# Filters unnecessary nodes and attributes.
# First argument is the XML file and second argument is the output file.

if (@ARGV < 2) {
    print_usage();
    exit 1;
}
my $ref = XMLin($ARGV[0], ForceArray => 1);
my $new_ref = filter_excess($ref);
nstore $new_ref, $ARGV[1];

exit 0;

# Filter all but group's blk (block, a description of group) and characters,
# and for a character, keep only it's name and character point (cp).
# Resulting data structure is:
# [
#   { block => ...,
#     chars => [
#         { name => ..., cp => ... },
#         ...
#     ]
#   },
#   ...
# ]
# Filters blocks which don't have at least one char node (they probably have
# something like 'first-char' and 'last-char' nodes. XXX Maybe add them later.
# Parameters: resulting reference from parsing an XML file
# Returns:    filtered reference
sub filter_excess {
    my $ref = shift;

    my $groups = $ref->{repertoire}->[0]->{group};
    my $new_ref = [];
    my $gi = 0;
    for my $group (@$groups) {
        next unless exists $group->{char};

        $group->{blk} =~ tr/_/ /
            if exists $group->{blk} && defined $group->{blk};
        $new_ref->[$gi]->{block} = $group->{blk};
        my $chars = $group->{char};
        my $ci = 0;
        for my $char (@$chars) {
            $new_ref->[$gi]->{chars}->[$ci]->{cp} = $char->{cp};
            $new_ref->[$gi]->{chars}->[$ci]->{name} = $char->{na1} || $char->{na} || '?';
            ++$ci;
        }

        $gi++;
    }

    return $new_ref;
}

sub print_usage {
    my $program_name = basename($0);

    print <<HELP;
Convert and filter unicode xml file to network storable
usage:
    $program_name <input file> <output file>
HELP
}
