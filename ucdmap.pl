#!/usr/bin/env perl
use warnings;
use strict;
use Tk;
use Tk::Table;
use Tk::Balloon;
use Storable qw/retrieve/;
use File::Basename;

my $options = {
    ucd_map          => undef,
    ucd_default_file => 'ucd.nstor',
    ucd_file         => 'ucd.nstor',
    fullscreen       => 1
};

$options->{ucd_file} = $ARGV[0] if @ARGV;
$options->{ucd_map} = retrieve($options->{ucd_file});
create_gui($options);

MainLoop;

# Create gui.
# Parameters: options
sub create_gui {
    my ($opt) = @_;

    my $main = MainWindow->new(-title => basename($0));
    $main->FullScreen;
    $main->packPropagate(0);

    my $menu = create_menu($main);
    $main->configure(-menu => $menu);

    my $table = $main->Table(
        -rows       => 50,
        -scrollbars => 'es'
    );
    fill_table($opt, $table);
    $table->pack(-fill => 'both');
    $table->focus;

    bind_keys($main);
}

# Create menu.
# Parameters: main window (Tk::MainWindow)
# Returns:    menu widget (Tk::Menu)
sub create_menu {
    my $main = shift;

    my $menu = $main->Menu(-type => 'normal');
    $menu->cascade(qw/-label ~File -tearoff 0 -menuitems/ => [
        [
            Button       => '~Quit',
            -command     => [ \&quit, $main ],
            -accelerator => 'Ctrl+Q'
        ]
    ]);

    return $menu;
}

# Fill table with ucd block names.
# TODO add a hidden table or something for cps
# Parameters: - options
#             - table (Tk::Table)
sub fill_table {
    my ($opt, $table) = @_;

    my $balloon = $table->Balloon;
    my ($row, $col) = (0, 0);
    for my $group (@{ $opt->{ucd_map} }) {
        my $block = $group->{block};
        my $label = $table->Label(-text => $block, -anchor => 'nw', -padx => 5);
        $balloon->attach($label, -balloonmsg => cp_range($group));
        $table->put($row++, $col, $label);
    }
}

# Find and return character point range for a character block.
# Parameters: character block (ArrayRef)
# Returns:    character point range (Str)
sub cp_range {
    my $group = shift;

    my $chars = $group->{chars};
    my $first = $chars->[0]->{cp} // '?';
    my $last  = $chars->[-1]->{cp} // '?';

    return 'CPs: ' . $first . '-' . $last;
}

# Quit program.
# Parameters: main window (Tk::MainWindow)
sub quit {
    my $main = shift;

    $main->destroy;
}

# Search cps, block names, names for character, ...
sub find {
    my ($main) = @_;

    my $window = $main->Toplevel(-title => 'Find');
    $window->geometry('300x200');
    $window->bind('<Escape>' => sub { $window->destroy });
    my $frame1 = $window->Frame->pack;
    my $entry = $frame1->Entry()->pack;
    $entry->focus;
    my $button = $frame1->Button(-text => 'Find', -command => sub {} )->pack;
}

# Bind keys.
# Parameters: - main window (Tk::MainWindow)
sub bind_keys {
    my ($main) = @_;

    $main->bind('<Control-q>' => [ \&quit ]);
    $main->bind('<Control-f>' => [ \&find ]);
}
