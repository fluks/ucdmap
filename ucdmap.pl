#!/usr/bin/env perl
use warnings;
use strict;
use Tk;
use Tk::Table;
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
        -fixedrows  => 1,
        -scrollbars => 'es',
        -takefocus  => 1
    );
    fill_table($opt, $table);
    $table->pack;

    bind_keys($main);
}

# Create menu.
# Parameters: main window
# Returns:    menu widget
sub create_menu {
    my $main = shift;

    my $menu = $main->Menu(-type => 'normal');
    $menu->cascade(qw/-label ~File -tearoff 0 -menuitems/ => [
            [ Button => '~Quit', -command => [ \&quit, $main ], -accelerator => 'Ctrl+Q' ]
    ]);

    return $menu;
}

sub fill_table {
    my ($opt, $table) = @_;

    my ($row, $col) = (0, 0);
    for my $group (@{ $opt->{ucd_map} }) {
        my $block = $group->{block};
        my $label = $table->Label(-text => $block);
        $table->put($row++, $col, $label);
    }
}

# Quit program.
# Parameters: main window
sub quit {
    my $main = shift;

    $main->destroy;
}

# Bind keys.
# Parameters: - main window
sub bind_keys {
    my ($main) = @_;

    $main->bind('<Control-q>' => [ \&quit ]);
}
