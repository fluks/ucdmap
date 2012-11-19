#!/usr/bin/env perl
use warnings;
use strict;
use Tk;
use Tk::Table;
use Tk::Balloon;
use Storable qw/retrieve/;
use File::Basename;
use feature qw/state/;

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

    my $table = $main->Table(-scrollbars => 'es');
    fill_table($opt, $table);
    $table->pack(-fill => 'both', -expand => 1);
    $table->focus;

    bind_keys($main, $table);
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
            -command     => sub { quit($main) },
            -accelerator => 'Ctrl+Q'
        ]
    ]);
    $menu->cascade(qw/-label ~View -tearoff 0 -menuitems/ => [
        [
            Button       => '~Expand All',
            -command     => sub {  },
            -accelerator => 'Ctrl+E'
        ]
    ]);

    return $menu;
}

# Fill table with ucd block names and balloons of cp ranges.
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

# Pop up a window for finding blocks, CPs and character names.
# Parameters: - Tk::MainWindow
#             - Tk::Table
sub pop_find_window {
    my ($main, $table) = @_;

    my $window = $main->Toplevel(-title => 'Find');
    $window->geometry('300x100');
    $window->bind('<Escape>' => sub { $window->destroy });

    my $frame1 = $window->Frame->pack(qw/-fill x -padx 5 -pady 5/);
    my $entry = $frame1->Entry->pack(qw/-side left -fill x -expand 1 -padx 5/);
    $entry->focus;
    state $cps_on = 0; state $block_on = 0; state $char_on = 0;
    my $button = $frame1->Button(-text      => 'Find',
                                 -underline => 0,
                                 -command   => sub {
        focus_find($table, \$cps_on, \$block_on, \$char_on, $entry->get)
    })->pack;

    my $frame2 = $window->Frame->pack;
    my $cps    = $frame2->Checkbutton(-text      => 'CP',
                                      -underline => 0,
                                      -variable  => \$cps_on)->pack(qw/-side left -anchor nw/);
    my $block  = $frame2->Checkbutton(-text      => 'Block name',
                                      -underline => 0,
                                      -variable  => \$block_on)->pack;
    my $char   = $frame2->Checkbutton(-text      => 'Char name',
                                      -underline => 1,
                                      -variable  => \$char_on)->pack;

    #$button->bind('<Enter>' => sub { $button->invoke; print "enter\n" });
    #$window->bind('<Alt-F>' => sub { $button->focus; print "alf f\n" });
    #$window->bind('<Alt-C>' => sub { $cps->focus });
    #$window->bind('<Alt-B>' => sub { $block->focus });
    #$window->bind('<Alt-A>' => sub { $char->focus });
}

# Find blocks, CPs and character names.
# Parameters: - Tk::Table
#             - cp checkbutton value (Bool)
#             - block checkbutton value (Bool)
#             - char checkbutton value (Bool)
#             - entry value (Str)
sub focus_find {
    my ($table, $cps_on, $block_on, $char_on, $entry) = @_;

    my @children = $table->children;
    for my $child (@children) {
        if (ref $child eq 'Tk::Label' && defined $child->cget('-text') && $child->cget('-text') eq $entry) {
            $table->see($child);
            my $bg = $child->cget('-bg');
            $child->configure(-bg => 'blue');
            $table->bind('<Escape>' => sub {
                $child->configure(-bg => $bg);
                $table->bind('<Escape>' => '');
            });
            
            #$child->after(3000, sub { $child->configure(-bg => $bg) });
            last;
        }
    }
}

# Bind keys.
# Parameters: - Tk::MainWindow
#             - Tk::Table
sub bind_keys {
    my ($main, $table) = @_;

    $main->bind('<Control-q>' => sub { quit($main) } );
    $main->bind('<Control-f>' => sub { pop_find_window($main, $table) } );
    $table->bind('<Home>' => sub { $table->see(0, 0) } );
    $table->bind('<End>' => sub { $table->see($table->totalRows, 0) } );
}
