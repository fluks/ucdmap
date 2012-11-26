#!/usr/bin/env perl
use warnings;
use strict;
use Tk;
use Tk::Pane;
use Tk::Balloon;
use Tk::Menu;
use Tk::Wm;
use Tk::Font;
use Tk::widgets qw/PNG/;
use Storable qw/retrieve/;
use File::Basename;
use Data::Dumper;
use feature qw/state/;

my $options = {
    ucd_map          => undef,
    ucd_default_file => 'ucd.nstor',
    ucd_file         => 'ucd.nstor',
    button_image     => 'arrow.png',
    button_paths     => []
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
    $main->optionAdd('*font', 'Cursor');

    my $menu = create_menu($main);
    $main->configure(-menu => $menu);

    my $pane = $main->Scrolled('Pane', qw/-scrollbars se/);
    $pane->pack(qw/-fill both -expand 1 -side left -anchor w/);
    fill_pane($opt, $pane);
    $pane->focus;

    bind_keys($main, $pane, $opt);
}

# Create menu.
# Parameters: Tk::MainWindow
# Returns:    Tk::Menu
sub create_menu {
    my $main = shift;

    my $menu = $main->Menu(-type => 'normal');
    $menu->cascade(qw/-label ~File -tearoff 0 -menuitems/ => [
        [
            Button       => '~Quit',
            -command     => sub { quit($main) },
            -accelerator => 'Ctrl+Q'
        ],
    ]);
    $menu->cascade(qw/-label ~Edit -tearoff 0 -menuitems/ => [
        [
            Button   => '~Preferences',
            -command => sub { }
        ],
    ]);
    $menu->cascade(qw/-label ~About -tearoff 0 -menuitems/ => [
        [
            Button       => '~Help',
            -command     => sub { show_help($main) },
            -accelerator => 'Ctrl+H'
        ],
    ]);

    return $menu;
}

# Fill pane with ucd block names, ucd blocks and balloons of cp ranges.
# Parameters: - options
#             - Tk::Pane
sub fill_pane {
    my ($opt, $pane) = @_;

    my $arrow = $pane->Photo(-file => $opt->{button_image});
    my $balloon = $pane->Balloon;
    for my $group (@{ $opt->{ucd_map} }) {
        my $frame = $pane->Frame->grid(qw/-sticky nw/);
        my $block = $group->{block};
        my $button = $frame->Button(-text     => $block,
                                    -image    => $arrow,
                                    -compound => 'left',
                                    -relief   => 'flat')->
            pack(qw/-expand 1 -fill both -side left -padx 5 -anchor w/);
        $balloon->attach($button, -balloonmsg => cp_range($group));

        push @{ $opt->{button_paths} }, $button->PathName;

        my $chars_frame = $frame->Frame;
        my $is_visible = 1;
        $button->configure(-command => sub {
            if ($is_visible) {
                my ($row, $col, $CHAR_COLUMNS_MAX) = (0, 0, 50);
                for my $char (@{ $group->{chars} }) {
                    my $cp = exists $char->{cp} ? $char->{cp} : undef;
                    my $label = $chars_frame->Label(
                        # TODO Is renaming a widget good idea?
                        Name         => 'character',
                        -text        => defined $cp ? chr(oct('0x' . $cp)) : '',
                        -borderwidth => 1,
                        -relief      => 'groove')->
                            grid(-row => $row, -column => $col++, -sticky => 'nsew');
                    $balloon->attach($label, -balloonmsg => ($cp || 'NO CP') . "\n" . $char->{name});

                    if ($col % $CHAR_COLUMNS_MAX == 0) {
                        $col = 0;
                        $row++;
                    }
                }
                $chars_frame->pack(qw/-side left -anchor e/);
            }
            else {
                $chars_frame->packForget;
            }

            $is_visible = !$is_visible;
        });
    }
}

# Find character point range for a character block.
# Parameters: character block (ArrayRef)
# Returns:    character point range (Str)
sub cp_range {
    my $group = shift;

    my $chars = $group->{chars};
    my $first = $chars->[0]->{cp} // '?';
    my $last  = $chars->[-1]->{cp} // '?';

    return 'CP: ' . $first . '-' . $last;
}

# Quit program.
# Parameters: Tk::MainWindow
sub quit {
    my $main = shift;

    $main->destroy;
}

# Read help text only once.
# Returns: help text (ScaRef)
sub read_help_text {
    local $/ = undef;
    my $text = <DATA>;

    return \$text;
}

# Show help.
# Parameters: - Tk::MainWindow
sub show_help {
    my ($main) = @_;

    state $text = '';
    $text = read_help_text($main)
        unless $text;
    $main->messageBox(-title => 'Help', -type => 'ok', -message => $$text);
}

# Pop up a window for finding blocks, CPs and character names.
# Parameters: - Tk::MainWindow
#             - Tk::Pane
#             - options
sub pop_find_window {
    my ($main, $pane, $opt) = @_;

    my $window = $main->Toplevel(-title => 'Find');
    $window->geometry('300x100-0+0');
    my $balloon = $window->Balloon;

    my $top_frame = $window->Frame->pack(qw/-fill x -expand 1 -pady 5 -padx 5/);
    my $entry = $top_frame->Entry(qw/-font 14/)->
        pack(qw/-side left -expand 1 -fill x -anchor n -padx 5/);
    my $message =<<MSG;
Perl regular expressions are supported.
The search is case-insensitive.
Code points are stored as hexadecimals, regular expressions are not used for them.
For example, 'A' is '0041', leading zeros are not required. 
MSG
    $balloon->attach($entry, -balloonmsg => $message);
    $entry->focus;

    state $cps_on = 0; state $block_on = 0; state $char_on = 0;
    my $button = $top_frame->Button(-text      => 'Find',
                                    -underline => 0,
                                    -command   => sub {
        focus_find($pane, \$block_on, \$char_on, \$cps_on, $entry->get, $opt)
    })->pack(qw/-side left -anchor n/);

    my $mid_frame = $window->Frame->pack(qw/-fill x -expand 1/);
    my $previous = $mid_frame->Button(qw/-text Previous -underline 0/)->
        pack(qw/-side right -anchor n -padx 2/);
    my $next = $mid_frame->Button(qw/-text Next -underline 0/)->
        pack(qw/-side right -anchor n -padx 2/);

    my $bottom_frame = $window->Frame->pack;
    my $block = $bottom_frame->Checkbutton(-text      => 'Block name',
                                           -underline => 0,
                                           -variable  => \$block_on)->pack(qw/-side left/);
    my $char  = $bottom_frame->Checkbutton(-text      => 'Char name',
                                           -underline => 1,
                                           -variable  => \$char_on)->pack(qw/-side left/);
    my $cps   = $bottom_frame->Checkbutton(-text      => 'CP',
                                           -underline => 0,
                                           -variable  => \$cps_on)->pack(qw/-side left/);

    $window->bind('<Escape>'             => sub { $window->destroy } );
    $entry->bind('Tk::Entry', '<Return>' => sub { $button->invoke } );
    # TODO these doesn't work
    $main->bind('<Alt-f>'                => sub { $button->invoke } );
    $main->bind('<Alt-c>'                => sub { $cps->toggle } );
    $main->bind('<Alt-b>'                => sub { $block->toggle } );
    $main->bind('<Alt-a>'                => sub { $char->toggle } );
}

# Find blocks, CPs and character names.
# Parameters: - Tk::Pane
#             - block checkbutton value (Bool)
#             - char checkbutton value (Bool)
#             - cp checkbutton value (Bool)
#             - entry value (Str)
#             - options
sub focus_find {
    my ($pane, $block_on, $char_on, $cps_on, $entry, $opt) = @_;

    my $regexp = qr/$entry/i;
    my $g = 0;
    BLOCK: for my $block (@{ $opt->{ucd_map} }) {
        my $button = $pane->Widget($opt->{button_paths}->[$g]);
        next BLOCK if (!defined $button || ref $button ne 'Tk::Button');

        if ($$block_on) {
            if (defined $block->{block} && $block->{block} =~ $regexp) {
                my $bg = $button->cget('-bg');
                $button->configure(-bg => 'blue');
                $pane->yview($button);
                last BLOCK;
            }
        }
        if ($$char_on || $$cps_on) {
            my $char_path_end = '.frame.character';
            my $parent_path = $button->parent->PathName;
            my $char_path = $parent_path . $char_path_end;
            my $c = 0;
            for my $char (@{ $block->{chars} }) {
                if (($$cps_on && defined $char->{cp} && oct('0x' . $char->{cp}) == oct('0x' . $entry)) ||
                    ($$char_on && $char->{name} =~ $regexp)) {
                    $button->invoke;
                    $pane->yview($button);
                    my $char = $pane->Widget($char_path . ($c || ''));
                    $char->configure(-bg => 'blue');
                    last BLOCK;
                }
                $c++;
            }
        }

        $g++;
    }
}

# Bind keys.
# Parameters: - Tk::MainWindow
#             - Tk::Pane
#             - options
sub bind_keys {
    my ($main, $pane, $opt) = @_;

    $main->bind('<Control-q>' => sub { quit($main) } );
    $main->bind('<Control-h>' => sub { show_help($main) } );
    $main->bind('<Control-f>' => sub { pop_find_window($main, $pane, $opt) } );
    $main->bind('<Home>'      => sub { $pane->yview(moveto => 0) } );
    $main->bind('<End>'       => sub { $pane->yview(moveto => 1)  } );
    $main->bind('<Prior>'     => sub { $pane->yview(scroll => -0.9, 'pages') } );
    $main->bind('<Next>'      => sub { $pane->yview(scroll => 0.9, 'pages')  } );
    #$main->bind('<Button-3>'  => sub { print $_[0]->PathName,"\n" } );
    $main->bind('<Button-3>'  => sub { popup_menu($_[0], $main) } );
}

# Pop up a mouse menu for copying character to clipboard.
# Parameters: - widget that right mouse button was pressed on
#             - Tk::MainWindow
sub popup_menu {
    my ($widget, $main) = @_;

    return
        if (index($widget->name, 'character') != 0);

    my $menu = $main->Menu(qw/-type normal -tearoff 0 -menuitems/ => [
        [
            command  => 'Character to clipboard',
            -command => sub {
                $main->clipboardClear;
                $main->clipboardAppend($widget->cget('-text'));
            }
        ],
        [
            command   => 'CP to clipboard',
            -command => sub {
                $main->clipboardClear;
                my $character = $widget->cget('-text');
                $main->clipboardAppend(sprintf "%x", ord($character));
            }
        ]
    ]);
    $menu->Popup(qw/-popover cursor/);
}

# This is text for help.
__DATA__
This program can only show characters for which the needed fonts are installed.

Some character blocks might be missing and also reserved characters are not included.

Keyboard shortcuts

Main window:

    Control + F - Find
    Control + H - Show help
    Control + Q - Quit
