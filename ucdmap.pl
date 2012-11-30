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
use Tk::BrowseEntry;
use Storable qw/retrieve/;
use File::Basename;
use Data::Dumper;
use feature qw/state/;

my $options = {
    ucd_map          => undef,
    ucd_default_file => 'ucd.nstor',
    ucd_file         => 'ucd.nstor',
    button_image     => 'arrow.png',
    # Save pathnames of buttons to make it easier to find widgets.
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
        # Are characters of a character block shown or hidden.
        my $is_visible = 1;
        $button->configure(-command => sub {
            if ($is_visible) {
                my ($row, $col, $CHAR_COLUMNS_MAX) = (0, 0, 50);
                for my $char (@{ $group->{chars} }) {
                    my $cp = exists $char->{cp} ? $char->{cp} : undef;
                    my $label = $chars_frame->Label(
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

# Read help text.
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

    # __DATA__ can be read only once.
    state $text = '';
    $text = read_help_text($main)
        unless $text;
    $main->messageBox(-title => 'Help', -type => 'ok', -message => $$text);
}

# Pop up a window for finding blocks, CPs and character names.
# Parameters: - Tk::MainWindow
#             - Tk::Pane
#             - options
#             - choices stored to last closing the popup (ArrayRef)
sub pop_find_window {
    my ($main, $pane, $opt, $choices) = @_;

    my $window = $main->Toplevel(-title => 'Find');
    $window->geometry('300x100-0+0');
    my $balloon = $window->Balloon;

    my $top_frame = $window->Frame->pack(qw/-fill x -expand 1 -pady 5 -padx 5/);
    my $choice;
    my $entry = $top_frame->BrowseEntry(-listheight      => 10,
                                        -autolimitheight => 1,
                                        -variable        => \$choice)->
        pack(qw/-side left -expand 1 -fill x -anchor n -padx 5/);

    # Restore possible earlier choices. TODO Pass array of choices to add_choice_to_list()?
    add_choice_to_list($_, $entry) for @$choices;

    my $message =<<MSG;
Perl regular expressions are supported.
The search is case-insensitive.
Code points are stored as hexadecimals, regular expressions are not used for them.
For example, 'A' is '0041', leading zeros are not required. 
MSG
    $balloon->attach($entry->Subwidget('entry'), -balloonmsg => $message);
    $entry->focus;

    # Checkbuttons' states.
    state $cps_on = 0; state $block_on = 0; state $char_on = 0;
    # Save find states. Continue search where we're left.
    my ($group_index, $char_index) = (0, 0);
    my $button = $top_frame->Button(-text      => 'Find',
                                    -underline => 0,
                                    -command   => sub {
        return unless $choice;                                        
        focus_find($pane, \$block_on, \$char_on, \$cps_on, \$choice, $opt, \$group_index, \$char_index);
        add_choice_to_list($choice, $entry);                                        
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

    # Save choices.
    $window->bind('<Escape>'             => sub { destroy_popup($window, $entry, $choices); } );
    $entry->bind('Tk::Entry', '<Return>' => sub { $button->invoke  } );
    $entry->bind('<Down>'                => sub {
        $entry->Subwidget('arrow')->focus;
        $entry->Subwidget('arrow')->eventGenerate('<space>');
     } );
    $window->bind('<Alt-f>'              => sub { $button->invoke } );
    $window->bind('<Alt-n>'              => sub { $button->invoke } );
    $window->bind('<Alt-p>'              => sub { $button->invoke } );
    $window->bind('<Alt-c>'              => sub { $cps->toggle    } );
    $window->bind('<Alt-b>'              => sub { $block->toggle  } );
    $window->bind('<Alt-h>'              => sub { $char->toggle   } );

    # When window closed by pressing on the cross or Alt + F4.
    $window->protocol('WM_DELETE_WINDOW', sub { destroy_popup($window, $entry, $choices); } );
}

# Find blocks, CPs and character names.
# Parameters: - Tk::Pane
#             - block checkbutton value (Bool)
#             - char checkbutton value (Bool)
#             - cp checkbutton value (Bool)
#             - entry value (Str)
#             - options
#             - group index, on which group last search was left
#             - character index, on which character last search was left
sub focus_find {
    my ($pane, $block_on, $char_on, $cps_on, $choice, $opt, $g_index, $c_index) = @_;

    my $regexp = qr/$$choice/i;
    for (; $$g_index < scalar @{ $opt->{ucd_map} }; $$g_index++) {
        my $group = $opt->{ucd_map}->[$$g_index];
        my $button = $pane->Widget($opt->{button_paths}->[$$g_index]);
        if (!defined $button || ref $button ne 'Tk::Button') {
            $$g_index++;
            next;
        }

        if ($$block_on) {
            if (defined $group->{block} && $group->{block} =~ $regexp) {
                my $bg = $button->cget('-bg');
                $button->configure(-bg => 'blue');
                $pane->yview($button);
                $$g_index++;
                return;
            }
        }
        if ($$char_on || $$cps_on) {
            my $char_path_end = '.frame.character';
            my $parent_path = $button->parent->PathName;
            my $char_path = $parent_path . $char_path_end;
            for (; $$c_index < scalar @{ $group->{chars} }; $$c_index++) {
                my $char = $group->{chars}->[$$c_index];

                if (($$cps_on && defined $char->{cp} && oct('0x' . $char->{cp}) == oct('0x' . $$choice)) ||
                    ($$char_on && $char->{name} =~ $regexp)) {
                    $pane->yview($button);
                    $button->invoke
                        unless is_visible($pane, $button->parent->PathName . '.frame');
                    my $char_widget = $pane->Widget($char_path . ($$c_index || ''));
                    $char_widget->configure(-bg => 'blue');
                    $$c_index++;
                    return;
                }
            }
            $$c_index = 0;
        }
    }
    $$g_index = 0;
}

# Test is a widget shown.
# Parameters: - some parent(TODO is this relative?) widget Tk::Widget
#             - pathname of the tested widget (Str)
# Returns:    undefined if widget not found at all, false if not shown, true otherwise
sub is_visible {
    my ($root_widget, $pathname) = @_;

    my $widget = $root_widget->Widget($pathname);
    return undef
        unless defined $widget;
    return $widget->ismapped;
}

# When searched, add search term (choice) to BrowseEntry's list of searches.
# Save the same term only once and in alphabetic order.
# Parameters: - search term (String)
#             - Tk::BrowseEntry
sub add_choice_to_list {
    my ($choice, $entry) = @_;
    
    my @set = $entry->get(0, 'end');

    my $i;
    for ($i = 0; $i < scalar @set; $i++) {
        return if $choice eq $set[$i];
        last if ($choice cmp $set[$i]) < 0;
    }
    $entry->insert(($i > $#set ? 'end' : $i), $choice);
}

# When destroying popup window, save choices list.
# If -choices array is used, -autolimitheight doesn't work, so choices must be saved
# separately.
# Parameters: - popup window (Tk::Toplevel)
#             - Tk::BrowseEntry
#             - choices (ArrayRef)
sub destroy_popup {
    my ($window, $entry, $choices) = @_;

    push @$choices, $entry->get(0, 'end');
    $window->destroy;
}

# Bind keys.
# Parameters: - Tk::MainWindow
#             - Tk::Pane
#             - options
sub bind_keys {
    my ($main, $pane, $opt) = @_;

    $main->bind('<Control-q>' => sub { quit($main) } );
    $main->bind('<Control-h>' => sub { show_help($main) } );
    # Need to declare here, because BrowseEntry's -choices doesn't work with autolimitheight.
    my $choices = [];
    $main->bind('<Control-f>' => sub {
        return
            if popup_opened($main);
        pop_find_window($main, $pane, $opt, $choices);
    } );
    $main->bind('<Home>'      => sub { $pane->yview(moveto => 0) } );
    $main->bind('<End>'       => sub { $pane->yview(moveto => 1)  } );
    $main->bind('<Prior>'     => sub { $pane->yview(scroll => -0.9, 'pages') } );
    $main->bind('<Next>'      => sub { $pane->yview(scroll => 0.9, 'pages')  } );
    $main->bind('<Button-3>'  => sub { popup_menu($_[0], $main) } );
}

# If popup is already opened, show it, don't create a new one.
# Parameters: Tk::MainWindow
# Returns:    true if popup is opened, false otherwise
sub popup_opened {
    my ($main) = @_;
    
    my $popup_pathname = '.toplevel';
    my $popup = $main->Widget($popup_pathname);
    return 0
        unless defined $popup;    

    $popup->raise;
    return 1;
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
