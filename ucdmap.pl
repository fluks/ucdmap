#!/usr/bin/env perl
use warnings;
use strict;
use Tk;
use Tk::Pane;
use Tk::Balloon;
use Tk::ROText;
use Tk::Menu;
use Tk::Wm;
use Tk::Font;
use Tk::widgets qw/PNG/;
use Tk::BrowseEntry;
use Storable qw/retrieve/;
use File::Basename;
use Data::Dumper;
use feature qw/state/;
use constant HELP_WINDOW_NAME => 'help';
use constant HELP_WINDOW_PATH => '.' . HELP_WINDOW_NAME;
use constant FIND_WINDOW_NAME => 'find';
use constant FIND_WINDOW_PATH => '.' . FIND_WINDOW_NAME;

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

    my $pane = $main->Scrolled('Pane', qw/-scrollbars se -sticky w/);
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
            -command     => sub {
                return
                    if popup_opened($main, HELP_WINDOW_PATH);
                pop_help($main);
            },
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
    # Cache characters, otherwise pressing same character block, would create
    # the same characters again and consume memory.
    my $char_cache = {};
    for my $group (@{ $opt->{ucd_map} }) {
        my $frame = $pane->Frame->grid(qw/-sticky nw/);
        my $block = $group->{block};
        my $button = $frame->Button(-text     => $block,
                                    -image    => $arrow,
                                    -compound => 'left',
                                    -anchor   => 'n',
                                    -relief   => 'flat')->
            pack(qw/-expand 1 -fill both -side left -padx 5 -anchor w/);
        $balloon->attach($button, -balloonmsg => cp_range($group));

        push @{ $opt->{button_paths} }, $button->PathName;

        my $chars_frame = $frame->Frame;
        # Are characters of a character block shown or hidden.
        my $is_visible = 1;
        $button->configure(-command => sub {
            if ($is_visible) {
                # Any individual key will do.
                my $id = $button->id;
                unless (exists $char_cache->{$id}) {
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
                    $char_cache->{$id} = $chars_frame;
                }
                $char_cache->{$id}->pack(qw/-side left -anchor e/);
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

# Show help window.
# Parameters: - Tk::MainWindow
sub pop_help {
    my ($main) = @_;

    # __DATA__ can be read only once.
    state $text = '';
    $text = read_help_text($main)
        unless $text;

    my $window = $main->Toplevel(Name => HELP_WINDOW_NAME, -title => 'Help');
    my $rotext = $window->Scrolled('ROText',
                                   -width      => 45,
                                   -scrollbars => 'e')->pack;
    $rotext->insert('end', $$text);         
    $rotext->configure(qw/-padx 5 -pady 5/);

    my $button = $window->Button(-text      => 'Ok',
                                 -underline => 0,
                                 -command   => sub { $window->destroy; })->pack;
    $button->focus;                                 

    $window->bind('<Escape>' => sub { $window->destroy; });
    $window->bind('<Alt-o>'  => sub { $window->destroy; });
    $window->bind('<Prior>'  => sub { $rotext->yviewScroll(-1, 'pages'); });
    $window->bind('<Next>'   => sub { $rotext->yviewScroll(1,  'pages'); });
}

# Read help text.
# Returns: help text (ScaRef)
sub read_help_text {
    local $/ = undef;
    my $text = <DATA>;

    return \$text;
}

# Pop up a window for finding blocks, CPs and character names.
# Parameters: - Tk::MainWindow
#             - Tk::Pane
#             - options
#             - choices stored to last closing the popup (ArrayRef)
sub pop_find_window {
    my ($main, $pane, $opt, $choices) = @_;

    my $window = $main->Toplevel(Name         => FIND_WINDOW_NAME,
                                 -title       => 'Find',
                                 -borderwidth => 5);
    $window->geometry('-0+0');
    my $balloon = $window->Balloon;

    my $top_frame = $window->Frame->pack(qw/-fill x -expand 1 -pady 5 -padx 5/);
    my $choice;
    my $entry = $top_frame->BrowseEntry(-listheight      => 10,
                                        -autolimitheight => 1,
                                        -variable        => \$choice)->
        pack(qw/-side left -expand 1 -fill x -anchor n -padx 5/);

    # Restore possible earlier choices. TODO Pass array of choices to add_choice_to_list()?
    add_choice_to_list(\$_, $entry) for @$choices;

    my $message =<<MSG;
Perl regular expressions are supported.
The search is case-insensitive.
Code points are stored as hexadecimals, regular expressions are not used for them.
For example, 'A' is '0041', leading zeros are not required. 
MSG
    $balloon->attach($entry->Subwidget('entry'), -balloonmsg => $message);
    $entry->focus;

    # Selected radio button.
    state $selected_radio = -1;
    my %radio = (
        selected => \$selected_radio,
        block    => 0,
        char     => 1,
        cp       => 2,
        none     => -1
    );
    # Save find states. Continue search where we're left.
    my ($group_index, $char_index) = (0, 0);
    my $last_found_item = { widget => undef, original_bg => undef };
    my $button = $top_frame->Button(-text      => 'Find',
                                    -underline => 0,
                                    -command   => sub {
        return                                        
            unless validate_choice(\$choice, \%radio);
        focus_find($pane, \%radio, \$choice, $opt, \$group_index, \$char_index, $last_found_item);
        add_choice_to_list(\$choice, $entry);                                        
    })->pack(qw/-side left -anchor n/);

    my $mid_frame = $window->Frame->pack(qw/-fill x -expand 1/);
    my $previous = $mid_frame->Button(qw/-text Previous -underline 0/)->
        pack(qw/-side right -anchor n -padx 2/);
    my $next = $mid_frame->Button(qw/-text Next -underline 0/)->
        pack(qw/-side right -anchor n -padx 2/);

    my $bottom_frame = $window->Frame->pack(qw/-fill both -expand 1/);
    my $block = $bottom_frame->Radiobutton(-text      => 'Block name',
                                           -underline => 0,
                                           -value     => $radio{block},
                                           -variable  => \$selected_radio)->pack(qw/-side left/);
    my $char  = $bottom_frame->Radiobutton(-text      => 'Char name',
                                           -underline => 1,
                                           -value     => $radio{char},
                                           -variable  => \$selected_radio)->pack(qw/-side left/);
    my $cps   = $bottom_frame->Radiobutton(-text      => 'CP',
                                           -underline => 0,
                                           -value     => $radio{cp},
                                           -variable  => \$selected_radio)->pack(qw/-side left/);

    $entry->bind('Tk::Entry', '<Return>' => sub { $button->invoke } );
    $entry->bind('<Down>'                => sub {
        $entry->Subwidget('arrow')->focus;
        $entry->Subwidget('arrow')->eventGenerate('<space>');
     } );
    $window->bind('<Alt-f>' => sub { $button->invoke } );
    $window->bind('<Alt-n>' => sub { $button->invoke } );
    $window->bind('<Alt-p>' => sub { $button->invoke } );
    $window->bind('<Alt-c>' => sub { $cps->select    } );
    $window->bind('<Alt-b>' => sub { $block->select  } );
    $window->bind('<Alt-h>' => sub { $char->select   } );

    # Save choices.
    $window->bind('<Escape>' => sub {
        destroy_popup($window, $entry, $choices, $last_found_item);
    } );
    # When window closed by pressing on the cross or Alt + F4.
    $window->protocol('WM_DELETE_WINDOW', sub {
        destroy_popup($window, $entry, $choices, $last_found_item);
    } );
}

# Validate entered search term.
# Only really for code point search.
# Parameters: - a reference to search term (RefStr)
#             - selected radiobutton (HashRef)
# Returns:    true if valid search term, false otherwise            
sub validate_choice {
    my ($choice, $radio) = @_;

    return 0
        unless $$choice;
    return 0
        if ${ $radio->{selected} } == $radio->{none};
    return 0
        if (${ $radio->{selected} } == $radio->{cp} && $$choice !~ m/^[a-h0-9]+$/i);
    return 1;
}

# TODO Previous not working. CP not working well. In the end of block or characters an
# extra search is needed to continue from the beginning.
# Find blocks, CPs and character names.
# Parameters: - Tk::Pane
#             - selected radiobutton (HashRef)
#             - entry value (Str)
#             - options (HashRef)
#             - group index, on which group last search was left
#             - character index, on which character last search was left
#             - last found widget (HashRef)
sub focus_find {
    my ($pane, $radio, $choice, $opt, $g_index, $c_index, $last_found_item) = @_;

    # Bring back original background of last matched widget. 
    $last_found_item->{widget}->configure('-bg' => $last_found_item->{original_bg})
        if defined $last_found_item->{widget};

    my $regexp = qr/$$choice/i;
    for (; $$g_index < scalar @{ $opt->{ucd_map} }; $$g_index++) {
        my $group = $opt->{ucd_map}->[$$g_index];
        # Just to be secure. Probably not needed?
        if (!exists $opt->{button_paths}->[$$g_index]) {
            $$g_index++;
            next;
        }
        my $button = $pane->Widget($opt->{button_paths}->[$$g_index]);

        if (${ $radio->{selected} } == $radio->{block}) {
            if (defined $group->{block} && $group->{block} =~ $regexp) {
                $pane->yview($button);
                $pane->xview($button);
                $last_found_item->{widget} = $button;
                $last_found_item->{original_bg} = $button->cget('-bg');
                $button->configure(-bg => 'blue');
                $$g_index++;
                return;
            }
        }
        if (${ $radio->{selected} } == $radio->{char} || ${ $radio->{selected} } == $radio->{cp}) {
            my $char_path_end = '.frame.character';
            my $parent_path = $button->parent->PathName;
            my $char_path = $parent_path . $char_path_end;

            for (; $$c_index < scalar @{ $group->{chars} }; $$c_index++) {
                my $char = $group->{chars}->[$$c_index];

                if ((${ $radio->{selected} } == $radio->{cp} && defined $char->{cp} &&
                    oct('0x' . $char->{cp}) == oct('0x' . $$choice)) ||
                    (${ $radio->{selected} } == $radio->{char} && $char->{name} =~ $regexp)) {
                    $button->invoke
                        unless is_visible($pane, $parent_path . '.frame');
                    my $char_widget = $pane->Widget($char_path . ($$c_index || ''));
                    $pane->yview($char_widget);
                    $pane->xview($char_widget);
                    $last_found_item->{widget} = $char_widget;
                    $last_found_item->{original_bg} = $char_widget->cget('-bg');
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
# Parameters: - a reference to search term (RefStr)
#             - Tk::BrowseEntry
sub add_choice_to_list {
    my ($choice, $entry) = @_;
    
    my @set = $entry->get(0, 'end');

    my $i;
    for ($i = 0; $i < scalar @set; $i++) {
        return if $$choice eq $set[$i];
        last if ($$choice cmp $set[$i]) < 0;
    }
    $entry->insert(($i > $#set ? 'end' : $i), $$choice);
}

# When destroying popup window, save choices list.
# If -choices array is used, -autolimitheight doesn't work, so choices must be saved
# separately.
# Parameters: - popup window (Tk::Toplevel)
#             - Tk::BrowseEntry
#             - choices (ArrayRef)
#             - last found widget (HashRef)
sub destroy_popup {
    my ($window, $entry, $choices, $last_found_item) = @_;

    # Clear background of a found widget.
    $last_found_item->{widget}->configure('-bg' => $last_found_item->{original_bg})
        if defined $last_found_item->{widget};

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
    $main->bind('<Control-h>' => sub {
        # TODO Try to return a ref to toplevel and check that way.
        return
            if popup_opened($main, HELP_WINDOW_PATH);
        pop_help($main);
    } );
    # Need to declare here, because BrowseEntry's -choices doesn't work with autolimitheight.
    my $choices = [];
    $main->bind('<Control-f>' => sub {
        return
            if popup_opened($main, FIND_WINDOW_PATH);
        pop_find_window($main, $pane, $opt, $choices);
    } );
    $main->bind('<Home>'     => sub { $pane->yview(moveto => 0) } );
    $main->bind('<End>'      => sub { $pane->yview(moveto => 1) } );
    $main->bind('<Prior>'    => sub { $pane->yview(scroll => -0.9, 'pages') } );
    $main->bind('<Next>'     => sub { $pane->yview(scroll => 0.9,  'pages') } );
    $main->bind('<Button-4>' => sub { $pane->yview(scroll => -0.3, 'pages') } );
    $main->bind('<Button-5>' => sub { $pane->yview(scroll => 0.3,  'pages') } );
    $main->bind('<Up>'       => sub { $pane->yview(scroll => -0.3, 'pages') } );
    $main->bind('<Down>'     => sub { $pane->yview(scroll => 0.3,  'pages') } );
    my $selected_chars = {};
    my ($sel_x1, $sel_y1) = (0, 0);
    $main->bind('<ButtonPress-1>' =>
        [ \&start_mouse_select, \$sel_x1, \$sel_y1, Ev('X'), Ev('Y') ] );
    $main->bind('<ButtonRelease-1>' =>
        [ \&end_mouse_select, $main, $selected_chars, \$sel_x1, \$sel_y1, Ev('X'), Ev('Y') ] );
    $main->bind('<Button-3>' => sub { popup_menu($_[0], $main, $selected_chars); } );
}

# Start selecting characters with mouse button 1.
# Parameters: - widget where button was pressed on (Tk::Widget)
#             - a reference to save x coordinate where button was pressed (ScaRef)
#             - a reference to save y coordinate where button was pressed (ScaRef)
#             - x coordinate where button was pressed (Int)
#             - y coordinate where button was pressed (Int)
sub start_mouse_select {
    my (undef, $sel_x1, $sel_y1, $x, $y) = @_;

    ($$sel_x1, $$sel_y1) = ($x, $y);
}

# Find characters on selected area and select them.
# Parameters: - widget where button was pressed
#             - Tk::MainWindow
#             - selected characters (HashRef)
#             - a reference to save x coordinate where button was pressed (ScaRef)
#             - a reference to save y coordinate where button was pressed (ScaRef)
#             - x coordinate where button was released (Int)
#             - y coordinate where button was released (Int)
sub end_mouse_select {
    my (undef, $main, $selected_chars, $sel_x1, $sel_y1, $sel_x2, $sel_y2) = @_;

    my $sel_square = { x1 => $$sel_x1, y1 => $$sel_y1, x2 => $sel_x2, y2 => $sel_y2 };
    switch_coords($sel_square);

    my @chars;
    $main->Walk(sub {
        my $w = shift;
    
        return
            if (index($w->name, 'character') != 0);

        my $label_square = {};
        ($label_square->{x1}, $label_square->{y1}) = ($w->rootx, $w->rooty);
        ($label_square->{x2}, $label_square->{y2}) =
            ($label_square->{x1} + $w->width, $label_square->{y1} + $w->height);

        if (squares_intersect($sel_square, $label_square) ||
            lines_cross($sel_square, $label_square)) {
            push @chars, $w;
        }
    });
    
    # Pressed and released over the same character. Select or unselect it.
    if (@chars == 1) {
        select_char($selected_chars, $chars[0]);
    }
    # Selection area covers more than one character, select all of them.
    else {
        select_all_chars($selected_chars, \@chars, 1);
    }
}

# Switch coordinates of the square area to have semantics of start and end,
# i.e., start coordinates are always smaller than end coordinates.
# Parameters: square (HashRef)
sub switch_coords {
    my ($square) = @_;

    if ($square->{x1} > $square->{x2}) {
        ($square->{x1}, $square->{x2}) = ($square->{x2}, $square->{x1});
    }
    if ($square->{y1} > $square->{y2}) {
        ($square->{y1}, $square->{y2}) = ($square->{y2}, $square->{y1});
    }
}

# Find if squares intersect.
# Parameters: - square (HashRef)
#             - square (HashRef)
# Returns:    true if containers intersect, false otherwise
sub squares_intersect {
    # Is a corner of a square inside other square and vice versa.
    my @squares = (@_, reverse @_);
    while (my ($s1, $s2) = splice @squares, 0, 2) {
        return 1
            if ($s2->{x1} > $s1->{x1} && $s2->{x1} < $s1->{x2} &&
                $s2->{y1} > $s1->{y1} && $s2->{y1} < $s1->{y2});
         return 1
            if ($s2->{x1} > $s1->{x1} && $s2->{x1} < $s1->{x2} &&
                $s2->{y2} > $s1->{y1} && $s2->{y2} < $s1->{y2});
         return 1
            if ($s2->{x2} > $s1->{x1} && $s2->{x2} < $s1->{x2} &&
                $s2->{y1} > $s1->{y1} && $s2->{y1} < $s1->{y2});
         return 1
            if ($s2->{x2} > $s1->{x1} && $s2->{x2} < $s1->{x2} &&
                $s2->{y2} > $s1->{y1} && $s2->{y2} < $s1->{y2});
    }
    return 0;
}

# Do lines of the squares cross.
# Not every possibility needs to be tested, because on other cases also the
# corner of a square is inside other square, which is already tested. Only the
# cases where one square "goes through other square" needs to be tested.
#  000 s2      o s1    s1 "goes through" s2 on both ascii images.
# oooooo s1  0 o 0 s2
#  000         o
sub lines_cross {
    my ($s1, $s2) = @_;

    return 1
        if ($s1->{x1} > $s2->{x1} && $s1->{x1} < $s2->{x2} &&
            $s2->{y1} > $s1->{y1} && $s2->{y1} < $s1->{y2});
    return 1
        if ($s2->{x1} > $s1->{x1} && $s2->{x1} < $s1->{x2} &&
            $s1->{y1} > $s2->{y1} && $s1->{y1} < $s2->{y2});

    return 0;
}

# Select a character and change it's background accordingly.
# Parameters: - selected characters (HashRef)
#             - character (Tk::Widget)
sub select_char {
    my ($selected_chars, $widget) = @_;

    my $id = $widget->id;
    if (exists $selected_chars->{$id}) {
        my $original_bg = $selected_chars->{$id}->{original_bg};
        $widget->configure('-bg' => $original_bg);
        delete $selected_chars->{$id};
    }
    else {
        $selected_chars->{$id}->{widget} = $widget;
        my $original_bg = $widget->cget('-bg');
        $selected_chars->{$id}->{original_bg} = $original_bg;
        $widget->configure('-bg' => 'red');
    }
}

# Select or unselect all characters in a block.
# Parameters: - selected characters (HashRef)
#             - characters (ArrayRef Tk::Label)
#             - select or unselect (Bool)
sub select_all_chars {
    my ($selected_chars, $characters, $on) = @_;

    for my $c (@$characters) {
        my $id = $c->id;
        if ($on && !exists $selected_chars->{$id}) {
            select_char($selected_chars, $c);
        }
        elsif (!$on && exists $selected_chars->{$id}) {
            select_char($selected_chars, $c);
        }
    }
}

# If popup is already opened, show it, don't create a new one.
# Parameters: - Tk::MainWindow
#             - widget pathname (Str)
# Returns:    true if popup is opened, false otherwise
sub popup_opened {
    my ($main, $pathname) = @_;
    
    my $widget = $main->Widget($pathname);
    return 0
        unless defined $widget;    

    $widget->raise;
    return 1;
}

# Pop up a mouse menu for copying character to clipboard.
# Parameters: - widget that right mouse button was pressed on
#             - Tk::MainWindow
#             - selected characters (HashRef)
sub popup_menu {
    my ($widget, $main, $selected_chars) = @_;

    my $is_chararacter = index($widget->name, 'character') == 0;

    my $menu = $main->Menu(qw/-type normal -tearoff 0 -menuitems/ => [
        [
            command  => 'Character to clipboard',
            -command => sub {
                $main->clipboardClear;
                $main->clipboardAppend($widget->cget('-text'));
            },
            -state   => $is_chararacter ? 'normal' : 'disabled',
        ],
        [
            command  => 'CP to clipboard',
            -command => sub {
                $main->clipboardClear;
                my $character = $widget->cget('-text');
                $main->clipboardAppend(sprintf "%x", ord($character));
            },
            -state   => $is_chararacter ? 'normal' : 'disabled',
        ],
        '',
        [
            command  => 'Selected characters to clipboard',
            -command => sub {
                $main->clipboardClear;
                my @chars;
                for my $char (values %$selected_chars) {
                    my $text = $char->{widget}->cget('-text');
                    push @chars, $text;
                }
                my $chars = join '', sort @chars;
                $main->clipboardAppend($chars);
            }
        ],
        '',
        [
            command  => 'Select all in block',
            -command => sub {
                select_all_chars($selected_chars, [ $widget->parent->children ], 1);
            },
            -state   => $is_chararacter ? 'normal' : 'disabled',
        ],
        [
            command  => 'Unselect all in block',
            -command => sub {
                select_all_chars($selected_chars, [ $widget->parent->children ], 0);
            },
            -state   => $is_chararacter ? 'normal' : 'disabled',
        ],

    ]);
    $menu->Popup(qw/-popover cursor/);
}

# This is text for help.
__DATA__
This program can only show characters for
which the needed fonts are installed.

Some character blocks might be missing
and also reserved characters are not included.

Keyboard shortcuts

Main window:

    Control + F - Find
    Control + H - Show help
    Control + Q - Quit

    Mouse button 1 - Select area of characters

Find window:

    Alt + F - Find next
    Alt + N - Find next
    Alt + P - Find previous
    Alt + B - Find blocks
    Alt + H - Find character names
    Alt + C - Find character code points
