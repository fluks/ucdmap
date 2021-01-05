#!/usr/bin/env perl

# charprop is extremely slow to go through all unicode characters. This
# should speed it up. https://www.perlmonks.org/?node_id=1221135
package MyUnicodeUCD;

use warnings;
use strict;

use constant EXPORT_OK => [
    qw(charprop prop_invmap),
];

use Unicode::UCD @{EXPORT_OK()};

use Exporter;
our @ISA=qw(Exporter);
our @EXPORT_OK = @{EXPORT_OK()};

use Memoize;
memoize 'prop_invmap';
*Unicode::UCD::prop_invmap = \&prop_invmap;

1;

package main;

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
use File::Basename;
use Unicode::UCD qw(charblocks charinfo charblock);
use Storable;
use File::HomeDir;
use File::Spec;
use File::Path qw(make_path);
use feature qw/state/;
use constant HELP_WINDOW_NAME => 'help';
use constant HELP_WINDOW_PATH => '.' . HELP_WINDOW_NAME;
use constant FIND_WINDOW_NAME => 'find';
use constant FIND_WINDOW_PATH => '.' . FIND_WINDOW_NAME;
use constant CHAR_WIDGET_NAME => 'character';
use constant CONFIG_DIR => File::Spec->catfile(File::HomeDir->my_home, '.config', 'ucdmap');
use constant CONFIG_CHARNAMES_FILE => File::Spec->catfile(CONFIG_DIR, 'charnames.bin');
use File::ShareDir qw(dist_file);
use Fcntl qw(:flock);

our $VERSION = '0.01';

my $options = {
    button_image => dist_file('ucdmap', 'arrow.png'),
    # Save pathnames of buttons to make it easier to find widgets.
    button_paths => []
};

my $pid;
if (-e CONFIG_CHARNAMES_FILE) {
    $options->{charnames} = retrieve(CONFIG_CHARNAMES_FILE);
}
else {
    # Any known file is good for locking. The child needs a lock first.
    my $file = lock_file(dist_file('ucdmap', 'arrow.png'));
    $pid = fork();
    if ($pid == 0) {
        print "Creating character name map...\n";
        my $charnames = get_all_charnames();
        make_path(CONFIG_DIR);
        store $charnames, CONFIG_CHARNAMES_FILE;
        # $fh is duped when forking.
        unlock_file($file);
        print "done!\n";
        exit;
    }
}

create_gui($options, $pid);

MainLoop;

sub lock_file {
    my ($file) = @_;

    open(my $fh, '<', $file) || die "Can't open for locking $file: $!";
    flock($fh, LOCK_EX) || die "Can't lock file $file: $!";

    return { filehandle => $fh, name => $file, };
}

sub unlock_file {
    my ($file) = @_;

    flock($file->{filehandle}, LOCK_UN) || die "Can't unlock file " . $file->{name} . ": $!";
    close($file->{filehandle}) || warn "Can't close file " . $file->{name} . ": $!";
}

# Create a hash for code point to character name. It's needed for quick name
# search.
sub get_all_charnames {
    my $charnames = {};
    for my $block (keys %{charblocks()}) {
        my $range = charblock($block)->[0];
        for (my $j = 0; $range->[0] + $j <= $range->[1]; $j++) {
            my $name = MyUnicodeUCD::charprop($range->[0] + $j, 'name');
            $charnames->{$range->[0] + $j} = $name;
        }
    }

    return $charnames;
}

# Create gui.
# Parameters: - options
#             - pid
sub create_gui {
    my ($opt, $pid) = @_;

    my $main = MainWindow->new(-title => basename($0));
    my $icon = $main->Photo(-file => dist_file('ucdmap', '108px-Unicode_logo.svg.png'), -format => 'png');
    $main->Icon(-image => $icon);
    $main->FullScreen;
    $main->packPropagate(0);
    $main->optionAdd('*font', 'Cursor 16');

    my @choices = ();
    add_find_frame($main, $opt, \@choices);

    my $pane = $main->Scrolled('Pane', qw/Name main_pane -scrollbars se -sticky w/);
    $pane->pack(qw/-fill both -expand 1 -side left -anchor w/);
    fill_pane($opt, $pane);
    $pane->focus;

    # Store all the search terms.
    my $menu = create_menu($main, $pane, \@choices, $opt);
    $main->configure(-menu => $menu);

    bind_keys($main, $pane, $opt, \@choices);

    if ($pid) {
        show_mapping_info($main, $opt);
    }
}

sub add_find_frame {
    my ($main, $opt, $choices) = @_;

    my $frame = $main->Frame->pack(qw/-fill x -padx 40/);

    my $top_frame = $frame->Frame->pack(qw/-anchor w -pady 5/);
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
Code point can be either decimal or hexadecimal and started with an 'x'. 'x41' and
'65' is 'A'.
MSG
    my $balloon = $frame->Balloon;
    $balloon->attach($entry->Subwidget('entry'), -balloonmsg => $message);

    # Selected radio button.
    state $selected_radio = 0;
    my %radio = (
        selected => \$selected_radio,
        block    => 0,
        char     => 1,
        cp       => 2,
        none     => -1
    );
    my $last_found_item = { widget => undef, original_bg => undef };
    my $last_search_term = '';
    my $last_radio_choice = $radio{none};
    my $match_paths = [];
    use constant {
        PREVIOUS    => -1,
        NEXT        => 1,
        INDEX_START => -1
    };
    my $last_index = INDEX_START;
    my $direction = NEXT;
    my $button = $top_frame->Button(-text      => 'Find',
                                    -underline => 1,
                                    -command   => sub {
        return
            unless validate_choice($choice, \%radio);
        restore_last_found_item_bg($last_found_item);
        my $pane = $main->Widget('.main_pane');
        unless ($choice eq $last_search_term && $last_radio_choice == $selected_radio) {
            $match_paths = find_matches($pane, \%radio, $choice, $opt);
            $last_index = INDEX_START;
        }
        if (defined ($last_found_item->{widget} =
                show_found_item($match_paths, $main, \$last_index, $direction))) {
            save_and_change_bg($last_found_item);
            position_found_widget($last_found_item->{widget}, $pane);
        }
        add_choice_to_list($choice, $entry);
        $last_search_term = $choice;
        $last_radio_choice = $selected_radio;
    })->pack(qw/-side left -anchor n -padx 10/);

    my $previous = $top_frame->Button(qw/-text Previous -underline 0/, -command => sub {
        $direction = PREVIOUS;
        $button->invoke;
    })->pack(qw/-side left -anchor n -padx 2/);
    my $next = $top_frame->Button(qw/-text Next -underline 0/, -command => sub {
        $direction = NEXT;
        $button->invoke;
    })->pack(qw/-side left -anchor n -padx 2/);

    my $bottom_frame = $frame->Frame->pack(qw/-fill both -expand 1/);
    my $block = $bottom_frame->Radiobutton(-text      => 'Block name',
                                           -underline => 0,
                                           -value     => $radio{block},
                                           -variable  => \$selected_radio)->pack(qw/-side left/);
    my $char  = $bottom_frame->Radiobutton(-text      => 'Character name',
                                           -underline => 1,
                                           -value     => $radio{char},
                                           -variable  => \$selected_radio)->pack(qw/-side left/);
    my $cps   = $bottom_frame->Radiobutton(-text      => 'Code point',
                                           -underline => 0,
                                           -value     => $radio{cp},
                                           -variable  => \$selected_radio)->pack(qw/-side left/);

    $entry->bind('Tk::Entry', '<Return>' => sub {
        $direction = NEXT;
        $button->invoke;
    } );
    $main->bind('<Alt-i>' => sub {
        $entry->focus;
        $direction = NEXT;
        $button->invoke;
    } );
    $main->bind('<Alt-n>' => sub {
        $direction = NEXT;
        $button->invoke;
    } );
    $main->bind('<Alt-p>' => sub {
        $direction = PREVIOUS;
        $button->invoke;
    } );
    $main->bind('<Alt-c>' => sub { $cps->select   } );
    $main->bind('<Alt-b>' => sub { $block->select } );
    $main->bind('<Alt-h>' => sub { $char->select  } );
    $entry->bind('<Down>'   => sub {
        $entry->Subwidget('arrow')->focus;
        $entry->Subwidget('arrow')->eventGenerate('<space>');
     } );
}

sub show_mapping_info {
    my ($main, $opt) = @_;

    my $text = 'Creating character name map';
    my $window = $main->Toplevel(-title => $text);
    $window->Label('-text', $text)->pack;
    my $label = $window->Label(qw/-text Waiting.../)->pack;
    # Almost center.
    $window->geometry('+' . ($main->screenwidth / 2) .  '+' . ($main->screenheight / 2));

    my $id;
    $id = $window->repeat(1000, sub {
        $label->configure('-text', $label->cget('-text') . '.');
        if (-e CONFIG_CHARNAMES_FILE) {
            # Possibly wait to make sure that the mapping is written fully to disk.
            my $file = lock_file(dist_file('ucdmap', 'arrow.png'));
            $opt->{charnames} = retrieve(CONFIG_CHARNAMES_FILE);
            unlock_file($file);
            $id->cancel;
            $window->destroy;
        }
    });
}

# Create menu.
# Parameters: - Tk::MainWindow
#             - Tk::Pane
#             - (ArrayRef)
# Returns:    Tk::Menu
sub create_menu {
    my ($main, $pane, $choices, $opt) = @_;

    my $menu = $main->Menu(-type => 'normal');
    $menu->cascade(qw/-label ~File -tearoff 0 -menuitems/ => [
        [
            Button       => '~Quit',
            -command     => sub { quit($main) },
            -accelerator => 'Ctrl+Q'
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
    my %charblocks = %{charblocks()};
    for my $group (sort { $charblocks{$a}->[0]->[0] <=> $charblocks{$b}->[0]->[0] } keys %charblocks) {
        my $frame = $pane->Frame->grid(qw/-sticky nw/);
        my ($block_start, $block_end) = @{$charblocks{$group}->[0]};
        my $button = $frame->Button(-text     => $group,
                                    -image    => $arrow,
                                    -compound => 'left',
                                    -anchor   => 'n',
                                    -relief   => 'flat')->
            pack(qw/-expand 1 -fill both -side left -padx 5 -anchor w/);
        $balloon->attach($button, -balloonmsg => 'Code points: ' . sprintf('0x%.4x', $block_start) . '-' . sprintf('0x%.4x', $block_end));

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
                    for my $cp ($block_start .. $block_end) {
                        my $label = $chars_frame->Label(
                            Name         => CHAR_WIDGET_NAME,
                            # Crashes for example when emoticons is opened.
                            -text        => defined $cp ? chr($cp) : '',
                            -borderwidth => 1,
                            -height      => 2,
                            -width       => 2,
                            -relief      => 'groove')->
                                grid(-row => $row, -column => $col++, -sticky => 'nsew');
                        my $msg = 'Code: ' . (sprintf("0x%.4x", $cp) || 'NO CP');
                        my $info = charinfo($cp);
                        if ($info) {
                            $msg .= "\nName: $info->{name}\nScript: $info->{script}";
                        }
                        $balloon->attach($label, -balloonmsg => $msg);

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

# Show found character entry.
# Parameters: - paths of matching widgets (ArrayRef)
#             - Tk::Pane
#             - index of last shown widget (IntRef)
#             - next(1) or previous(-1) match (Int)
# Returns:    next found widget or undef if none found or something unexpected happened
sub show_found_item {
    my ($match_paths, $pane, $last_index, $direction) = @_;

    return undef
        unless @$match_paths;

    # Index out of bounds, so set index either to 0 or to last.
    $$last_index = $#{ $match_paths }
        if (($$last_index += $direction) < 0);
    $$last_index = 0
        if $$last_index > $#{ $match_paths };

    my $button = $pane->Widget($match_paths->[$$last_index]);
    # Path is a button path.
    return $button
        if ref $button eq 'Tk::Button';

    # Path is a character path.
    # XXX Once a character block is shown and later hidden, in this stage $button contains
    # a Tk::Label instance. TODO exploit it?
    $match_paths->[$$last_index] =~ /(.*)(\.\w+){2}$/;
    my $outer_frame_path = $1;
    my $outer_frame = undef;
    unless (defined ($outer_frame = $pane->Widget($outer_frame_path))) {
        return undef;
    }
    ($button, my $char_frame) = $outer_frame->children;
    $button->invoke
        unless is_visible($pane, $char_frame->PathName);

    return $pane->Widget($match_paths->[$$last_index]);
}

# Validate entered search term.
# Only really for code point search.
# Parameters: - a search term (Str)
#             - selected radiobutton (HashRef)
# Returns:    true if valid search term, false otherwise            
sub validate_choice {
    my ($choice, $radio) = @_;

    return 0
        unless $choice;
    return 0
        if ${ $radio->{selected} } == $radio->{none};
    return 0
        if (${ $radio->{selected} } == $radio->{cp} && $choice !~ m/(^x[a-h0-9]+)|[0-9]+$/i);
    return 1;
}

# Find paths for character names and blocks and CPs.
# Parameters: - Tk::Pane
#             - selected radiobutton (HashRef)
#             - entry value (Str)
#             - options (HashRef)
# Returns:    a reference to paths of widgets which content matches search,
#             or empty list if no matches found
sub find_matches {
    my ($pane, $radio, $choice, $opt) = @_;

    my @match_paths;
    my $regexp = qr/$choice/i;
    my %charblocks = %{charblocks()};
    my @groups = sort { $charblocks{$a}->[0]->[0] <=> $charblocks{$b}->[0]->[0] } keys %charblocks;
    GROUPS: for (my $i = 0; $i < scalar @groups; $i++) {
        my $group = $groups[$i];
        # Just to be secure. Probably not needed?
        next
            unless exists $opt->{button_paths}->[$i];
        my $button = $pane->Widget($opt->{button_paths}->[$i]);
        my $char_path_end = '.frame.character';
        my $parent_path = $button->parent->PathName;
        my $char_path = $parent_path . $char_path_end;

        if (${ $radio->{selected} } == $radio->{block} && $group =~ $regexp) {
            push @match_paths, $opt->{button_paths}->[$i];
        }
        elsif (${ $radio->{selected} } == $radio->{char}) {
            my $range = charblock($group)->[0];
            for (my $j = 0; $range->[0] + $j <= $range->[1]; $j++) {
                my $name = $opt->{charnames}->{$range->[0] + $j};
                if ($name && $name =~ $regexp) {
                    push @match_paths, $char_path . ($j || '');
                }
            }
        }
        elsif (${ $radio->{selected} } == $radio->{cp}) {
            my $base_10_choice = substr($choice, 0, 1) eq 'x' ? hex $choice : $choice;
            my $range = charblock($group)->[0];
            next
                if ($base_10_choice < $range->[0] || $base_10_choice > $range->[1]);

            for (my $j = 0; $range->[0] + $j <= $range->[1]; $j++) {
                if ($range->[0] + $j == $base_10_choice) {
                    push @match_paths, $char_path . ($j || '');
                    last GROUPS;
                }
            }
        }
    }

    return \@match_paths;
}

# Restore background of the most recently found widget.
# Parameters: last found item (HashRef)
sub restore_last_found_item_bg {
    my $last_found_item = shift;

    $last_found_item->{widget}->configure('-bg' => $last_found_item->{original_bg})
        if defined $last_found_item->{widget};
}

# Save original background of the found widget and change current background color.
# Parameters: last found item (HashRef)
sub save_and_change_bg {
    my $last_found_item = shift;

    my $widget = $last_found_item->{widget};
    my $original_bg = $widget->cget('-bg');
    $last_found_item->{original_bg} = $original_bg;

    $widget->configure('-bg' => 'lightblue');
}

# Position the found widget to top left on the screen.
# Parameters: - found widget (Tk::Widget)
#             - Tk::Pane
sub position_found_widget {
    my ($widget, $pane) = @_;

    $pane->yview($widget);
    $pane->xview($widget);
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

# Check is widget a character widget.
# Parameters: Tk::widget
# Returns:    true if widget is a character widget, false otherwise
sub is_char_widget {
    my $widget = shift;

    return index($widget->name, CHAR_WIDGET_NAME) == 0;
}

# When searched, add search term (choice) to BrowseEntry's list of searches.
# Save the same term only once and in alphabetic order.
# Parameters: - search term (Str)
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

# Bind keys.
# Parameters: - Tk::MainWindow
#             - Tk::Pane
#             - options (HashRef)
#             - list to store searches (ArrayRef)
sub bind_keys {
    my ($main, $pane, $opt, $choices) = @_;

    $main->bind('<Control-q>' => sub { quit($main) } );
    $main->bind('<Control-h>' => sub {
        # TODO Try to return a ref to toplevel and check that way.
        return
            if popup_opened($main, HELP_WINDOW_PATH);
        pop_help($main);
    } );
    $main->bind('<Home>'     => sub { $pane->yview(moveto => 0) } );
    $main->bind('<End>'      => sub { $pane->yview(moveto => 1) } );
    $main->bind('<Prior>'    => sub { $pane->yview(scroll => -0.9, 'pages') } );
    $main->bind('<Next>'     => sub { $pane->yview(scroll => 0.9,  'pages') } );
    $main->bind('<Button-4>' => sub { $pane->yview(scroll => -0.3, 'pages') } );
    $main->bind('<Button-5>' => sub { $pane->yview(scroll => 0.3,  'pages') } );
    $main->bind('<Up>'       => sub { $pane->yview(scroll => -0.3, 'pages') } );
    $main->bind('<Down>'     => sub { $pane->yview(scroll => 0.3,  'pages') } );
    $main->bind('<Left>'     => sub { $pane->xview(scroll => -1,  'units')  } );
    $main->bind('<Right>'    => sub { $pane->xview(scroll => 1,  'units')   } );
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
            unless is_char_widget($w);

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

    my $is_chararacter = is_char_widget($widget);

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
            command  => 'Code point to clipboard',
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

    Control + I - Find
    Control + H - Show help
    Control + Q - Quit

    Mouse button 1 - Select area of characters

Find:

    Alt + I - Find next
    Alt + N - Find next
    Alt + P - Find previous
    Alt + B - Find blocks
    Alt + H - Find character names
    Alt + C - Find character code points
