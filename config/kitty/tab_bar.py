from kitty.fast_data_types import Screen
from kitty.tab_bar import DrawData, ExtraData, TabBarData, draw_title

def as_rgb(x: int) -> int:
    return (x << 8) | 2

def draw_tab(
    draw_data: DrawData, screen: Screen, tab: TabBarData,
    before: int, max_tab_length: int, index: int, is_last: bool,
    extra_data: ExtraData
) -> int:
    tab_bg = screen.cursor.bg
    default_bg = as_rgb(int(draw_data.default_bg))

    left_separator_symbol = ''
    right_separator_symbol, soft_separator_symbol = ('', '')
    min_title_length = 1 + 2
    start_draw = 2

    screen.cursor.bg = default_bg
    screen.cursor.fg = tab_bg
    screen.draw(left_separator_symbol)
    if screen.cursor.x == 0:
        start_draw = 1

    screen.cursor.bg = tab_bg
    if min_title_length >= max_tab_length:
        screen.draw('…')
    else:
        draw_title(draw_data, screen, tab, index, max_tab_length)
        extra = screen.cursor.x + start_draw - before - max_tab_length
        if extra > 0 and extra + 1 < screen.cursor.x:
            screen.cursor.x -= extra + 1
            screen.draw('…')

    screen.draw(' ')
    screen.cursor.bg = default_bg
    screen.cursor.fg = tab_bg
    screen.draw(right_separator_symbol)

    end = screen.cursor.x
    if end < screen.columns:
        screen.draw(' ')
    return end
