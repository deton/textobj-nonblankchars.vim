nonblankchars.vim - Text object for WORD (non-blank characters).

This plugin selects a sequence of non-blank characters, separated with
white space even if characters contain Japanese character.
(Vim treats a Japanese word/WORD as a sequence of same kind of characters
 (Kanji, Hiragana, Katakana, ...) when &encoding is euc-jp or cp932.)
This text object is same as aW/iW in &encoding=utf-8.
