" vi:set ts=8 sts=2 sw=2 tw=0:
scriptencoding utf-8
" plugin/textobj/nonblankchars.vim - Text object for WORD(non-blank characters)
"   This plugin selects a sequence of non-blank characters, separated with
"   white space even if characters contain Japanese character.
"   (Vim treats a Japanese word/WORD as a sequence of same kind of characters
"    (Kanji, Hiragana, Katakana, ...) when &encoding is euc-jp or cp932.)
"   This text object is same as aW/iW in &encoding=utf-8.
"
" Maintainer: KIHARA Hideto <deton@m1.interq.or.jp>
" Last Change: 2013-03-16
" Copyright (c) 2013 KIHARA, Hideto
" License: So-called MIT/X license  {{{
"     Permission is hereby granted, free of charge, to any person obtaining
"     a copy of this software and associated documentation files (the
"     "Software"), to deal in the Software without restriction, including
"     without limitation the rights to use, copy, modify, merge, publish,
"     distribute, sublicense, and/or sell copies of the Software, and to
"     permit persons to whom the Software is furnished to do so, subject to
"     the following conditions:
"
"     The above copyright notice and this permission notice shall be included
"     in all copies or substantial portions of the Software.
"
"     THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
"     OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
"     MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
"     IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
"     CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
"     TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
"     SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
" }}}

if exists('g:loaded_textobj_nonblankchars')
  finish
endif
let g:loaded_textobj_nonblankchars = 1

onoremap <silent> <Plug>TextObjNonBlankCharsA :<C-U>call <SID>select_function_wrapper('<SID>select_a', 'o', v:count1)<CR>
vnoremap <silent> <Plug>TextObjNonBlankCharsVA <Esc>:call <SID>select_function_wrapperv('<SID>select_a', 0)<CR>
onoremap <silent> <Plug>TextObjNonBlankCharsI :<C-U>call <SID>select_function_wrapper('<SID>select_i', 'o', v:count1)<CR>
vnoremap <silent> <Plug>TextObjNonBlankCharsVI <Esc>:call <SID>select_function_wrapperv('<SID>select_i', 1)<CR>

if !get(g:, 'textobj_nonblankchars_no_default_key_mappings', 0)
  omap <silent> aE <Plug>TextObjNonBlankCharsA
  omap <silent> iE <Plug>TextObjNonBlankCharsI
  xmap <silent> aE <Plug>TextObjNonBlankCharsVA
  xmap <silent> iE <Plug>TextObjNonBlankCharsVI
endif

" from vim-textobj-user
function! s:select_function_wrapper(function_name, previous_mode, count1)
  let ORIG_POS = getpos('.')

  let _ = function(a:function_name)(a:count1, 0, 0)
  if _ is 0
    if a:previous_mode ==# 'v'
      normal! gv
    else  " if a:previous_mode ==# 'o'
      call setpos('.', ORIG_POS)
    endif
  else
    let [motion_type, start_position, end_position] = _
    execute 'normal!' motion_type
    call setpos('.', start_position)
    normal! o
    call setpos('.', end_position)
  endif
endfunction

function! s:pos_lt(pos1, pos2)  " less than
  return a:pos1[1] < a:pos2[1] || a:pos1[1] == a:pos2[1] && a:pos1[2] < a:pos2[2]
endfunction

function! s:select_function_wrapperv(function_name, inner)
  let cnt = v:prevcount
  if cnt == 0
    let cnt = 1
  endif
  let pos = getpos('.')
  execute 'normal! gvo' . "\<Esc>"
  let otherpos = getpos('.')
  execute 'normal! gvo' . "\<Esc>"

  " When start and end of the Visual area are the same (just after typing "v"):
  " select one object, the same as for using an operator.
  if pos == otherpos
    call s:select_function_wrapper(a:function_name, 'v', cnt)
    return
  endif

  " When start and end of the Visual area are not the same:
  " The area is extended by one object or the white
  " space up to the next object, or both for the "a" objects.
  " The direction in which this happens depends on which side of the Visual
  " area the cursor is.
  if s:pos_lt(pos, otherpos)
    " backward
    if a:inner
      call s:select_i(cnt, 1, 1)
    else
      call s:select_a_b(cnt)
    endif
  else
    if a:inner
      call s:select_i(cnt, 1, 0)
    else
      call s:select_a(cnt, 1, 0)
    endif
  endif
  let newpos = getpos('.')
  normal! gv
  call setpos('.', newpos)
endfunction

function! s:select_a(count1, visual, dummy)
  let spincluded = 0
  let lnum = line('.')
  let line = getline(lnum)
  if a:visual
    let st = getpos('.')
    " extend
    call search('.', '')
    let lnum = line('.')
  else
    if line == ''
      let st = getpos('.')
      let lnum += 1
      call cursor(lnum, 1)
      call search('\S', 'c', lnum)
    elseif match(line, '\%' . col('.') . 'c\s') != -1
      " select from beginning of spaces when cursor is on space.
      call search('\S\zs\s\|^', 'bc', lnum)
      let st = getpos('.')
      call search('\S', '', lnum)
      let spincluded = 1
    else
      call search('\s\zs\S\|^', 'bc', lnum)
      let st = getpos('.')
    endif
  endif
  let sflag = 'ce'
  let cnt = a:count1
  while cnt > 0
    if search('\S\+', sflag, lnum) == 0
      let lnum += 1
      call cursor(lnum, 1)
    endif
    let sflag = 'e'
    let cnt -= 1
  endwhile
  " include spaces after last word if cursor was not on space.
  if !spincluded
    if search('\%' . col('.') . 'c.\s\+', 'ce', lnum) == 0
      " include spaces before start position if no spaces after last word.
      let ed = getpos('.')
      call setpos('.', st)
      call search('\s\+\%' . col('.') . 'c.', 'bc', line('.'))
      let st = getpos('.')
      call setpos('.', ed)
    endif
  endif
  let ed = getpos('.')
  return ['v', st, ed]
endfunction

function! s:select_a_b(count1)
  let spincluded = 0
  let st = getpos('.')
  call search('.', 'b')
  let lnum = line('.')
  let sflag = 'bc'
  let cnt = a:count1
  while cnt > 0
    if search('\S\+', sflag, lnum) == 0
      let lnum -= 1
      call cursor(lnum, 0)
      call cursor(0, col('$'))
    endif
    let sflag = 'b'
    let cnt -= 1
  endwhile
  call search('\s\+\%' . col('.') . 'c.', 'bc', lnum)
  let ed = getpos('.')
  return ['v', st, ed]
endfunction

function! s:select_i(count1, visual, backward)
  let cnt = a:count1
  let lnum = line('.')
  let line = getline(lnum)
  if !a:visual
    if match(line, '\%' . col('.') . 'c\s') != -1
      " select from beginning of spaces when cursor is on space.
      call search('\S\zs\s\|^', 'bc', lnum)
    else
      call search('\s\zs\S\|^', 'bc', lnum)
    endif
  else
    " extend
    if a:backward
      call search('.', 'b')
    else
      call search('.', '')
    endif
    let lnum = line('.')
    let line = getline(lnum)
  endif
  let sflag = 'ce'
  let nextsflag = ''
  if a:backward
    let sflag = 'bc'
    let nextsflag = 'b'
  endif
  let st = getpos('.')
  while cnt > 0
    if line == ''
      let nexpat = '\S'
    elseif match(line, '\%' . col('.') . 'c\s') != -1 " cursor is on space
      call search('\s\+', sflag, lnum)
      let nextpat = '\S'
    else " cursor is on word
      call search('\S\+', sflag, lnum)
      let nextpat = '\s'
    endif

    let cnt -= 1
    if cnt <= 0
      break
    endif

    if search(nextpat, nextsflag, lnum) == 0
      if a:backward
	let lnum -= 1
	call cursor(lnum, 0)
	call cursor(0, col('$'))
      else
	let lnum += 1
	call cursor(lnum, 1)
      endif
      let line = getline(lnum)
    endif
  endwhile
  let ed = getpos('.')
  return ['v', st, ed]
endfunction
