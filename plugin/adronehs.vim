scriptencoding utf-8

if exists('g:loaded_adronehs')
  finish
endif
let g:loaded_adronehs = 1

let s:save_cpo = &cpo
set cpo&vim

"-------------------"

let g:adronehs_say_buffer_height = get(g:, 'adronehs_say_buffer_height', 2)
let g:adronehs_keymap_default    = get(g:, 'adronehs_keymap_default', 1)

let g:adronehs_data_dir = get(g:, 'adronehs_data_dir',
\	exists('$XDG_DATA_HOME') ? expand('$XDG_DATA_HOME/.adrone-hs')
\	                         : expand('~/.adrone-hs')
\)


command! -bar AdroneHsHome call adronehs#open_home_buffer()
command! -bar AdroneHsSay  call adronehs#open_say_buffer()


" adrone-hs-home buffer's key mappings
nnoremap <silent> <Plug>(adronehs_reload)       :<C-u>call adronehs#reshow()<CR>
nnoremap <silent> <Plug>(adronehs_go_to_future) :<C-u>call adronehs#show_future_adlog()<CR>
nnoremap <silent> <Plug>(adronehs_go_to_past)   :<C-u>call adronehs#show_past_adlog()<CR>
nnoremap <silent> <Plug>(adronehs_open_say)     :<C-u>call adronehs#open_say_buffer()<CR>

" adrone-hs-say buffer's key mappings
nnoremap <silent> <Plug>(adronehs_say_post) :<C-u>call adronehs#post_say()<CR>

"-------------------"

let &cpo = s:save_cpo
unlet s:save_cpo
