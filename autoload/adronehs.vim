let s:V   = vital#adronehs#new()
let s:Job = s:V.import('System.Job')

" ---

let s:ADRONE_HS_PATH = fnamemodify(expand('<sfile>'), ':p:h') . '/../adrone-hs'
lockvar! s:ADRONE_HS_PATH

function! s:stack_run(stack_cmd, on_exit) abort
	let l:cmd = printf('cd "%s"; stack exec adrone-hs -- %s', s:ADRONE_HS_PATH, a:stack_cmd)
	call s:Job.start(l:cmd, {'on_exit' : a:on_exit})
endfunction

function! s:notify_finishing(_, __, ___) abort
	echo 'adrone-hs.vim: the message was post !'
endfunction

" ---

function! adronehs#say(msg) abort
	call s:stack_run(printf('insert "%s"', a:msg), function('s:notify_finishing'))
endfunction
