" Vim syntax file
" Language : Self defined simple markup for notes in Vim
" Author/Maintainer : Samhita Kasula
" Modifier: Lowe Thiderman (daethorian@ninjaloot.se)
" Last Modified : 2010 Nov 2

if exists("b:current_syntax")
  finish
endif

if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

" TODO  or FIXME nested in a comment
syn keyword notesTodo contained TODO FIXME XXX

syn region notesMaster  start=/__/ skip=/\\\\\|\\__/ end=/__/ oneline
syn match notesMasterBody  /\(__:\)\@<=.*$/

" Comments are enclosed within // //
" matched in a single line only
syn region notesComment start=+//+  end=+//+ oneline contains=notesTodo

" Sentences starting with - or * are bullets
syn match notesBullet /^\s*- /
syn match notesBullet /^\s*\* /
syn match notesBullet /^\s*+ /

" Highlighting section(=) and subsection(-)
syn match notesSection /.*\(\n=\{3,60\}\([^=]\|\n\|\s\)\)\@=/ contains=@Section
syn match notesSection /.*\(\n\s\+=\{3,60\}\([^=]\|\n\|\s\)\)\@=/ contains=@Section
syn match notesSubsection /.*\(\n-\{3,60\}\([^-]\|\n\|\s\)\)\@=/ contains=@Section
syn match notesSubsection /.*\(\n\s\+-\{3,60\}\([^-]\|\n\|\s\)\)\@=/ contains=@Section

syn cluster Section contains=notesBold,notesItalic

" Highlight page breaks (characterized by 61 or more = or -)
syn match notesBreak /^=\{61,\}/
syn match notesBreak /\s\+=\{61,\}/
syn match notesBreak /^-\{61,\}/
syn match notesBreak /\s\+-\{61,\}/

" Highlight strings
syn region notesString start=/"/ skip=/\\\\\|\\"/ end=/"/ oneline
" syn region notesString start=/'/ skip=/\\\\\|\\'/ end=/'/ oneline

" Highlight keyword (within 's)
syn region notesKey start=/'/ skip=/\\\\\|\\'/ end=/'/ oneline

" Bold and Italic
syn match notesItalic / _[^_ 	]\+_/ms=s+2,me=e-1
syn match notesBold / \*[^* 	]*\*/ms=s+2,me=e-1

" A synchronisation guideline 
" For basic synching
syntax sync minlines=40 maxlines=500

" Highlighting and Linking :

hi def link notesTodo Todo

hi def link notesMaster Conditional
hi def link notesMasterBody Function

hi def link notesComment Comment

hi def link notesBullet Function

hi def link notesSection Type
hi def link notesSubsection Preproc

hi def link notesBreak Comment
hi def link notesString String
hi def link notesKey SpecialChar

hi  notesItalic gui=italic term=italic cterm=italic
hi  notesBold gui=bold term=bold cterm=bold

let b:current_syntax = "notes"

