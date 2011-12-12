" TODO:
" Refine the GIT to handle non-branch commits better
" Better colors in the end
" Move filetype to the left?
" Add quickfix and loclist counters
" Add special ft for gitcommit
" \ Pl#Segment("%{substitute(SyntasticStatuslineFlag(), '\\[Syntax: line:\\(\\d\\+\\) \\((\\(\\d\\+\\))\\)\\?\\]', ' \\3 err ', 'i')}",

call Pl#Statusline(
	\ Pl#Segment("  %-2{Stl_GetMode()} ",
		\ Pl#HiCurrent(   Pl#FG(245), Pl#BG(232), Pl#Attr('bold')),
		\ Pl#HiInsert(    Pl#FG(232), Pl#BG(32), Pl#Attr('bold')),
		\ Pl#HiNonCurrent(Pl#FG(232), Pl#BG(232))
		\ ),
	\
	\ Pl#Segment("%{Stl_GetBranch('$branch')}",
		\ exists('g:loaded_fugitive') && g:loaded_fugitive == 1,
		\
		\ Pl#HiCurrent(   Pl#FG(250), Pl#BG(236)),
		\ Pl#HiInsert(    Pl#FG(250), Pl#BG(236)),
		\ Pl#HiNonCurrent(Pl#FG(239), Pl#BG(234))
		\ ),
	\
	\ Pl#SegmentGroup(
		\ Pl#HiCurrent(   Pl#BG(235)),
		\ Pl#HiInsert(    Pl#BG(235)),
		\ Pl#HiNonCurrent(Pl#BG(234)),
		\
		\ Pl#Segment("%{&readonly ? '  $ro ' : ''}",
			\ Pl#HiCurrent(   Pl#FG(196)),
			\ Pl#HiInsert(    Pl#FG(196)),
			\ Pl#HiNonCurrent(Pl#FG(239))
			\ ),
		\ Pl#Segment("%{getbufvar(bufnr(bufname('%')), '&mod') == 1 ? '  '.substitute(bufname('%'), '.*/', '', '').' ' : ''}",
			\ Pl#HiCurrent(   Pl#FG(196), Pl#Attr('bold')),
			\ Pl#HiInsert(    Pl#FG(196), Pl#Attr('bold')),
			\ Pl#HiNonCurrent(Pl#FG(244), Pl#Attr('bold'))
			\ ),
		\ Pl#Segment("%{getbufvar(bufnr(bufname('%')), '&mod') == 0 ? '  '.substitute(bufname('%'), '.*/', '', '').' ' : ''}",
			\ Pl#HiCurrent(   Pl#FG(221), Pl#Attr('bold')),
			\ Pl#HiInsert(    Pl#FG(221), Pl#Attr('bold')),
			\ Pl#HiNonCurrent(Pl#FG(244), Pl#Attr('bold'))
			\ ),
		\  Pl#Segment("%W ",
			\ Pl#HiCurrent(   Pl#FG(250)),
			\ Pl#HiInsert(    Pl#FG(250)),
			\ Pl#HiNonCurrent(Pl#FG(239))
			\ ),
		\ Pl#Segment("%{Stl_GetSyntaxErrors('$line')}",
			\ exists('g:loaded_syntastic_plugin') && g:loaded_syntastic_plugin == 1,
			\
			\ Pl#HiCurrent(   Pl#FG(255), Pl#BG(160), Pl#Attr('bold')),
			\ Pl#HiInsert(    Pl#FG(255), Pl#BG(160), Pl#Attr('bold')),
			\ Pl#HiNonCurrent(Pl#FG(239), Pl#BG(160), Pl#Attr('bold'))
			\ ),
		\ Pl#Segment("%{StatuslineLongLineWarning()}",
			\ Pl#HiCurrent(   Pl#FG(124), Pl#BG(234), Pl#Attr('bold')),
			\ Pl#HiInsert(    Pl#FG(124), Pl#BG(234), Pl#Attr('bold')),
			\ Pl#HiNonCurrent(Pl#FG(239), Pl#BG(234), Pl#Attr('bold'))
			\ )
		\ ),
	\
	\ Pl#Split(
		\ Pl#HiCurrent(   Pl#BG(234)),
		\ Pl#HiInsert(    Pl#BG(234)),
		\ Pl#HiNonCurrent(Pl#BG(234))
		\ ),
	\
	\ Pl#Segment("%{NonDefaultOptions()} ",
		\ Pl#HiCurrent(   Pl#FG(202), Pl#BG(234), Pl#Attr('bold')),
		\ Pl#HiInsert(    Pl#FG(202), Pl#BG(234), Pl#Attr('bold'))
		\ ),
	\
	\ Pl#Segment(" %{strlen(&ft) ? &ft : 'n/a'} ",
		\ Pl#HiCurrent(   Pl#FG(248), Pl#BG(235)),
		\ Pl#HiInsert(    Pl#FG(248), Pl#BG(235)),
		\ ),
	\
	\ Pl#Segment(" %4(%p%%%) ",
		\ Pl#HiCurrent(   Pl#FG(250), Pl#BG(236)),
		\ Pl#HiInsert(    Pl#FG(250), Pl#BG(236)),
		\ Pl#HiNonCurrent(Pl#FG(250), Pl#BG(236))
		\ ),
	\
	\ Pl#SegmentGroup(
		\ Pl#HiCurrent(   Pl#BG(237)),
		\ Pl#HiInsert(    Pl#BG(237)),
		\ Pl#HiNonCurrent(Pl#BG(237)),
		\
		\ Pl#Segment(" %4(%l%)",
			\ Pl#HiCurrent(   Pl#FG(250), Pl#Attr('bold')),
			\ Pl#HiInsert(    Pl#FG(250), Pl#Attr('bold')),
			\ Pl#HiNonCurrent(Pl#FG(250))
			\ ),
		\ Pl#Segment(":%3(%c%) ",
			\ Pl#HiCurrent(   Pl#FG(244)),
			\ Pl#HiInsert(    Pl#FG(244)),
			\ Pl#HiNonCurrent(Pl#FG(244))
			\ )
		\ )
	\ )
