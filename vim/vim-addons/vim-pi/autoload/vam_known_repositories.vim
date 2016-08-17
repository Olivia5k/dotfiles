if !exists('g:vim_addon_manager')
    let g:vim_addon_manager={}
endif
let s:c=g:vim_addon_manager

let s:c['MergeSources']=get(g:vim_addon_manager, 'MergeSources', 'vam_known_repositories#MergeSources')

" the function returning the package pool.
" It can be configured in VAM and is run when command completion is used or
" when an update/ install action takes place.
" If you have custom packages overwrite this function and patch whatever you
" want to patch...
" 
" arg default_sources: See vimpi#www_vim_org_generated#Sources() (result is patched vamkr#patch#Patch)
" arg scm_plugin_sources: See  vimpi#scm#Sources() (vim_script_nr_X is replaced by plugin's name in keys)
fun! vam_known_repositories#MergeSources(user_sources, default_sources, scm_plugin_sources, patch_function, snr_to_name)

  let merge_strategy = get(s:c, 'scm_merge_strategy', 'force')

  let d = {}

  " (1) merge in www.vim.org sources:
  call extend(d, a:default_sources)

  " (2) merge in SCM sources only if support isn't disabled
  "     (See VAMs documentation 4. Options -> drop_{scm}_soucres)
  "
  " g:vim_addon_manager['scm_merge_strategy'] options:
  " force: prefer scm version over www.vim.org
  " keep:  only add scm version which have no released versions on www.vim.org
  " never: Don't add scm versions to list of known sources
  if merge_strategy isnot# 'never'
    call filter(a:scm_plugin_sources, '!get(s:c, "drop_".(v:val.type)."_sources", 0)')

    " old code, will be dropped: scm_support was renamed to drop_scm_sources
    call filter(a:scm_plugin_sources, 'get(s:c, (v:val.type)."_support", 1)')

    call extend(d, a:scm_plugin_sources, merge_strategy)
  endif

  let d=call(a:patch_function, [d, a:snr_to_name], {})

  " always keep what the user has set:
  call extend(a:user_sources, d, 'keep')
endf

" returns the package pool. Probably you want to overwrite MergeSources instead
" This function preparse www_vim_org and scm sources for MergeSources
" Its the default implementation for the one function returning the pool of
" known addons.
fun! vam_known_repositories#Pool()
  let www_vim_org = vimpi#LoadDBFile('vimorgsources.json')
  let drchip      = vimpi#LoadDBFile('drchip.json')
  let snr_to_name = vimpi#GetNrToNameMap(www_vim_org)
  let scm         = vimpi#GetSCMSources(snr_to_name, www_vim_org)

  " Add drchip sources to vim.org ones
  let default_sources = extend(copy(www_vim_org), drchip)

  "  start from scratch adding plugin sources to pool:
  let pool = copy(get(s:c, 'plugin_sources', {}))

  " now call MergeSources merge function so that user can pick scm over
  " www_vim_org sources as she desires.
  call call(s:c['MergeSources'], [pool, default_sources, scm, 'vimpi#PatchSources', snr_to_name], {})

  return pool
endf
