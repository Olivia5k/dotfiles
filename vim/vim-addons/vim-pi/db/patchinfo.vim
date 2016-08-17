" This file is the "patch" to modify the repository informations retrieved
" from vim.sf.net
" Most numbers represent vim-script numbers
" See vimpi#PatchSources
let patch_repo = {}
let addon_info = {}
let addon_info_deps = {}
let renamings = {}
"▶1 Hooks
let hook = 'execute "lcd" fnameescape(%d."/ruby/command-t") | call vam#utils#RunShell("ruby extconf.rb") | call vam#utils#RunShell("make") | lcd -'
let addon_info.3025 = {}
let addon_info.3025['post-install-hook']     = hook
let addon_info.3025['post-update-hook']      = hook
let addon_info.3025['post-scms-update-hook'] = hook
unlet hook

" Assuming that system is used right and current user does not have priveleges 
" to write to this directory by default
let hook = 'echohl WarningMsg | echom "Copy vimtweak.dll to the same directory with gvim.exe" | echohl None'
let addon_info.687 = {}
let addon_info.687['post-install-hook']     = hook
let addon_info.687['post-update-hook']      = hook
let addon_info.687['post-scms-update-hook'] = hook
unlet hook

let hook = 'execute "lcd" fnameescape(%d."/smartgrep") | call vam#utils#RunShell("make") | lcd -'
let addon_info.4295 = {}
let addon_info.4295['post-install-hook']     = hook
let addon_info.4295['post-update-hook']      = hook
let addon_info.4295['post-scms-update-hook'] = hook
unlet hook

let hook = 'execute "lcd" fnameescape(%d."/plugin") | call vam#utils#RunShell("make") | lcd -'
let addon_info.4384 = {}
let addon_info.4384['post-install-hook']     = hook
let addon_info.4384['post-update-hook']      = hook
let addon_info.4384['post-scms-update-hook'] = hook
unlet hook
"▶1 Wrong archive names
" Author wrote that contents of vert.txt should go to vimrc, but it should work 
" fine as a script in plugin directory
let addon_info.1742={'archive_name': 'vert.vim'}
" It pretents being tar file, but it is actually tar.gz
let addon_info.4734={'archive_name': 'united-front.tar.gz'}
"▶1 Type corrections
let patch_repo.2368={'script-type': 'plugin'}
let patch_repo.1638={'script-type': 'plugin'}
let patch_repo.3868={'script-type': 'plugin'}
let patch_repo.1780={'script-type': 'syntax'}
let patch_repo.1686={'script-type': 'colors'}
let patch_repo.2527={'script-type': 'ftplugin'}
let patch_repo.4388={'script-type': 'ftplugin'}
let patch_repo.4750={'script-type': 'plugin'}
let patch_repo.3938={'script-type': 'plugin'}
"▶1 Fixing target directories
let patch_repo.2372={'target_dir': 'ftplugin/vlog'}
let patch_repo.1542={'target_dir': 'autoload'}
let patch_repo.1662={'target_dir': 'autoload'}
let patch_repo.2150={'target_dir': 'after/syntax'}
let patch_repo.2548={'target_dir': 'after/syntax'}
let patch_repo.2224={'target_dir': 'after/syntax'}
let patch_repo.2493={'target_dir': 'after/syntax'}
let patch_repo.4769={'target_dir': 'autoload/airline/themes'}
let patch_repo.2611={'strip-components': 0}
let patch_repo.2572={'strip-components': 0}
let patch_repo.2429={'strip-components': 0}
"▶1 Deprecations
call extend(patch_repo.1780, {'deprecated': "The syntax doesn't highlight \"\"\" strings correctly. I don't know how to contact the maintainer. So I moved the file and a fix into vim-addon-scala"})
call extend(patch_repo.1662, {'deprecated': "you should consider using ruby-vim instead"})
let patch_repo.113 ={'deprecated': "greputils supersedes this plugin"}

let patch_repo.143 ={'deprecated': "Merged into lh-vim-lib (vimscript #214)"}

let patch_repo.3432={'deprecated': "lazysnipmate’s update is just snipmate"}

let patch_repo.1963={'deprecated': "This was one of my biggest mistakes. This library won't be maintained. I'm mvoing contents into individual plugins slowly. Its just bloat"}

let patch_repo.3184={'deprecated': "Vimpluginloader evolved into unmaintainable blob. Use frawor if you seek for framework"}
let patch_repo.3325={'deprecated': "All functions from this plugin are available through `os' resource of @/os frawor module"}
let patch_repo.3187={'deprecated': "Deprecated in favour of FWC DSL defined in frawor plugin"}
let patch_repo.3188={'deprecated': "Deprecated in favour of FWC DSL defined in frawor plugin"}
let patch_repo.3186={'deprecated': "Functions from this plugin were either dropped or moved to frawor plugin, see its documentation"}

let patch_repo.1318={'deprecated': "Use snipmate instead. jano on irc reported that place holders don't work - last release 2006"}
let patch_repo.2540={'deprecated': "snipMate is an alias to snipmate now - so use 'snipmate'"}

let patch_repo.1272={'deprecated': "Superseded by vimscript #1431 (checksyntax)"}
let patch_repo.3233={'deprecated': "Superseded by Buffersaurus (vimscript #3620)"}
let patch_repo.3134={'deprecated': "This functionality has been rolled into tpope's vim-rvm"}
let patch_repo.108 ={'deprecated': "Superseded by vimscript #197 (genutils)"}
let patch_repo.1815={'deprecated': "This file is out of date and is now included in the Windows PowerShell Syntax Plugin package (vimscript #1327)"}
let patch_repo.1816={'deprecated': "This file is out of date and is now included in the Windows PowerShell Syntax Plugin package (vimscript #1327)"}
let patch_repo.2518={'deprecated': "This plugin has been replaced with the HyperList plugin (vimscript #4006)"}

let patch_repo.3874={'deprecated': "This script is no longer supported. Please use the matchit.vim plugin (already bundled with vim > 7) instead."}
let patch_repo.287 ={'deprecated': "This script has been retired. You should use #273"}
let patch_repo.2765={'deprecated': "Maintainer has changed. You should use #4452 (vim-javascript) now"}

let patch_repo.3102={'deprecated': "Author recommends using other plugin installers and states that this one is kept only for historical reasons"}

let patch_repo.2554={'deprecated': "Author says it is buggy and thus should not be used"}

let patch_repo.2850={'deprecated': "Functionality of this plugin is present in current NERDTree version"}

let patch_repo.103 ={'deprecated': "This plugin states that it requires Johannes Zellner's ScratchBuffer.vim plugin, but it is not available"}
let patch_repo.159 ={'deprecated': "No more maintained, use “minibufexplorer” instead. Requires git or you’ll have vimscript #3239 from vim.org which is itself deprecated (changed maintainer, new one posts only to git)"}

let patch_repo.3901={'deprecated': "Accident (?) duplicate of vimscript #3900"}
let patch_repo.4577={'deprecated': "Accident (?) duplicate of vimscript #4576"}

let patch_repo.3881={'deprecated': "Superseded by powerline (https://github.com/Lokaltog/powerline)"}


let patch_repo.3160={'deprecated': "According to github its superseded by vim-flake8 (vimscript #3927). You probably want to prefer syntastic anyway"}
let patch_repo.3161={'deprecated': "According to github its superseded by vim-flake8 (vimscript #3927). You probably want to prefer syntastic anyway"}

let patch_repo.2914={'deprecated': "Plugin seems to be no longer supported (last update 2 years ago (from Jan 2013)). Consider giving syntastic a try instead. It supports more backends"}
let patch_repo.3430={'deprecated': "Plugin seems to be no longer supported (last update 1 year ago (from Jan 2013)). Consider giving syntastic a try instead. It supports more backends"}

let patch_repo.4043={'deprecated': "The author recommends using neosnippet instead"}
"▶1 Missing dependencies
let addon_info_deps.1984 = [3252]
let addon_info_deps.2665 = [3464]
let addon_info_deps.2972 = [2806, 2971]
let addon_info_deps.884  = [294]
let addon_info_deps.337  = [338]
let addon_info_deps.746  = [745]
let addon_info_deps.788  = [166]
let addon_info_deps.1145 = [5]
let addon_info_deps.1236 = [1235, 935]
let addon_info_deps.1380 = [31]
let addon_info_deps.1717 = [1603]
let addon_info_deps.2742 = [1839]
let addon_info_deps.2997 = [293]
let addon_info_deps.3729 = [3597]
let addon_info_deps.3873 = [3023]
let addon_info_deps.3961 = [4145]
let addon_info_deps.3979 = [3431]
let addon_info_deps.4079 = [4050, 4056]
let addon_info_deps.4116 = [4115, 2544]
let addon_info_deps.4117 = [4116]
let addon_info_deps.4194 = [4193]
let addon_info_deps.4253 = [2646]
let addon_info_deps.4322 = [4321, 1359]
let addon_info_deps.4463 = [2467]
let addon_info_deps.4492 = [4491]
let addon_info_deps.4511 = [3590]
let addon_info_deps.4532 = [3133, 'twibill']
let addon_info_deps.4777 = [2136]
" airline dependants
let addon_info_deps.4769 = [4661]
let addon_info_deps.4756 = [4661]
" optional: 3396, 3476, unite-outline, vimproc, favstar-vim
" ConqueTerm dependants
let addon_info_deps.4222 = [2771]
let addon_info_deps.4601 = [2771]
" vim-misc dependants
let addon_info_deps.4586 = [4597]
let addon_info_deps.3144 = [4597]
let addon_info_deps.3625 = [4597]
let addon_info_deps.3169 = [4597]
let addon_info_deps.3375 = [4597]
let addon_info_deps.3148 = [4597]
let addon_info_deps.3150 = [4597]
let addon_info_deps.3123 = [4597]
" ctrl-p dependants
let addon_info_deps.4283 = [3736]
let addon_info_deps.4542 = [3736]
let addon_info_deps.4592 = [3736]
let addon_info_deps.4665 = [3736]
let addon_info_deps.4673 = [3736]
" vimproc dependants
let addon_info_deps.4336 = ['vimproc']
let addon_info_deps.4473 = ['vimproc']
" ingo-library dependants
let addon_info_deps.4449 = [4433]
let addon_info_deps.4462 = [4433]
let addon_info_deps.4465 = [4433, 4140]
let addon_info_deps.4654 = [4433]
let addon_info_deps.4658 = [4433, 3914]
let addon_info_deps.4795 = [4433]
" fugitive dependants
let addon_info_deps.3509 = [2975]
let addon_info_deps.3574 = [2975]
" CompleteHelper dependants
let addon_info_deps.3915 = [3914]
let addon_info_deps.4248 = [3914]
let addon_info_deps.4265 = [3914]
let addon_info_deps.4313 = [3914]
" MotionComplete dependants
let addon_info_deps.4266 = [4265]
let addon_info_deps.4267 = [4265]
" NERDTree plugins
let addon_info_deps.4138 = [1658]
let addon_info_deps.4672 = [1658]
" Non-Kana textobj-user dependants
let addon_info_deps.3382 = [2100, 39]
let addon_info_deps.4304 = [2100]
let addon_info_deps.4348 = [2100]
let addon_info_deps.4458 = [2100, 'vim-gitgutter']
let addon_info_deps.4508 = [2100]
let addon_info_deps.4570 = [2100]
" getvar dependants
let addon_info_deps.352  = [353, 354]
let addon_info_deps.994  = [353]
let addon_info_deps.2561 = [353]
" Writebackup dependants
let addon_info_deps.1829 = [1828]
let addon_info_deps.3107 = [1828]
let addon_info_deps.3940 = [1828, 1829]
" Non-tom link tlib dependants
let addon_info_deps.2141 = [1863]
" DfrankUtil dependants
let addon_info_deps.3872 = [3884]
let addon_info_deps.3221 = [3884, 3872]
" CountJump dependants
let addon_info_deps.3179 = [3130]
let addon_info_deps.3180 = [3130]
let addon_info_deps.3181 = [3130]
let addon_info_deps.3182 = [3130]
let addon_info_deps.3719 = [3130]
let addon_info_deps.3968 = [3130]
let addon_info_deps.3990 = [3130]
let addon_info_deps.3991 = [3130]
let addon_info_deps.4003 = [3130]
let addon_info_deps.4004 = [3130]
let addon_info_deps.4338 = [3130]
" VxLib dependants
let addon_info_deps.2606 = [3061]
let addon_info_deps.3060 = [3061]
" vsutil dependants
let addon_info_deps.1038 = [1056]
let addon_info_deps.1039 = [1056]
let addon_info_deps.1054 = [1056]
let addon_info_deps.1060 = [1056]
let addon_info_deps.1091 = [1056]
" genutils dependants
let addon_info_deps.107  = [197]
let addon_info_deps.129  = [197]
let addon_info_deps.171  = [197]
let addon_info_deps.598  = [197]
let addon_info_deps.745  = [197]
let addon_info_deps.900  = [197]
let addon_info_deps.953  = [197]
let addon_info_deps.1014 = [197]
let addon_info_deps.153  = [197, 166]
" genutils+multvals
let addon_info_deps.113  = [197, 171]
let addon_info_deps.158  = [197, 171]
let addon_info_deps.533  = [197, 171]
let addon_info_deps.568  = [197, 171]
let addon_info_deps.1062 = [197, 171]
" multvals
let addon_info_deps.564  = [171]
let addon_info_deps.1321 = [171]
let addon_info_deps.1386 = [171]
" let addon_info_deps.889  = [171]
" Unite.vim plugins
let addon_info_deps.3318 = [3396]
let addon_info_deps.3319 = [3396]
let addon_info_deps.3330 = [3396]
let addon_info_deps.3348 = [3396]
let addon_info_deps.3854 = [3396]
let addon_info_deps.3356 = [3396, 3133, 4019]
" let addon_info_deps.3485 = [3396]
" webapi dependants
let addon_info_deps.2423 = [4019]
let addon_info_deps.4143 = [4019]
" Neocomplcache plugins
let addon_info_deps.3423 = [2620]
let addon_info_deps.3440 = [2620]
let addon_info_deps.4043 = [2620]
" let addon_info_deps.4459 = [2620]
" Operator-user dependents
let addon_info_deps.3046 = [2692]
let addon_info_deps.3610 = [2692, 2944]
let addon_info_deps.3312 = [2692]
let addon_info_deps.3211 = [2692]
let addon_info_deps.2782 = [2692]
" snipMate dependents
let addon_info_deps.3249 = [2540]
let addon_info_deps.3664 = [2540]
let addon_info_deps.4276 = [2540, 2926]
" ▶2 Missing information for kana sources
let addon_info_deps.2336 = [2335]
let addon_info_deps.2403 = [2402]
let addon_info_deps.3892 = [3891]
" Script 2782 above, in operator-user dependants
" ku dependants
let addon_info_deps.2410 = [2337]
let addon_info_deps.2622 = [2337]
let addon_info_deps.2343 = [2337, 2338]
let addon_info_deps.2344 = [2337, 2335, 2336]
" textobj-user dependants
let addon_info_deps.2716 = [2100]
let addon_info_deps.2484 = [2100]
let addon_info_deps.2355 = [2100]
let addon_info_deps.2276 = [2100]
let addon_info_deps.2619 = [2100]
let addon_info_deps.2275 = [2100]
let addon_info_deps.2610 = [2100]
let addon_info_deps.2415 = [2100]
let addon_info_deps.2101 = [2100]
let addon_info_deps.3886 = [2100]
" eaasytags
let addon_info_deps.3114 = [4597]
"▶1 Missing runtimepath information for vim.org plugins
let addon_info.2424 = {'runtimepath': 'TransmitFTP'}
let addon_info.2883 = {'runtimepath': 'vimlib'}
let addon_info.2824 = {'runtimepath': 'vimlib'}
let addon_info.2847 = {'runtimepath': 'vimlib'}
let addon_info.663  = {'runtimepath': 'vim'}
call extend(addon_info.4295, {'runtimepath': 'smartgrep'})
"▶1 renamings
" Some names got wired titles by the script, try to use better ones
" Policies:
" - Renaming "Supertab%1643" to "Supertab" is OK if most users think one is more 
"   useful than the other.
" - Renaming "vim-textobj-*" to "textobj-*" or such is not feasable, because it 
"   is a property that you can pick and google for a key. People can tell me 
"   that they think differently about this to make me change my mind.
" - Horrible names like "tags_for_std_c_STL_streams_..." are the intented use 
"   case for renamings.
let renamings['tags_for_std_c_STL_streams_...'] = 'cpp_src'
let renamings['SuperTab%1643'] = 'Supertab'
let renamings['vim-addon-errorformats%4630'] = 'vim-addon-errorformats'

let r=[patch_repo, addon_info, addon_info_deps, renamings]
" vim: ft=vim ts=2 sts=2 sw=2 et fdm=marker fmr=▶,▲
