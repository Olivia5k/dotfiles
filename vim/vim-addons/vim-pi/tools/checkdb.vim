"▶1 CheckVal : checks whether scm/scmnr value is valid
function CheckVal(s, mes, curauthor)
    try
        if has_key(a:curauthor, 'hook')
            let hook=a:curauthor.hook
        endif
        let val=eval(a:s)
        if val.type is# 'git' && val.url =~? '\V\w\+://github.com\[:/]\|git@github.com:'
            if val.url[:16] isnot# 'git://github.com/'
                throw 'Github URLs must start with git://github.com/'
            elseif val.url[-4:] is# '.git'
                throw 'Github URLs must not end with .git'
            endif
        endif
        return (val.type is# 'archive')*2+
                    \(has_key(val, 'addon-info') &&
                    \ !empty(filter(keys(val['addon-info']), 'stridx(v:val, "hook")!=-1')))
    catch
        throw printf(a:mes, a:s, v:exception)
    finally
        if exists('hook')
            unlet hook
        endif
    endtry
endfunction
"▲1
call feedkeys("\n\n\n\n\n")
let types = ['SCM unhooked source',
            \'SCM hooked source',
            \'archive source',
            \'archive hooked source']
try
    set rtp=.
    let g:vim_pi_running_hook_test=1
    "▶1 Load vimorgsources
    try
        let www_vim_org=vimpi#LoadDBFile('vimorgsources.json')
    catch
        throw 'Error while loading vo DB file: '.v:exception
    endtry
    "▲1
    let snr_to_name={}
    call map(copy(www_vim_org), 'extend(snr_to_name, {v:val.vim_script_nr : v:key})')
    "▶1 Prepare variables for validating file
    let lines=readfile('db/scmsources.vim')
    let topvars=['scm', 'scmnr']
    let alltopvars=copy(topvars)
    let prevempty=0
    let curauthorempty={'type': 0, 'vo': 1, 'hashook': 0, 'mintype': 5, 'hasvo': 0, 'srcnum': 0, 'lsnr': 0}
    let curauthor={}
    let isend=0
    let afterline=0
    let lnr=0
    let psfsnr=0
    let def={'snr': {}, 'name': {}}
    "▶1 Validate lines
    for line in lines
        let lnr+=1
        if empty(line) && empty(topvars)
            let prevempty=1
            continue
        elseif prevempty && !isend
            if line =~# '" \v\S+(\ \S+)*$'
                if empty(curauthor)
                elseif curauthor.hashook
                    throw lnr.':You must unlet the hook variable (with line exactly matching “unlet hook”) before proceeding to new author'
                elseif !curauthor.srcnum
                    if afterline!=3
                        throw lnr.':Authors without any sources must go after the third line'
                    endif
                elseif !curauthor.hasvo
                    if afterline!=2
                        throw lnr.':Authors without any vim.org sources must go after the second line'
                    endif
                elseif curauthor.mintype>0
                    if afterline!=1
                        throw lnr.':Authors without any non-hooked SCM sources must go after the first line'
                    endif
                endif
                let curauthor=deepcopy(curauthorempty)
            elseif line =~# '^"-\+$'
                let afterline+=1
                let curauthor={}
            else
                throw lnr.':It is allowed to only separate author sections with empty lines'
            endif
        elseif line=~#'\v^%(\".*)?$'
            if !empty(curauthor)
                let curauthor.lsnr=0
            endif
        elseif isend
            throw lnr.':Only comment and empty lines are allowed after “let r=…” line: '.line
        elseif line=~#'\v^let\ %('.join(topvars, '|').')\m\s*=\s*{}$'
            let topvar=matchstr(line, '\a\+\ze\s*=')
            call filter(topvars, 'v:val isnot# topvar')
            if empty(topvars)
                let section=1
            endif
        elseif line[:9] is# 'let scmnr.'
            if !curauthor.vo
                throw lnr.':www.vim.org sources must go before scm ones'
            endif
            let match=matchlist(line, '^let scmnr\.\v(\d+)\s+\=\s+(.*)$')
            if empty(match)
                throw lnr.':Invalid scmnr line: '.line
            elseif !has_key(snr_to_name, match[1])
                throw lnr.':Absent snr: '.match[1]
            endif
            if has_key(def.snr, match[1])
                throw lnr.':Duplicating entry for scmnr '.match[1].' (previous on line '.def.snr[match[1]].'): '.line
            endif
            let def.snr[match[1]]=lnr
            let curtype=CheckVal(match[2], lnr.':Invalid scmnr value %s: %s', curauthor)
            if curauthor.type!=curtype
                let curauthor.lsnr=0
            endif
            let snr=+match[1]
            if !curauthor.hasvo && !afterline
                if snr<psfsnr
                    throw lnr.':First script number is lesser then first script number in previous section: '.snr.'<'.psfsnr
                endif
                let psfsnr=snr
            endif
            if snr<curauthor.lsnr
                throw lnr.':Script number is lesser then previous one: '.snr.'<'.curauthor.lsnr
            endif
            let curauthor.lsnr=snr
            unlet snr
            let curauthor.hasvo=1
            let curauthor.vo=1
        elseif line[:6] is# 'let scm'
            let match=matchlist(line, '^let scm\v\[\''([a-zA-Z0-9_\-]+)(\@[a-zA-Z0-9_\-]+|\#[a-zA-Z0-9_\-]+%(\%\d+)?)?\''\]\s+\=\s+(.*)$')
            if empty(match)
                throw lnr.':Invalid scm line: '.line
            endif
            let name=match[1].match[2]
            if has_key(www_vim_org, name)
                throw lnr.':Key '.name.' already present in www_vim_org dictionary'
            endif
            if has_key(def.name, name)
                throw lnr.':Duplicating scm '.name.' (previous on line '.def.name[name].'): '.line
            endif
            let def.name[name]=lnr
            let curtype=CheckVal(match[3], lnr.':Invalid scm value %s: %s', curauthor)
            if curauthor.vo
                let curauthor.vo=0
                let curauthor.type=0
            endif
        elseif line=~#'^let hook'
            let match=matchlist(line, 'hook\s*=\s*\(.*\)$')
            if empty(match)
                throw lnr.':Invalid hook line: '.line
            endif
            try
                let curauthor.hook=eval(match[1])
            catch
                throw lnr.':Invalid hook item: '.match[1]
            endtry
            let curauthor.hashook=1
        elseif line is# 'unlet hook'
            let curauthor.hashook=0
            unlet curauthor.hook
        elseif line is# 'let r=['.join(alltopvars, ', ').']'
            let isend=1
        else
            throw lnr.':Unknown line: '.line
        endif
        if exists('curtype')
            if curauthor.type>curtype
                throw lnr.':Invalid source order: '.g:types[curtype].' should not follow '.g:types[curauthor.type]
            endif
            let curauthor.type=curtype
            if curauthor.mintype>curtype
                let curauthor.mintype=curtype
            endif
            unlet curtype
            let curauthor.srcnum+=1
        endif
        let prevempty=0
    endfor
    "▶1 Check for last line
    if !isend
        throw 'End line not found'
    endif
    "▲1
    let scmsources=vimpi#LoadDBFile('scmsources.vim')
    "▶1 Check whether vimpi#GetSCMSources runs properly (it has some checks on its own)
    redir => g:messages
    call vimpi#GetSCMSources(snr_to_name, www_vim_org)
    redir END
    if match(g:messages, '\S')!=-1
        throw 'Not empty messages: '.g:messages
    endif
    "▶1 Prepare variables for scmsources values checking
    let keys={'addon-info': {}, 'url': '', 'type': '',
                \'archive_name': '', 'script-type': '',
                \'dependencies': {}, 'archive': '',
                \'branch': ''}
    call map(keys, 'type(v:val)')
    let mandatory_keys=['type', 'url']
    let mandatory_archive_keys=['archive_name', 'script-type']
    let numcheckid='id !~# ''\v^[1-9]\d*$'''
    let strcheckid='id =~# ''[^a-zA-Z0-9_\-@#]'''
    let checkid=numcheckid
    let ai_keys={'post-install-hook': '', 'post-update-hook': '',
                \'post-scm-update-hook': '', 'archive_name': '',
                \'runtimepath': '', 'dependencies': {}}
    call map(ai_keys, 'type(v:val)')
    "▶1 Validate scmsources dictionaries
    for [id, src] in items(scmsources[1])+[[0, 1]]+items(scmsources[0])
        if id is 0
            unlet src
            let checkid=strcheckid
            continue
        endif
        let s=id.' '.string(src)
        if type(src)!=type({})
            throw 'Not a dict: '.s
        elseif eval(checkid)
            throw 'Invalid key: '.s
        elseif !empty(filter(copy(mandatory_keys), '!has_key(src, v:val)'))
            throw 'Mandatory key missing: '.s
        elseif !empty(filter(copy(keys), 'has_key(src, v:key) && type(src[v:key])!=v:val'))
            throw 'Type mismatch: '.s
        elseif !empty(filter(keys(src), '!has_key(keys, v:val)'))
            throw 'Unknown key: '.s
        elseif src.type is# 'archive' && !empty(filter(copy(mandatory_archive_keys), '!has_key(src, v:val)'))
            throw 'Mandatory archive key missing: '.s
        elseif src.type is# 'git' && src.url=~#'\v^git\:\/\/github\.com\/.*\.git$|^(git)@!\a+\:\/\/github\.com\/'
            throw 'Invalid github url: '.s
        elseif has_key(src, 'addon-info') && (empty(src['addon-info']) || !empty(filter(copy(ai_keys), 'has_key(src["addon-info"], v:key) && type(src["addon-info"][v:key])!=v:val')))
            throw 'Invalid addon-info: '.s
        endif
        unlet src
    endfor
    "▶1 Prepare variables for patchinfo values checking
    try
        let patchinfo=vimpi#LoadDBFile('patchinfo.vim')
    catch
        throw 'Error while loading patchinfo DB file: '.v:exception.' '.v:throwpoint
    endtry
    let add_keys={'script-type': '', 'target_dir': '', 'strip-components': 0, 'deprecated': ''}
    call map(add_keys, 'type(v:val)')
    let checkid=numcheckid
    "▶1 Check patchinfo values: patch_repo
    for [id, add] in items(patchinfo[0])
        let s=id.' '.string(add)
        if type(add)!=type({})
            throw 'Not a dict: '.s
        elseif eval(checkid)
            throw 'Invalid key: '.s
        elseif empty(add)
            throw 'Empty dict: '.s
        elseif !empty(filter(copy(add_keys), 'has_key(add, v:key) && type(add[v:key])!=v:val'))
            throw 'Type mismatch: '.s
        endif
        unlet add
    endfor
    "▶1 Check patchinfo values: addon_info
    for [id, mai] in items(patchinfo[1])
        let s=id.' '.string(mai)
        if type(mai)!=type({})
            throw 'Not a dict: '.s
        elseif eval(numcheckid)
            throw 'Invalid key: '.s
        elseif empty(mai)
            throw 'Empty dict: '.s
        elseif !empty(filter(copy(ai_keys), 'has_key(mai, v:key) && type(mai[v:key])!=v:val'))
            throw 'Type mismatch: '.s
        endif
        unlet mai
    endfor
    "▶1 Check patchinfo values: addon_info_deps
    for [id, msd] in items(patchinfo[2])
        let s=id.' '.string(msd)
        if type(msd)!=type([])
            throw 'Not a list: '.s
        elseif eval(numcheckid)
            throw 'Invalid key: '.s
        elseif !empty(filter(copy(msd), 'type(v:val)>1 || (type(v:val)==type(0) ? (v:val<=0) : (eval(substitute(strcheckid, "id", "v:val", ""))))'))
            throw 'Invalid value '.s
        endif
    endfor
    "▶1 Check renamings: renamings
    for [old_name, new_name] in items(patchinfo[3])
        if type(new_name) != type('')
            throw 'Not a string: '.new_name
        elseif !has_key(www_vim_org, old_name)
            throw 'Unknown plugin: '.old_name
        elseif old_name is# new_name
            throw 'Old name is equal to new one: '.old_name
        endif
    endfor
    "▶1 Prepare variables for patchinfo lines validating
    let lines=readfile('db/patchinfo.vim')
    let topvars=['patch_repo', 'addon_info', 'addon_info_deps', 'renamings']
    let alltopvars=copy(topvars)
    let prevempty=0
    let lnr=0
    " 0: topvars
    " 1: hooks
    " 2: wrong archive names
    " 3: type corrections
    " 4: target directories
    " 5: deprecations
    " 6: Missing dependencies
    " 7: Missing runtimepaths
    " 8: Renamings
    let stage=0
    let vardefined=0
    let dictdefined=0
    let snr=0
    let snrs={'as': {}, 'an': {}, 'ms': {}, 'msd': {}}
    let rns={'to': {}, 'from': {}}
    for line in lines
        let lnr+=1
        if empty(line) && empty(topvars)
            let prevempty=1
            continue
        elseif line[0:1] is# '" '
            continue
        elseif line[0:4] is# '"'."\u25b6".'1'
            if !empty(vardefined)
                throw lnr.':Must undefine all variables before starting new section'
            endif
            let stage+=1
            let prevempty=1
            continue
        elseif stage==0
            if empty(topvars) || line isnot# 'let '.topvars[0].' = {}'
                throw lnr.':Invalid top variable line: '.line
            endif
            call remove(topvars, 0)
        elseif stage==1
            if !empty(topvars)
                throw lnr.':Not all top variables were defined'
            endif
            if line[:10] is# 'let hook = '
                if !prevempty
                    throw lnr.':Hook section must be preceded with an empty line'
                elseif vardefined
                    throw lnr.':You must undefine previous hook before defining a new one'
                endif
                let hook=eval(line[11:])
                if type(hook)!=type('')
                    throw lnr.':Hook is not a string: '.line
                endif
                let vardefined=1
            elseif line =~# '^let addon_info\.\d\+ = \S'
                if !vardefined
                    throw lnr.':You must define hook before doing anything else'
                elseif dictdefined
                    throw lnr.':addon_info dictionary was already defined'
                endif
                let value=eval(line[stridx(line, '=')+2:])
                if type(value)!=type({})
                    throw lnr.':Invalid addon_info value: '.line
                elseif !empty(value)
                    throw lnr.':Everything must be defined afterwards: '.line
                endif
                let snr=str2nr(line[15:])
                if has_key(snrs.ms, snr)
                    throw lnr.':Duplicating addon_info entry for '.snr.' (previous on line '.snrs.ms[snr].'): '.line
                endif
                let snrs.ms[snr]=lnr
                let dictdefined=1
            elseif line =~# '\v^let\ addon_info\.'.snr.'\[\''(post\-(install|(scms\-)?update)|pre\-update)\-hook\'']\s+\=\ hook$'
                if !dictdefined
                    throw lnr.':Invalid hook definition: '.line
                endif
            elseif line is# 'unlet hook'
                let vardefined=0
                let dictdefined=0
            else
                throw lnr.':Invalid line in hooks section: '.line
            endif
        elseif stage==2
            if line=~#'^let addon_info\.\d\+='
                let value=eval(line[stridx(line, '=')+1:])
                if type(value)!=type({})
                    throw lnr.':Invalid addon_info value: '.line
                elseif !has_key(value, 'archive_name')
                    throw lnr.':Wrong section (no archive_name key): '.line
                elseif type(value.archive_name)!=type('')
                    throw lnr.':Wrong archive_name type: '.line
                endif
                let snr=str2nr(line[15:])
                if has_key(snrs.ms, snr)
                    throw lnr.':Duplicating addon_info entry for '.snr.' (previous on line '.snrs.ms[snr].'): '.line
                endif
                let snrs.ms[snr]=lnr
            else
                throw lnr.':Invalid line in archive names section: '.line
            endif
        elseif stage==3 || stage==4
            if line=~#'^let patch_repo\.\d\+='
                        \|| line=~#'\V\^call extend(patch_repo.\d\+, '
                if line[0] is# 'l'
                    let value=eval(line[stridx(line, '=')+1:])
                else
                    let value=eval(line[stridx(line, ',')+2:-2])
                endif
                if stage==3
                    if keys(value) !=# ['script-type']
                        throw lnr.':No script-type key or something besides it: '.line
                    endif
                elseif stage==4
                    if keys(value) !=# ['target_dir'] && keys(value) !=# ['strip-components']
                        throw lnr.':No target_dir or strip-components key or something besides them: '.line
                    endif
                endif
                let snr=str2nr(line[15:])
                if has_key(snrs.as, snr)
                    throw lnr.':Duplicating patch_repo entry for '.snr.' (previous on line '.snrs.as[snr].'): '.line
                endif
                let snrs.as[snr]=lnr
            else
                if stage==3
                    throw lnr.':Invalid line in type corrections section: '.line
                elseif stage==4
                    throw lnr.':Invalid line in target directories section: '.line
                endif
            endif
        elseif stage==5
            if line=~#'^let patch_repo\.\v([^=]{4})@=\d+\s*\V={''deprecated'': "\v(\\.|[^\\"])+\V"}\$'
                let snr=str2nr(line[15:])
                if has_key(snrs.as, snr)
                    throw lnr.':Duplicating patch_repo entry for '.snr.' (previous on line '.snrs.as[snr].'): '.line
                endif
                let snrs.as[snr]=lnr
            elseif line=~#'\V\^call extend(patch_repo.\d\+, {''deprecated'': "\v(\\.|[^\\"])+\V"})\$'
                continue
            else
                throw lnr.':Invalid line in deprecations section: '.line
            endif
        elseif stage==6
            if line=~#'^let addon_info_deps\.\v([^=]{5})@=\d+\s+\V= [\v(\d+|\''[^'']+\'')(\,\ (\d+|\''[^'']+\''))*\V]\$'
                let types=map(eval(line[27:]), 'type(v:val)')
                for i in range(0, len(types)-2)
                    if types[i]==type('') && types[i+1]==type(0)
                        throw lnr.':Numbers must go before strings in dependencies list: '.line
                    endif
                endfor
                let snr=str2nr(line[20:])
                if has_key(snrs.msd, snr)
                    throw lnr.':Duplicating addon_info_deps entry for '.snr.' (previous on line '.snrs.msd[snr].'): '.line
                endif
                let snrs.msd[snr]=lnr
            else
                throw lnr.':Invalid line in missing dependencies section: '.line
            endif
        elseif stage==7
            if line=~#'^let addon_info\.\v([^=]{5})@=\d+\s+\V= {''runtimepath'': ''\[^'']\+''}\$'
                let snr=str2nr(line[15:])
                if has_key(snrs.ms, snr)
                    throw lnr.':Duplicating addon_info entry for '.snr.' (previous on line '.snrs.ms[snr].'): '.line
                endif
                let snrs.ms[snr]=lnr
            elseif line=~#'\V\^call extend(addon_info.\d\+, {''runtimepath'': ''\[^'']\+''})\$'
                continue
            else
                throw lnr.':Invalid line in runtimepath information section: '.line
            endif
        elseif stage==8
            if line is# 'let r=['.join(alltopvars, ', ').']'
                let stage+=1
                continue
            endif
            if line=~#'^let renamings\[''\(''''\|[^'']\)\+''\] = ''\(''''\|[^'']\)\+''$'
                let [old_name, new_name]=matchlist(line, '\[\(''\%(''''\|[^'']\)\+''\)\] = \(''\%(''''\|[^'']\)\+''\)')[1:2]
                if has_key(rns.from, new_name)
                    throw lnr.':Renaming two plugins to the same name, previous on line '.rns.from[new_name].': '.line
                elseif has_key(rns.to, old_name)
                    throw lnr.':Trying to rename one plugin to two different names, previous on line '.rns.to[old_name].': '.line
                endif
                let rns.from[new_name]=lnr
                let rns.to[old_name]=lnr
            endif
        endif
        let prevempty=0
    endfor
    "▲1
catch
    call writefile([v:exception, v:throwpoint], 'exception.fail', 'b')
    let exception=1
endtry
"▶1 Check whether vam_known_repositories#Pool works
try
    " XXX For unknown reason putting the following line into previous :try block 
    "     prevents catching exceptions
    call vam_known_repositories#Pool()
    if !exists('exception')
        qall!
    endif
catch
    call writefile([v:exception, v:throwpoint], 'exception.fail', 'b')
endtry
"▲1
redir! > messages.fail
silent messages
redir END
cquit
" vim: tw=0 ts=4 sts=4 sw=4 et fmr=▶,▲
