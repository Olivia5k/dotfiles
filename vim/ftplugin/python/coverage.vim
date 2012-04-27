" File: py-coverage-highlight.vim
" Author: Marius Gedminas <marius@gedmin.as>
" Version: 0.4
" Last Modified: 2010-01-09
"
" Overview
" --------
" Vim script to higlight coverage report results.
"
" Installation
" ------------
" Copy this file to $HOME/.vim/plugin directory
"
" Usage with Ned Batchelder's coverage.py
" ---------------------------------------
" Produce a coverage report with coverage (it's assumed that you use
" zc.buildout and the coverage script is ./bin/coverage relative to the
" current working directory).  Open a source file.  Use :HiglightCoverage
" to load coverage info, and :HiglightCoverageOff to turn it off.
"
" Usage with Python's trace.py
" ----------------------------
" Produce a coverage report with Python's trace.py.  Open a source file.
" Load the highlighting with :HiglightCoverage filename/to/coverage.report
" Turn off the highlighting with :HiglightCoverageOff or :sign unplace *

if !has("python")
    finish
endif

hi link NoCoverageSign Error
hi link NoCoverage CursorColumn
sign define NoCoverage text=!! texthl=NoCoverageSign linehl=NoCoverage

if !exists("g:coverage_enabled")
    let g:coverage_enabled = 0
endif

function! CoverageToggle()
    if !g:coverage_enabled
        HiglightCoverage
        let g:coverage_enabled = 1
    else
        HiglightCoverageOff
        let g:coverage_enabled = 0
    endif
endfunction

nmap ,C :call CoverageToggle()<cr>
nmap ,nc A  # pragma: no cover

function! HiglightCoverage(filename)
    sign unplace *
    python <<END

import vim, os, subprocess

def filename2module(filename):
    pkg = os.path.splitext(os.path.abspath(filename))[0]
    root = os.path.dirname(pkg)
    while os.path.exists(os.path.join(root, '__init__.py')):
        new_root = os.path.dirname(root)
        if new_root == root:
            break # prevent infinite loops in crazy systems
        else:
            root = new_root
    pkg = pkg[len(root) + len(os.path.sep):].replace('/', '.')
    return pkg


def find_coverage_report(modulename):
    filename = 'coverage/%s.cover' % modulename
    root = os.path.abspath(os.path.curdir)
    while not os.path.exists(os.path.join(root, filename)):
        new_root = os.path.dirname(root)
        if new_root == root:
            break # prevent infinite loops in crazy systems
        else:
            root = new_root
    return os.path.join(root, filename)


class Signs(object):

    def __init__(self):
        self.signid = 0
        self.bufferid = vim.eval('bufnr("%")')

    def place(self, lineno):
        self.signid += 1
        cmd = "sign place %d line=%d name=NoCoverage buffer=%s" % (self.signid, lineno, self.bufferid)
        vim.command(cmd)

def parse_cover_file(filename):
    lineno = 0
    signs = Signs()
    for line in file(filename):
        lineno += 1
        if line.startswith('>>>>>>'):
            signs.place(lineno)


def parse_coverage_output(output, filename):
    # Example output:
    # Name                          Stmts   Exec  Cover   Missing
    # -----------------------------------------------------------
    # src/foo/bar/baz/qq/__init__     146    136    93%   170-177, 180-184
    last_line = output.splitlines()[-1]
    filename = os.path.relpath(filename)
    filename_no_ext = os.path.splitext(filename)[0]
    signs = Signs()
    if last_line.startswith(filename_no_ext + ' '):
        print last_line
        last_line = last_line[len(filename_no_ext) + 1:].lstrip()
        columns = last_line.split(None, 3)
        if len(columns) > 3:
            parse_lines(columns[3], signs)
    else:
        print "Got confused by %s" % repr(last_line)
        print "Expected it to start with %s" % repr(filename_no_ext + ' ')
        print "Full output:"
        print output


def parse_lines(formatted_list, signs):
    for item in formatted_list.split(', '):
        if '-' in item:
            lo, hi = item.split('-')
        else:
            lo = hi = item
        lo, hi = int(lo), int(hi)
        for lineno in range(lo, hi+1):
            signs.place(lineno)


def program_in_path(program):
    path = os.environ.get("PATH", os.defpath).split(os.pathsep)
    path = [os.path.join(dir, program) for dir in path]
    path = [True for file in path if os.path.isfile(file)]
    return bool(path)


def find_coverage_script():
    if os.path.exists('bin/coverage'):
        return 'bin/coverage'
    if program_in_path('coverage'):
        # assume it was easy_installed
        return 'coverage'


filename = vim.eval('a:filename')
if filename:
      parse_cover_file(filename)
else:
    filename = vim.eval('bufname("%")')
    coverage_script = find_coverage_script()
    if os.path.exists('.coverage') and coverage_script:
        print "Running %s -rm %s" % (coverage_script, filename)
        output = subprocess.Popen([coverage_script, '-rm', filename],
                                  stdout=subprocess.PIPE).communicate()[0]
        parse_coverage_output(output, filename)
    else:
        modulename = filename2module(filename)
        filename = find_coverage_report(modulename)
        print "Using", filename
        parse_cover_file(filename)

END
endf

command! -nargs=? -complete=file -bar HiglightCoverage  call HiglightCoverage(<q-args>)
command!                              HiglightCoverageOff   sign unplace *
