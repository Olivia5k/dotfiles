" Control
nmap <C-J> <esc>/<++><cr>4s
imap <C-J> <esc>/<++><cr>4s

" Standard
nmap ghbr A<br /><esc>
nmap ghhr o<hr /><esc>
nmap ghpp o<p><return></p><esc>k^<esc>
nmap ghpc o<p class=""<esc>mha><return></p><esc>`hi
nmap ghpp o<p><return></p><esc>O<tab>
nmap ghma o<area shape=""<esc>mha coords="<++>" /><esc>`hi

" Anchors
nmap ghaa o<a href=""<esc>mha></a><esc>`hi
nmap ghac o<a class=""<esc>mha href="<++>"></a><esc>`hi
nmap ghai o<a id=""<esc>mha href="<++>"></a><esc>`hi
nmap ghaI o<a id=""<esc>mha class="<++>" href="<++>"></a><esc>`hi

" Doctype
nmap ghDs ggO<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd"><return><esc>k
nmap ghDt ggO<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd"><return><esc>k
nmap ghDf ggO<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Frameset//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-frameset.dtd"><return><esc>k
nmap ghD1 ggO<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.1//EN" "http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd"><return><esc>k

" Comments
nmap ghcc o<!--  --><esc>3hi
nmap ghcs I<!-- <esc>^<esc>
nmap ghce A --><esc>$<esc>

" Divs
nmap ghdd o<div><return></div><esc>k$
nmap ghdc o<div class=""<esc>mha><return></div><esc>`hi
nmap ghdi o<div id=""<esc>mha><return></div><esc>`hi
nmap ghdI o<div id=""<esc>mha class="<++>"><return></div><esc>`hi
nmap ghdss o<div><esc>$
nmap ghdsc o<div class=""<esc>mha><esc>`hi
nmap ghdsi o<div id=""<esc>mha><esc>`hi
nmap ghdsI o<div id=""<esc>mha class="<++>"><esc>`hi
nmap ghde o</div><esc>$

" Headers
nmap gh11 o<h1><return></h1><esc>O
nmap gh1c o<h1 class=""<esc>mha><return></h1><esc>`hi
nmap gh1i o<h1 id=""<esc>mha><return></h1><esc>`hi
nmap gh1I o<h1 id=""<esc>mha class="<++>"><return></h1><esc>`hi
nmap gh22 o<h2><return></h2><esc>O
nmap gh2c o<h2 class=""<esc>mha><return></h2><esc>`hi
nmap gh2i o<h2 id=""<esc>mha><return></h2><esc>`hi
nmap gh2I o<h2 id=""<esc>mha class="<++>"><return></h2><esc>`hi
nmap gh33 o<h3><return></h3><esc>O
nmap gh3c o<h3 class=""<esc>mha><return></h3><esc>`hi
nmap gh3i o<h3 id=""<esc>mha><return></h3><esc>`hi
nmap gh3I o<h3 id=""<esc>mha class="<++>"><return></h3><esc>`hi

" Images
nmap ghimm o<img src=""<esc>mha alt="<++>" /><esc>`hi
nmap ghimc o<img class=""<esc>mha src="<++>" alt="<++>" /><esc>`hi
nmap ghimi o<img id=""<esc>mha src="<++>" alt="<++>" /><esc>`hi
nmap ghimI o<img id=""<esc>mha class="<++>" src="<++>" alt="<++>" /><esc>`hi

" Inputs
nmap ghinn o<input type=""<esc>mha name="<++>" /><esc>`hi
nmap ghinc o<input class=""<esc>mha type="<++>" name="<++>" /><esc>`hi
nmap ghini o<input id=""<esc>mha type="<++>" name="<++>" /><esc>`hi
nmap ghinI o<input id=""<esc>mha class="<++>" type="<++>" name="<++>" /><esc>`hi

" Linewise
nmap ghSc I<span class=""<esc>mha><esc>A</span><esc>`hi
nmap ghSi I<span id=""<esc>mha><esc>A</span><esc>`hi
nmap ghSs I<span><esc>A</span><esc>^<esc>
nmap ghA I<a href=""<esc>mha><esc>A</a><esc>`hi
nmap ghI I<i><esc>A</i><esc>^<esc>
nmap ghB I<b><esc>A</b><esc>^<esc>
nmap ghP I<p><esc>A</p><esc>^<esc>
nmap ghU I<u><esc>A</u><esc>^<esc>
nmap ghC I<!-- <esc>A --><esc>^<esc>

" Links
nmap ghlc o<link type="text/css" rel="stylesheet" href=".css"><esc>5hi
nmap ghlc o<link type="text/javascript" rel="" href=".js"><esc>5hi

" Lists
nmap ghluu o<ul><return></ul><esc>k^<esc>
nmap ghluc o<ul class=""<esc>mha><return></ul><esc>`hi
nmap ghlui o<ul id=""<esc>mha><return></ul><esc>`hi
nmap ghluI o<ul id=""<esc>mha class="<++>"><return></ul><esc>`hi
nmap ghloo o<ol><return></ol><esc>k^<esc>
nmap ghloc o<ol class=""<esc>mha><return></ol><esc>`hi
nmap ghloi o<ol id=""<esc>mha><return></ol><esc>`hi
nmap ghloI o<ol id=""<esc>mha class="<++>"><return></ol><esc>`hi
nmap ghli  o<li><return></li><esc>k^<esc>
nmap ghlic o<li class=""<esc>mha><return></li><esc>`hi
nmap ghlii o<li id=""<esc>mha><return></li><esc>`hi
nmap ghliI o<li id=""<esc>mha class="<++>"><return></li><esc>`hi

" Script
nmap ghsc o<script><return></script><esc>O

" Select
nmap ghsee o<select><return></select>
nmap ghsec o<select class=""<esc>mha><return></select><esc>`hi
nmap ghsei o<select id=""<esc>mha><return></select><esc>`hi
nmap ghseI o<select id=""<esc>mha class="<++>"><return></select><esc>`hi
nmap ghoo o<option></option><esc>8hi
nmap ghoc o<option class=""<esc>mha></option><esc>`hi
nmap ghoi o<option id=""<esc>mha></option><esc>`hi
nmap ghoI o<option id=""<esc>mha class="<++>"></option><esc>`hi

" Span
nmap ghspp o<span></span><esc>6hi
nmap ghspc o<span class=""></span><esc>8hi
nmap ghspi o<span id=""></span><esc>8hi
nmap ghspI o<span id=""<esc>mha class="<++>"></span><esc>`hi

" Tables
nmap ghtt o<table><return></table><esc>k^<esc>
nmap ghtc o<table class=""<esc>mha><return></table><esc>`hi
nmap ghti o<table id=""<esc>mha><return></table><esc>`hi
nmap ghtI o<table id=""<esc>mha class="<++>"><return></table><esc>`hi
nmap ghtdd o<td><return></td><esc>k$
nmap ghtdc o<td class=""<esc>mha><return></td><esc>`hi
nmap ghtdi o<td id=""<esc>mha><return></td><esc>`hi
nmap ghtdI o<td id=""<esc>mha class="<++>"><return></td><esc>`hi
nmap ghtrr o<tr><return></tr><esc>k
nmap ghtrc o<tr class=""<esc>mha><return></tr><esc>`hi
nmap ghtrI o<tr id=""<esc>mha class="<++>"><return></tr><esc>`hi

" Textareas
nmap ghtaa o<textarea cols=""<esc>mha rows="<++>" name="<++>"></textarea><esc>`hi
nmap ghtac o<textarea class=""<esc>mha cols="<++>" rows="<++>" name="<++>"></textarea><esc>`hi
nmap ghtai o<textarea id=""<esc>mha cols="<++>" rows="<++>" name="<++>"></textarea><esc>`hi
nmap ghtaI o<textarea id=""<esc>mha class="<++>" cols="<++>" rows="<++>" name="<++>"></textarea><esc>`hi

" Visual
vmap ghaa "hdi<a href=""<esc>mha><C-R>h</a><esc>`hi
vmap ghac "hdi<a href=""<esc>mha class="<++>"><C-R>h</a><esc>`hi
vmap ghb "hdi<b><C-R>h</b><esc>l
vmap ghi "hdi<i><C-R>h</i><esc>l
vmap ghp "hdi<p><C-R>h</p><esc>l
vmap ghss "hdi<span><C-R>h</span><esc>l
vmap ghsc "hdi<span class=""<esc>mha><C-R>h</span><esc>`hi
vmap ghsi "hdi<span id=""<esc>mha><C-R>h</span><esc>`hi
vmap ghsI "hdi<span id=""<esc>mha class="<++>"><C-R>h</span><esc>`hi
vmap ghu "hdi<u><C-R>h</u><esc>l
vmap ghc "hdi<!-- <C-R>h --><esc>l

" CSS
nmap gcc O/*  */<esc>2hi

" Custom
nmap ghdC o<div class="clear"></div><esc>^<esc>
nmap ghdm o</div><return><div class="main container"><esc>^<esc>
nmap ,h :source ~/.vim/plugin/html.vim<cr>
nmap ,H :sp ~/.vim/plugin/html.vim<cr>
nmap gxc o<x><return></x><esc>k:.,+1s/x/
nmap gxe o< /><esc>2hi
