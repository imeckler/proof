" Vim syntax file
" Language: Structured Proof Markup Language
" Maintainer: Izaak Meckler

if exists("b:current_syntax")
  finish
end

" proof is case sensitive
syn case match

" Keywords
syn keyword proofKeywords theorem definition claim cases let take assume prove macros comment because suppose then
syn keyword proofListitem case

" Matches
syn match proofKeywords /such that/
syn match	proofLineComment +^[ \t:]*%.*$+

syn include @LATEX syntax/tex.vim
syn region proofTexBlock start="\v\[\|" end="\v\|\]" keepend contains=@LATEX
" syn region proofTexBlock start="begintex" end="endtex" keepend contains=@LATEX

let b:current_syntax = "proof"

hi def link proofKeywords Constant
hi def link proofListitem Statement
hi def link proofLineComment Comment

