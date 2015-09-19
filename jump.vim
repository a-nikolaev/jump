" Vim syntax file
" Language: Jump
" Maintainer: X
" Latest Revision: 2015-09-18

if exists("b:current_syntax")
  finish
endif


syn keyword jumpKeyword let in end where fun 
syn match   jumpKeywordOp "\%(>>\|=\|->\||\)"
syn match   jumpDelim   display "\%(,\)"

syn match   jumpOp "\%(+\|-\|<\|>\|<=\|>=\|==\)"
syn keyword jumpBool true false

syn match jumpInt '\d\+'

syn match jumpIdent "[a-z][a-z_0-9]*"

syn match jumpLabel "[A-Z][a-z_0-9]*"

hi def link jumpKeyword    Keyword
hi def link jumpKeywordOp  Operator
hi def link jumpDelim      Operator
hi def link jumpOp         Operator
hi def link jumpBool       Boolean

hi def link jumpInt        Number

hi def link jumpLabel      Special
