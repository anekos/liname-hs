" Vim syntax file
" Language:     LiName
" Maintainer:   anekos
" vim: ts=2 sw=2 et

if exists("b:current_syntax")
  finish
endif

syntax clear

syntax match   linameComment            ".*"
syntax match   linameCommand            "^[=!%]\+" skipwhite nextgroup=linameNumber
syntax match   linameNumber             "\d\+\t" skipwhite nextgroup=linameFile,linameFolder
syntax match   linameFile               ".*" contained
syntax match   linameFolder             ".*\\$" contained

highlight link linameComment            Comment
highlight link linameNumber             Number
highlight link linameCommand            Keyword

