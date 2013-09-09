" Vim syntax file
" Language:     LiName
" Maintainer:   anekos
" vim: ts=2 sw=2 et

if exists("b:current_syntax")
  finish
endif

syntax clear

syntax match   linameInvalid            ".*"
syntax match   linameComment            "#.*"
syntax match   linameLine               "^[!=%]*[0-9]\+\t.\+$" contains=linameCommand,linameKey,linamePath
syntax match   linameCommand            "^[!=%]\+" nextgroup=linameKey
syntax match   linameKey                "\d\+\t" nextgroup=linamePath
syntax match   linamePath               ".*$" contained

highlight link linameComment            Comment
highlight link linameKey                Label
highlight link linameCommand            Type
highlight link linameInvalid            Error

