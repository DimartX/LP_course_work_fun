:- module(app_scratchpad, []).
:- use_module(library(pengines)).

:- pengine_application(scratchpad).
:- use_module(scratchpad:scratchpad).
:- use_module(scratchpad:smith).

:- use_module(pengine_sandbox:scratchpad).
:- use_module(library(sandbox)).

:- multifile sandbox:safe_primitive/1.

sandbox:safe_primitive(scratchpad:ask_question(_, _)).
