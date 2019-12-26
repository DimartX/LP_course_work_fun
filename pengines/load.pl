% Main file to load the pengines demo.  This file is included from
%
%   - debug.pl for local debugging
%   - daemon.pl to run pengines as a Unix service

:- use_module(library(pengines)).
:- use_module(library(http/http_error)).
:- use_module(server).
:- use_module(storage).

:- if(exists_source(apps(scratchpad/app))).
:- use_module(apps(scratchpad/app)).
:- endif.

