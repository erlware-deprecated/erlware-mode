;; erlang.el --- Major modes for editing and running Erlang
;; The contents of this file are subject to the Erlang Public License,
;; Version 1.1, (the "License"); you may not use this file except in
;; compliance with the License. You should have received a copy of the
;; Erlang Public License along with this software. If not, it can be
;; retrieved via the world wide web at http://www.erlang.org/.

;; Software distributed under the License is distributed on an "AS IS"
;; basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
;; the License for the specific language governing rights and limitations
;; under the License.

;; The Initial Developer of the Original Code is Ericsson Utvecklings AB.
;; All Rights Reserved.



(defvar erlang-skel
  '(("If"            "if"            erlang-skel-if)
    ("Case"          "case"          erlang-skel-case)
    ("Receive"       "receive"       erlang-skel-receive)
    ("Receive After" "after"         erlang-skel-receive-after)
    ("Receive Loop"  "loop"          erlang-skel-receive-loop)
    ("Module"        "module"        erlang-skel-module)
    ("Author"        "author"        erlang-skel-author)
    ("Function"      "function"      erlang-skel-function)
    ()
    ("Small Header"  "small-header"
     erlang-skel-small-header erlang-skel-header)
    ("Normal Header" "normal-header"
     erlang-skel-normal-header erlang-skel-header)
    ("Large Header"  "large-header"
     erlang-skel-large-header erlang-skel-header)
    ()
    ("Small Server"   "small-server"
     erlang-skel-small-server erlang-skel-header)
    ()
    ("Application" "application"
     erlang-skel-application erlang-skel-header)
    ("Supervisor" "supervisor"
     erlang-skel-supervisor erlang-skel-header)
    ("supervisor_bridge" "supervisor-bridge"
     erlang-skel-supervisor-bridge erlang-skel-header)
    ("gen_server" "generic-server"
     erlang-skel-generic-server erlang-skel-header)
    ("gen_event" "gen-event"
     erlang-skel-gen-event erlang-skel-header)
    ("gen_fsm" "gen-fsm"
     erlang-skel-gen-fsm erlang-skel-header)
    ("Library module" "gen-lib"
     erlang-skel-lib erlang-skel-header)
    ("Corba callback" "gen-corba-cb"
     erlang-skel-corba-callback erlang-skel-header)
    ("Erlang test suite TS frontend" "ts-test-suite"
     erlang-skel-ts-test-suite erlang-skel-header)
    ("Erlang test suite CT frontend" "ct-test-suite"
     erlang-skel-ct-test-suite erlang-skel-header)
  )
  "*Description of all skeleton templates.
Both functions and menu entries will be created.

Each entry in `erlang-skel' should be a list with three or four
elements, or the empty list.

The first element is the name which shows up in the menu.  The second
is the `tempo' identifier (The string \"erlang-\" will be added in
front of it).  The third is the skeleton descriptor, a variable
containing `tempo' attributes as described in the function
`tempo-define-template'.  The optional fourth elements denotes a
function which should be called when the menu is selected.

Functions corresponding to every template will be created.  The name
of the function will be `tempo-template-erlang-X' where `X' is the
tempo identifier as specified in the second argument of the elements
in this list.

A list with zero elements means that the a horizontal line should
be placed in the menu.")

;; In XEmacs `user-mail-address' returns "x@y.z (Foo Bar)" ARGH!
;; What's wrong with that? RFC 822 says it's legal.   [sverkerw]
;; This needs to use the customized value.  If that's not sane, things like
;; add-log will lose anyhow.  Avoid it if there _is_ a paren.
(defvar erlang-skel-mail-address
  (if (or (not user-mail-address) (string-match "(" user-mail-address))
      (concat (user-login-name) "@"
	      (or (and (boundp 'mail-host-address)
		       mail-host-address)
		  (system-name)))
    user-mail-address)
  "Mail address of the user.")

;; Expression templates:
(defvar erlang-skel-case
  '((erlang-skel-skip-blank) o >
    "case " p " of" n> p "_ ->" n> p "ok" n> "end" p)
  "*The skeleton of a `case' expression.
Please see the function `tempo-define-template'.")

(defvar erlang-skel-if
  '((erlang-skel-skip-blank) o >
    "if"  n> p " ->" n> p "ok" n> "end" p)
  "The skeleton of an `if' expression.
Please see the function `tempo-define-template'.")

(defvar erlang-skel-receive
  '((erlang-skel-skip-blank) o >
    "receive" n> p "_ ->" n> p "ok" n> "end" p)
  "*The skeleton of a `receive' expression.
Please see the function `tempo-define-template'.")

(defvar erlang-skel-receive-after
  '((erlang-skel-skip-blank) o >
    "receive" n> p "_ ->" n> p "ok" n> "after " p "T ->" n>
    p "ok" n> "end" p)
  "*The skeleton of a `receive' expression with an `after' clause.
Please see the function `tempo-define-template'.")

(defvar erlang-skel-receive-loop
  '(& o "loop(" p ") ->" n> "receive" n> p "_ ->" n>
      "loop(" p ")" n> "end.")
  "*The skeleton of a simple `receive' loop.
Please see the function `tempo-define-template'.")


(defvar erlang-skel-function
  '((erlang-skel-separator 2)
    "%% @spec " n
    "%% " n
    "%% @doc " n
    "%% " n
    "%% @end" n
    (erlang-skel-separator 2))
    "*The template of a function skeleton.
Please see the function `tempo-define-template'.")


;; Attribute templates

(defvar erlang-skel-module
  '(& "-module("
      (erlang-add-quotes-if-needed (erlang-get-module-from-file-name))
      ")." n)
  "*The skeleton of a `module' attribute.
Please see the function `tempo-define-template'.")

(defvar erlang-skel-author
  '(& "-author('" erlang-skel-mail-address "')." n)
  "*The skeleton of a `author' attribute.
Please see the function `tempo-define-template'.")

(defvar erlang-skel-vc nil
  "*The skeleton template to generate a version control attribute.
The default is to insert nothing.  Example of usage:

    (setq erlang-skel-vc '(& \"-rcs(\\\"$\Id: $ \\\").\") n)

Please see the function `tempo-define-template'.")

(defvar erlang-skel-export
  '(& "-export([" n> "])." n)
  "*The skeleton of an `export' attribute.
Please see the function `tempo-define-template'.")

(defvar erlang-skel-import
  '(& "%%-import(Module, [Function/Arity, ...])." n)
  "*The skeleton of an `import' attribute.
Please see the function `tempo-define-template'.")

(defvar erlang-skel-compile nil
  ;;  '(& "%%-compile(export_all)." n)
  "*The skeleton of a `compile' attribute.
Please see the function `tempo-define-template'.")


;; Comment templates.

(defvar erlang-skel-date-function 'erlang-skel-dd-mmm-yyyy
  "*Function which returns date string.
Look in the module `time-stamp' for a battery of functions.")

(defvar erlang-skel-copyright-comment 
  (if (boundp '*copyright-organization*)
      '(& "%%% @copyright (C) " (format-time-string "%Y") ", " 
          *copyright-organization*  n)
      '(& "%%% @copyright (C) " (format-time-string "%Y") ", " 
          (user-full-name)  n))
  "*The template for a copyright line in the header, normally empty.
This variable should be bound to a `tempo' template, for example:
  '(& \"%%% Copyright (C) 2000, Yoyodyne, Inc.\" n)
Please see the function `tempo-define-template'.")

(defvar erlang-skel-created-comment
  '(& "%%% Created : " (funcall erlang-skel-date-function) " by "
      (user-full-name) " <" erlang-skel-mail-address ">" n)
  "*The template for the \"Created:\" comment line.")

(defvar erlang-skel-author-comment
  '(& "%%% @author " (user-full-name) " <" erlang-skel-mail-address ">" n)
  "*The template for creating the \"Author:\" line in the header.
Please see the function `tempo-define-template'.")

(defvar erlang-skel-small-header
  '(o (erlang-skel-include erlang-skel-module)
      ;;                           erlang-skel-author)
      n
      (erlang-skel-include erlang-skel-compile
			   ;;			   erlang-skel-export
			   erlang-skel-vc))
  "*The template of a small header without any comments.
Please see the function `tempo-define-template'.")

(defvar erlang-skel-normal-header
  '(o (erlang-skel-include  erlang-skel-author-comment)
      "%%% @doc "  n
      "%%% " p n
      "%%% @end " n
      (erlang-skel-include erlang-skel-copyright-comment) 
      (erlang-skel-include erlang-skel-created-comment) n
      (erlang-skel-include erlang-skel-small-header) n)
  "*The template of a normal header.
Please see the function `tempo-define-template'.")

(defvar erlang-skel-large-header
  '(o (erlang-skel-separator)
      (erlang-skel-include erlang-skel-author-comment)
      "%%% @doc "  n 
      "%%% " p n
      "%%% @end" n  
      (erlang-skel-include erlang-skel-copyright-comment) 
      (erlang-skel-include erlang-skel-created-comment)
      (erlang-skel-separator) 
      (erlang-skel-include erlang-skel-small-header) )
  "*The template of a large header.
Please see the function `tempo-define-template'.")

 
 ;; Server templates.
(defvar erlang-skel-small-server
  '((erlang-skel-include erlang-skel-large-header)
    "-export([start/0,init/1])." n n n
    "start() ->" n> "spawn(" (erlang-get-module-from-file-name)
    ", init, [self()])." n n
    "init(From) ->" n>
    "loop(From)." n n
    "loop(From) ->" n>
    "receive" n>
    p "_ ->" n>
    "loop(From)" n>
    "end."
    )
  "*Template of a small server.
Please see the function `tempo-define-template'.")

;; Behaviour templates.
(defvar erlang-skel-application
  '((erlang-skel-include erlang-skel-large-header)
    "-behaviour(application)." n n
    "%% Application callbacks" n
    "-export([start/2, stop/1])." n n
    (erlang-skel-double-separator 2)
    "%% Application callbacks" n
    (erlang-skel-double-separator 2)
    (erlang-skel-separator 2)
    "%% @spec start(Type, StartArgs) -> {ok, Pid} |" n
    "%%                                     {ok, Pid, State} |" n
    "%%                                     {error, Reason}." n
    "%% " n
    "%% @doc " n
    "%% This function is called whenever an application " n
    "%% is started using application:start/1,2, and should start the processes" n
    "%% of the application. If the application is structured according to the" n
    "%% OTP design principles as a supervision tree, this means starting the" n
    "%% top supervisor of the tree." n
    "%% @end" n
    (erlang-skel-separator 2)
    "start(_Type, StartArgs) ->" n>
    "case 'TopSupervisor':start_link(StartArgs) of" n>
    "{ok, Pid} -> " n>
    "{ok, Pid};" n>
    "Error ->" n>
    "Error" n>
    "end." n
    n
    (erlang-skel-separator 2)
    "%% @spec stop(State) -> void()." n
    "%% " n
    "%% @doc " n
    "%% This function is called whenever an application" n
    "%% has stopped. It is intended to be the opposite of Module:start/2 and" n
    "%% should do any necessary cleaning up. The return value is ignored. "n
    "%% @end "n
    (erlang-skel-separator 2)
    "stop(_State) ->" n>
    "ok." n
    n
    (erlang-skel-double-separator 2)
    "%% Internal functions" n
    (erlang-skel-double-separator 2)
    )
  "*The template of an application behaviour.
Please see the function `tempo-define-template'.")

(defvar erlang-skel-supervisor
  '((erlang-skel-include erlang-skel-large-header)
    "-behaviour(supervisor)." n n

    "%% API" n
    "-export([start_link/0])." n n 

    "%% Supervisor callbacks" n
    "-export([init/1])." n n

    "-define(SERVER, ?MODULE)." n n
    
    (erlang-skel-double-separator 2)
    "%% API functions" n
    (erlang-skel-double-separator 2)
    (erlang-skel-separator 2)
    "%% @spec start_link() -> {ok,Pid} | ignore | {error,Error}." n
    "%% " n
    "%% @doc " n
    "%% Starts the supervisor" n
    "%% @end" n
    (erlang-skel-separator 2)
    "start_link() ->" n>
    "supervisor:start_link({local, ?SERVER}, ?MODULE, [])." n 
    n
    (erlang-skel-double-separator 2)
    "%% Supervisor callbacks" n
    (erlang-skel-double-separator 2)
    (erlang-skel-separator 2)
    "%% @spec init(Args) -> {ok,  {SupFlags,  [ChildSpec]}} |" n
    "%%                     ignore                          |" n
    "%%                     {error, Reason}." n
    "%% " n
    "%% @doc " n
    "%%  Whenever a supervisor is started using "n
    "%% supervisor:start_link/[2,3], this function is called by the new process "n
    "%% to find out about restart strategy, maximum restart frequency and child "n
    "%% specifications." n
    "%% @end" n
    (erlang-skel-separator 2)
    "init([]) ->" n>
    "AChild = {'AName',{'AModule',start_link,[]}," n>
    "permanent,2000,worker,['AModule']}," n>
    "{ok,{{one_for_all,0,1}, [AChild]}}." n
    n
    (erlang-skel-double-separator 2)
    "%% Internal functions" n
    (erlang-skel-double-separator 2)
    )
  "*The template of an supervisor behaviour.
Please see the function `tempo-define-template'.")

(defvar erlang-skel-supervisor-bridge
  '((erlang-skel-include erlang-skel-large-header)
    "-behaviour(supervisor_bridge)." n n

    "%% API" n
    "-export([start_link/0])." n n

    "%% supervisor_bridge callbacks" n
    "-export([init/1, terminate/2])." n n
    
    "-define(SERVER, ?MODULE)." n n

    "-record(state, {})." n n
     
    (erlang-skel-double-separator 2)
    "%% API" n
    (erlang-skel-double-separator 2)
    (erlang-skel-separator 2)
    "%% @spec start_link() -> {ok,Pid} | ignore | {error,Error}." n
    "%% " n
    "%% @doc " n
    "%% Starts the supervisor bridge" n
    "%% @end" n
    (erlang-skel-separator 2) 
    "start_link() ->" n>
    "supervisor_bridge:start_link({local, ?SERVER}, ?MODULE, [])." n
    n
    (erlang-skel-double-separator 2)
    "%% supervisor_bridge callbacks" n
    (erlang-skel-double-separator 2)
    (erlang-skel-separator 2)
    "%% @spec init(Args) -> {ok,  Pid, State} |" n
    "%%                     ignore            |" n
    "%%                     {error, Reason}.    " n
    "%% " n
    "%% @doc " n
    "%% Creates a supervisor_bridge process, linked to the calling" n
    "%% process, which calls Module:init/1 to start the subsystem. To ensure a" n
    "%% synchronized start-up procedure, this function does not return until" n
    "%% Module:init/1 has returned. "  n
    "%% @end "  n
    (erlang-skel-separator 2)
    "init([]) ->" n>
    "case 'AModule':start_link() of" n>
    "{ok, Pid} ->" n>
    "{ok, Pid, #state{}};" n>
    "Error ->" n>
    "Error" n>
    "end." n
    n
    (erlang-skel-separator 2)
    "%% @spec terminate(Reason, State) -> void()." n
    "%% " n
    "%% @doc " n 
    "%% This function is called by the supervisor_bridge when it is"n
    "%% about to terminate. It should be the opposite of Module:init/1 and stop"n
    "%% the subsystem and do any necessary cleaning up.The return value is ignored." n
    "%% @end " n
    (erlang-skel-separator 2)
    "terminate(Reason, State) ->" n>
    "'AModule':stop()," n>
    "ok." n
    n
    (erlang-skel-double-separator 2)
    "%% Internal functions" n
    (erlang-skel-double-separator 2)
    )
  "*The template of an supervisor_bridge behaviour.
Please see the function `tempo-define-template'.")

(defvar erlang-skel-generic-server
  '((erlang-skel-include erlang-skel-large-header)
    "-behaviour(gen_server)." n n

    "%% API" n
    "-export([start_link/0])." n n
    
    "%% gen_server callbacks" n
    "-export([init/1, handle_call/3, handle_cast/2, "
    "handle_info/2," n>
    "terminate/2, code_change/3])." n n

    "-define(SERVER, ?MODULE). " n n

    "-record(state, {})." n n
    
    (erlang-skel-double-separator 2)
    "%% API" n
    (erlang-skel-double-separator 2)
    (erlang-skel-separator 2)
    "%% @spec start_link() -> {ok,Pid} | ignore | {error,Error}." n
    "%% " n
    "%% @doc " n
    "%% Starts the server" n
    "%% @end " n
    (erlang-skel-separator 2) 
    "start_link() ->" n>
    "gen_server:start_link({local, ?SERVER}, ?MODULE, [], [])." n
    n
    (erlang-skel-double-separator 2)
    "%% gen_server callbacks" n
    (erlang-skel-double-separator 2)
    n
    (erlang-skel-separator 2)
    "%% @spec init(Args) -> {ok, State} |" n
    "%%                     {ok, State, Timeout} |" n
    "%%                     ignore               |" n
    "%%                     {stop, Reason}." n
    "%% " n   
    "%% @doc " n
    "%% Initiates the server" n
    "%% @end " n
    (erlang-skel-separator 2)
    "init([]) ->" n>
    "{ok, #state{}}." n
    n
    (erlang-skel-separator 2)
    "%% @spec handle_call(Request, From, State) -> {reply, Reply, State} |" n
    "%%                                      {reply, Reply, State, Timeout} |" n
    "%%                                      {noreply, State} |" n
    "%%                                      {noreply, State, Timeout} |" n
    "%%                                      {stop, Reason, Reply, State} |" n 
    "%%                                      {stop, Reason, State}." n       
    "%% " n
    "%% @doc " n
    "%% Handling call messages" n
    "%% @end " n
    (erlang-skel-separator 2)
    "handle_call(_Request, _From, State) ->" n>
    "Reply = ok," n>
    "{reply, Reply, State}." n
    n
    (erlang-skel-separator 2)
    "%% @spec handle_cast(Msg, State) -> {noreply, State} |" n
    "%%                                      {noreply, State, Timeout} |" n
    "%%                                      {stop, Reason, State}." n
    "%% " n
    "%% @doc " n 
    "%% Handling cast messages" n
    "%% @end " n

    (erlang-skel-separator 2)
    "handle_cast(_Msg, State) ->" n>
    "{noreply, State}." n
    n
    (erlang-skel-separator 2)
    "%% @spec handle_info(Info, State) -> {noreply, State} |" n
    "%%                                       {noreply, State, Timeout} |" n
    "%%                                       {stop, Reason, State}." n
    "%% " n
    "%% @doc " n
    "%% Handling all non call/cast messages" n
    "%% @end " n
    (erlang-skel-separator 2)
    "handle_info(_Info, State) ->" n>
    "{noreply, State}." n
    n
    (erlang-skel-separator 2)
    "%% @spec terminate(Reason, State) -> void()." n
    "%% " n
    "%% @doc " n
    "%% This function is called by a gen_server when it is about to"n
    "%% terminate. It should be the opposite of Module:init/1 and do any necessary"n
    "%% cleaning up. When it returns, the gen_server terminates with Reason." n
    "%% The return value is ignored." n 
    "%% @end " n
    (erlang-skel-separator 2)
    "terminate(_Reason, _State) ->" n>
    "ok." n
    n
    (erlang-skel-separator 2)
    "%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}." n
    "%% " n
    "%% @doc " n
    "%% Convert process state when code is changed" n
    "%% @end " n
    (erlang-skel-separator 2)
    "code_change(_OldVsn, State, _Extra) ->" n>
    "{ok, State}." n
    n
    (erlang-skel-double-separator 2)
    "%%% Internal functions" n
    (erlang-skel-double-separator 2)
    )
  "*The template of a generic server.
Please see the function `tempo-define-template'.")

(defvar erlang-skel-gen-event
  '((erlang-skel-include erlang-skel-large-header)
    "-behaviour(gen_event)." n

    "%% API" n
    "-export([start_link/0, add_handler/0])." n n

    "%% gen_event callbacks" n
    "-export([init/1, handle_event/2, handle_call/2, " n>
    "handle_info/2, terminate/2, code_change/3])." n n

    "-record(state, {})." n n

    (erlang-skel-double-separator 2)
    "%% gen_event callbacks" n
    (erlang-skel-double-separator 2)
    (erlang-skel-separator 2)
    "%% @spec start_link() -> {ok,Pid} | {error,Error}. " n
    "%% " n
    "%% @doc " n
    "%% Creates an event manager." n
    "%% @end " n
    (erlang-skel-separator 2) 
    "start_link() ->" n>
    "gen_event:start_link({local, ?SERVER}). " n
    n
    (erlang-skel-separator 2)
    "%% @spec add_handler() -> ok | {'EXIT',Reason} | term()." n
    "%% " n
    "%% @doc " n
    "%% Adds an event handler" n
    "%% @end " n
    (erlang-skel-separator 2) 
    "add_handler() ->" n>
    "gen_event:add_handler(?SERVER, ?MODULE, [])." n 
    n
    (erlang-skel-double-separator 2)
    "%% gen_event callbacks" n
    (erlang-skel-double-separator 2)
    (erlang-skel-separator 2)
    "%% @spec init(Args) -> {ok, State}." n
    "%% " n
    "%% @doc " n
    "%% Whenever a new event handler is added to an event manager,"n
    "%% this function is called to initialize the event handler." n
    "%% @end " n
    (erlang-skel-separator 2)
    "init([]) ->" n>
    "{ok, #state{}}." n
    n
    (erlang-skel-separator 2)
    "%% @spec  handle_event(Event, State) -> {ok, State} |" n
    "%%                               {swap_handler, Args1, State1, Mod2, Args2} |"n
    "%%                               remove_handler." n
    "%% " n
    "%% @doc " n
    "%% Whenever an event manager receives an event sent using"n
    "%% gen_event:notify/2 or gen_event:sync_notify/2, this function is called for"n
    "%% each installed event handler to handle the event. "n
    "%% @end " n
    (erlang-skel-separator 2)
    "handle_event(_Event, State) ->" n>
    "{ok, State}." n
    n
    (erlang-skel-separator 2)
    "%% @spec handle_call(Request, State) -> {ok, Reply, State} |" n
    "%%                                {swap_handler, Reply, Args1, State1, "n
    "%%                                  Mod2, Args2} |" n
    "%%                                {remove_handler, Reply}." n
    "%% " n
    "%% @doc " n
    "%% Whenever an event manager receives a request sent using"n
    "%% gen_event:call/3,4, this function is called for the specified event "n
    "%% handler to handle the request."n
    "%% @end " n
    (erlang-skel-separator 2)
    "handle_call(_Request, State) ->" n>
    "Reply = ok," n>
    "{ok, Reply, State}." n
    n
    (erlang-skel-separator 2)
    "%% @spec handle_info(Info, State) -> {ok, State} |" n
    "%%                             {swap_handler, Args1, State1, Mod2, Args2} |" n
    "%%                              remove_handler." n
    "%% " n
    "%% @doc " n
    "%% This function is called for each installed event handler when"n
    "%% an event manager receives any other message than an event or a synchronous"n
    "%% request (or a system message)."n
    "%% @end " n
    (erlang-skel-separator 2)
    "handle_info(_Info, State) ->" n>
    "{ok, State}." n
    n
    (erlang-skel-separator 2)
    "%% @spec terminate(Reason, State) -> void()." n
    "%% " n
    "%% @doc " n
    "%% Whenever an event handler is deleted from an event manager,"n
    "%% this function is called. It should be the opposite of Module:init/1 and "n
    "%% do any necessary cleaning up. " n
    "%% @end " n
    (erlang-skel-separator 2)
    "terminate(_Reason, _State) ->" n>
    "ok." n
    n
    (erlang-skel-separator 2)
    "%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}. " n
    "%% " n
    "%% @doc " n
    "%% Convert process state when code is changed" n
    "%% @end " n
    (erlang-skel-separator 2)
    "code_change(_OldVsn, State, _Extra) ->" n>
    "{ok, State}." n
    n
    (erlang-skel-double-separator 2)
    "%%% Internal functions" n
    (erlang-skel-double-separator 2)
    )
  "*The template of a gen_event.
Please see the function `tempo-define-template'.")

(defvar erlang-skel-gen-fsm
  '((erlang-skel-include erlang-skel-large-header)
    "-behaviour(gen_fsm)." n n

    "%% API" n
    "-export([start_link/0])." n n
    
    "%% gen_fsm callbacks" n
    "-export([init/1, state_name/2, state_name/3, handle_event/3," n>
    "handle_sync_event/4, handle_info/3, terminate/3, code_change/4])." n n

    "-record(state, {})." n n

    (erlang-skel-double-separator 2)
    "%% API" n
    (erlang-skel-double-separator 2)
    (erlang-skel-separator 2)
    "%% @spec start_link() -> ok,Pid} | ignore | {error,Error}." n
    "%% " n
    "%% @doc " n 
    "%% Creates a gen_fsm process which calls Module:init/1 to"n
    "%% initialize. To ensure a synchronized start-up procedure, this function" n
    "%% does not return until Module:init/1 has returned.  " n
    "%% @end " n
    (erlang-skel-separator 2) 
    "start_link() ->" n>
    "gen_fsm:start_link({local, ?SERVER}, ?MODULE, [], [])." n 
    n
    (erlang-skel-double-separator 2)
    "%% gen_fsm callbacks" n
    (erlang-skel-double-separator 2)
    (erlang-skel-separator 2)
    "%% @spec init(Args) -> {ok, StateName, State} |" n
    "%%                         {ok, StateName, State, Timeout} |" n
    "%%                         ignore                              |" n
    "%%                         {stop, StopReason}.                   " n
    "%% " n
    "%% @doc " n
    "%% Whenever a gen_fsm is started using gen_fsm:start/[3,4] or"n
    "%% gen_fsm:start_link/3,4, this function is called by the new process to "n
    "%% initialize. " n
    "%% @end " n
    (erlang-skel-separator 2)
    "init([]) ->" n>
    "{ok, state_name, #state{}}." n
    n
    (erlang-skel-separator 2)
    "%% @spec state_name(Event, State) -> {next_state, NextStateName, NextState}|" n
    "%%                             {next_state, NextStateName, " n
    "%%                                NextState, Timeout} |" n
    "%%                             {stop, Reason, NewState}." n
    "%% " n
    "%% @doc " n
    "%% There should be one instance of this function for each possible"n
    "%% state name. Whenever a gen_fsm receives an event sent using" n
    "%% gen_fsm:send_event/2, the instance of this function with the same name as"n
    "%% the current state name StateName is called to handle the event. It is also "n
    "%% called if a timeout occurs. " n
    "%% @end " n
    (erlang-skel-separator 2)
    "state_name(_Event, State) ->" n>
    "{next_state, state_name, State}." n
    n
    (erlang-skel-separator 2)
    "%% @spec state_name(Event, From, State) -> {next_state, NextStateName, NextState} |"n
    "%%                                   {next_state, NextStateName, " n
    "%%                                     NextState, Timeout} |" n
    "%%                                   {reply, Reply, NextStateName, NextState}|"n
    "%%                                   {reply, Reply, NextStateName, " n
    "%%                                    NextState, Timeout} |" n
    "%%                                   {stop, Reason, NewState}|" n
    "%%                                   {stop, Reason, Reply, NewState}." n
    "%% " n
    "%% @doc " n
    "%%  There should be one instance of this function for each" n
    "%%  possible state name. Whenever a gen_fsm receives an event sent using" n
    "%%  gen_fsm:sync_send_event/2,3, the instance of this function with the same"n
    "%%  name as the current state name StateName is called to handle the event." n
    "%% @end " n
    (erlang-skel-separator 2)
    "state_name(_Event, _From, State) ->" n>
    "Reply = ok," n>
    "{reply, Reply, state_name, State}." n
    n
    (erlang-skel-separator 2)
    "%% @spec handle_event(Event, StateName, State) -> {next_state, NextStateName, "n
    "%%						  NextState} |" n
    "%%                                          {next_state, NextStateName, "n
    "%%					          NextState, Timeout} |" n
    "%%                                          {stop, Reason, NewState}." n
    "%% " n
    "%% @doc " n 
    "%% Whenever a gen_fsm receives an event sent using"n
    "%% gen_fsm:send_all_state_event/2, this function is called to handle"n
    "%% the event." n
    "%% @end " n
    (erlang-skel-separator 2)
    "handle_event(_Event, StateName, State) ->" n>
    "{next_state, StateName, State}." n
    n
    (erlang-skel-separator 2)
    "%% @spec handle_sync_event(Event, From, StateName, "n
    "%%                   State) -> {next_state, NextStateName, NextState} |" n
    "%%                             {next_state, NextStateName, NextState, " n
    "%%                              Timeout} |" n
    "%%                             {reply, Reply, NextStateName, NextState}|" n
    "%%                             {reply, Reply, NextStateName, NextState, " n
    "%%                              Timeout} |" n
    "%%                             {stop, Reason, NewState} |" n
    "%%                             {stop, Reason, Reply, NewState}." n
    "%% " n
    "%% @doc " n
    "%% Whenever a gen_fsm receives an event sent using"n
    "%% gen_fsm:sync_send_all_state_event/2,3, this function is called to handle"n
    "%% the event."n
    "%% @end " n    
    (erlang-skel-separator 2)
    "handle_sync_event(Event, From, StateName, State) ->" n>
    "Reply = ok," n>
    "{reply, Reply, StateName, State}." n
    n
    (erlang-skel-separator 2)
    "%% @spec handle_info(Info,StateName,State)-> {next_state, NextStateName, NextState}|" n
    "%%                                     {next_state, NextStateName, NextState, "n
    "%%                                       Timeout} |" n
    "%%                                     {stop, Reason, NewState}." n
    "%% " n
    "%% @doc " n
    "%% This function is called by a gen_fsm when it receives any"n
    "%% other message than a synchronous or asynchronous event"n
    "%% (or a system message)." n
    "%% @end " n
    (erlang-skel-separator 2)
    "handle_info(_Info, StateName, State) ->" n>
    "{next_state, StateName, State}." n
    n
    (erlang-skel-separator 2)
    "%% @spec terminate(Reason, StateName, State) -> void()." n
    "%% " n
    "%% @doc " n
    "%% This function is called by a gen_fsm when it is about"n
    "%% to terminate. It should be the opposite of Module:init/1 and do any"n
    "%% necessary cleaning up. When it returns, the gen_fsm terminates with"n
    "%% Reason. The return value is ignored." n
    "%% @end " n
    (erlang-skel-separator 2)
    "terminate(_Reason, _StateName, _State) ->" n>
    "ok." n
    n
    (erlang-skel-separator 2)
    "%% @speccode_change(OldVsn, StateName, State, Extra) -> {ok, StateName, NewState}." n
    "%% " n
    "%% @doc " n
    "%% Convert process state when code is changed" n
    "%% @end " n
    (erlang-skel-separator 2)
    "code_change(_OldVsn, StateName, State, _Extra) ->" n>
    "{ok, StateName, State}." n
    n
    (erlang-skel-double-separator 2)
    "%%% Internal functions" n
    (erlang-skel-double-separator 2)
    )
  "*The template of a gen_fsm.
Please see the function `tempo-define-template'.")

(defvar erlang-skel-lib
  '((erlang-skel-include erlang-skel-large-header)

    "%% API" n
    "-export([])." n n
    
    (erlang-skel-double-separator 2)
    "%% API" n
    (erlang-skel-double-separator 2)
    (erlang-skel-separator 2)
    "%% @spec " n
    "%% @doc" n
    "%% " n
    "%% @end " n
    (erlang-skel-separator 2) 
    n
    (erlang-skel-double-separator 2)
    "%% Internal functions" n
    (erlang-skel-double-separator 2)
    )
  "*The template of a library module.
Please see the function `tempo-define-template'.")

(defvar erlang-skel-corba-callback
  '((erlang-skel-include erlang-skel-large-header)
    "%% Include files" n n

    "%% API" n
    "-export([])." n n

    "%% Corba callbacks" n
    "-export([init/1, terminate/2, code_change/3])." n n

    "-record(state, {})." n n
    
    (erlang-skel-double-separator 2)
    "%% Corba callbacks" n
    (erlang-skel-double-separator 2)
    (erlang-skel-separator 2)
    "%% @spec init(Args) -> {ok, State} |" n
    "%%                         {ok, State, Timeout} |" n
    "%%                         ignore               |" n
    "%%                         {stop, Reason}." n
    "%% " n
    "%% @doc " n
    "%% Initiates the server" n
    "%% @end " n
    (erlang-skel-separator 2)
    "init([]) ->" n>
    "{ok, #state{}}." n
    n
    (erlang-skel-separator 2)
    "%% @spec terminate(Reason, State) -> void()." n
    "%% " n
    "%% @doc " n
    "%% Shutdown the server" n
    "%% @end " n
    (erlang-skel-separator 2)
    "terminate(_Reason, _State) ->" n>
    "ok." n
    n
    (erlang-skel-separator 2)
    "%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}. " n
    "%% " n
    "%% @doc " n
    "%%  Convert process state when code is changed" n
    "%% @end " n
    (erlang-skel-separator 2)
    "code_change(_OldVsn, State, _Extra) ->" n>
    "{ok, State}." n
    n
    (erlang-skel-double-separator 2)
    "%% Internal functions" n
    (erlang-skel-double-separator 2)
    )
  "*The template of a library module.
Please see the function `tempo-define-template'.")

(defvar erlang-skel-ts-test-suite
 '((erlang-skel-include erlang-skel-large-header)
   "%% Note: This directive should only be used in test suites." n
    "-compile(export_all)." n n

    "-include(\"test_server.hrl\")." n n
    
    "%% Test server callback functions" n
    (erlang-skel-separator 2)
    "%% @spec init_per_suite(Config) -> Config." n
    "%% " n
    "%% @doc " n
    "%% Config - [tuple()]" n
    "%%   A list of key/value pairs, holding the test case configuration." n
    "%% Initiation before the whole suite" n
    "%%" n
    "%% Note: This function is free to add any key/value pairs to the Config" n
    "%% variable, but should NOT alter/remove any existing entries." n
    "%% @end " n
    (erlang-skel-separator 2) 
    "init_per_suite(Config) ->" n >
    "Config." n n

    (erlang-skel-separator 2)
    "%% @spec end_per_suite(Config) -> _." n
    "%% " n
    "%% @doc " n
    "%% Config - [tuple()]" n
    "%%   A list of key/value pairs, holding the test case configuration." n
    "%% Cleanup after the whole suite" n
    "%% @end " n
    (erlang-skel-separator 2)
    "end_per_suite(_Config) ->" n >
    "ok." n n

    (erlang-skel-separator 2)
    "%% @spec init_per_testcase(TestCase, Config) -> Config." n
    "%% " n
    "%% @doc " n
    "%% Case - atom()" n
    "%%   Name of the test case that is about to be run." n
    "%% Config - [tuple()]" n
    "%%   A list of key/value pairs, holding the test case configuration." n
    "%%" n
    "%%  Initiation before each test case" n
    "%%" n
    "%% Note: This function is free to add any key/value pairs to the Config" n
    "%% variable, but should NOT alter/remove any existing entries." n
    "%% @doc Initiation before each test case" n
    "%% @end " n
    (erlang-skel-separator 2)
    "init_per_testcase(_TestCase, Config) ->" n >
    "Config." n n

    (erlang-skel-separator 2)
    "%% @spec end_per_testcase(TestCase, Config) -> _." n
    "%% " n
    "%% @doc "
    "%% Case - atom()" n
    "%%   Name of the test case that is about to be run." n
    "%% Config - [tuple()]" n
    "%%   A list of key/value pairs, holding the test case configuration." n
    "%%  Cleanup after each test case" n
    "%% @end " n
    (erlang-skel-separator 2)
    "end_per_testcase(_TestCase, _Config) ->" n >
    "ok."n n

    (erlang-skel-separator 2)
    "%% @spec all(Clause) -> TestCases." n
    "%% " n
    "%% @doc " n
    "%% Clause - atom() - suite | doc" n
    "%% TestCases - [Case] " n
    "%% Case - atom()" n
    "%%   Name of a test case." n
    "%% Returns a list of all test cases in this test suite" n
    "%% @end " n
    (erlang-skel-separator 2)
    "all(doc) -> " n >
    "[\"Describe the main purpose of this suite\"];" n n
    "all(suite) -> " n >
    "[]." n n
    
    "%% Test cases starts here." n
    (erlang-skel-separator 2)
    "test_case(doc) -> " n >
    "[\"Describe the main purpose of test case\"];" n n
    "test_case(suite) -> " n >
    "[];" n n
    "test_case(Config) when is_list(Config) -> " n >
    "ok." n
   )
 "*The template of a library module.
Please see the function `tempo-define-template'.")

(defvar erlang-skel-ct-test-suite
 '((erlang-skel-include erlang-skel-large-header)
   "-suite_defaults([{timetrap, {minutes, 10}}])." n n

   "%% Note: This directive should only be used in test suites." n
    "-compile(export_all)." n n

    "-include(\"ct.hrl\")." n n
    
    "%% Test server callback functions" n
    (erlang-skel-separator 2)
    "%% @spec init_per_suite(Config) -> Config." n
    "%% " n
    "%% @doc " n
    "%% Config - [tuple()]" n
    "%%   A list of key/value pairs, holding the test case configuration." n
    "%%  Initiation before the whole suite" n
    "%%" n
    "%% Note: This function is free to add any key/value pairs to the Config" n
    "%% variable, but should NOT alter/remove any existing entries." n
    "%% @end " n
    (erlang-skel-separator 2) 
    "init_per_suite(Config) ->" n >
    "Config." n n

    (erlang-skel-separator 2)
    "%% @spec end_per_suite(Config) -> _." n
    "%% " n
    "%% @doc " n
    "%% Config - [tuple()]" n
    "%%   A list of key/value pairs, holding the test case configuration." n
    "%%  Cleanup after the whole suite" n
    "%% @end " n
    (erlang-skel-separator 2)
    "end_per_suite(_Config) ->" n >
    "ok." n n

    (erlang-skel-separator 2)
    "%% @spec init_per_testcase(TestCase, Config) -> Config." n
    "%% " n
    "%% @doc " n
    "%% Case - atom()" n
    "%%   Name of the test case that is about to be run." n
    "%% Config - [tuple()]" n
    "%%   A list of key/value pairs, holding the test case configuration." n
    "%%" n
    "%% Initiation before each test case" n
    "%%" n
    "%% Note: This function is free to add any key/value pairs to the Config" n
    "%% variable, but should NOT alter/remove any existing entries." n
    "%% Initiation before each test case" n
    "%% @end " n
    (erlang-skel-separator 2)
    "init_per_testcase(_TestCase, Config) ->" n >
    "Config." n n

    (erlang-skel-separator 2)
    "%% @spec end_per_testcase(TestCase, Config) -> _." n
    "%% " n
    "%% @doc " n
    "%% Case - atom()" n
    "%%   Name of the test case that is about to be run." n
    "%% Config - [tuple()]" n
    "%%   A list of key/value pairs, holding the test case configuration." n
    "%%  Cleanup after each test case" n
    "%% @end " n
    (erlang-skel-separator 2)
    "end_per_testcase(_TestCase, _Config) ->" n >
    "ok."n n

    (erlang-skel-separator 2)
    "%% @spec all() -> TestCases." n
    "%% " n
    "%% @doc " n
    "%% TestCases - [Case] " n
    "%% Case - atom()" n
    "%%   Name of a test case." n
    "%% Returns a list of all test cases in this test suite" n
    "%% @end " n
    (erlang-skel-separator 2)
    "all() -> " n >
    "[]." n n
    
    "%% Test cases starts here." n
    (erlang-skel-separator 2)
    "test_case() -> " n >
    "[{doc, \"Describe the main purpose of this test case\"}]." n n
    "test_case(Config) when is_list(Config) -> " n >
    "ok." n
    )
 "*The template of a library module.
Please see the function `tempo-define-template'.")

;; Skeleton code:

;; This code is based on the package `tempo' which is part of modern
;; Emacsen.  (GNU Emacs 19.25 (?) and XEmacs 19.14.)

(defun erlang-skel-init ()
  "Generate the skeleton functions and menu items.
The variable `erlang-skel' contains the name and descriptions of
all skeletons.

The skeleton routines are based on the `tempo' package.  Should this
package not be present, this function does nothing."
  (interactive)
  (condition-case nil
      (require 'tempo)
    (error t))
  (if (featurep 'tempo)
      (let ((skel erlang-skel)
	    (menu '()))
	(while skel
	  (cond ((null (car skel))
		 (setq menu (cons nil menu)))
		(t
		 (funcall (symbol-function 'tempo-define-template)
			  (concat "erlang-" (nth 1 (car skel)))
			  ;; The tempo template used contains an `include'
			  ;; function call only, hence changes to the
			  ;; variables describing the templates take effect
			  ;; immdiately.
			  (list (list 'erlang-skel-include (nth 2 (car skel))))
			  (nth 1 (car skel)))
		 (setq menu (cons (erlang-skel-make-menu-item
				   (car skel)) menu))))
	  (setq skel (cdr skel)))
	(setq erlang-menu-skel-items
	      (list nil (list "Skeletons" (nreverse menu))))
	(setq erlang-menu-items
	      (erlang-menu-add-above 'erlang-menu-skel-items
				     'erlang-menu-version-items
				     erlang-menu-items))
	(erlang-menu-init))))

(defun erlang-skel-make-menu-item (skel)
  (let ((func (intern (concat "tempo-template-erlang-" (nth 1 skel)))))
    (cond ((null (nth 3 skel))
	   (list (car skel) func))
	  (t
	   (list (car skel)
		 (list 'lambda '()
		       '(interactive)
		       (list 'funcall
			     (list 'quote (nth 3 skel))
			     (list 'quote func))))))))

;; Functions designed to be added to the skeleton menu.
;; (Not normally used)
(defun erlang-skel-insert (func)
  "Insert skeleton generated by FUNC and goto first tempo mark."
  (save-excursion (funcall func))
  (funcall (symbol-function 'tempo-forward-mark)))

(defun erlang-skel-header (func)
  "Insert the header generated by FUNC at the beginning of the buffer."
  (goto-char (point-min))
  (save-excursion (funcall func))
  (funcall (symbol-function 'tempo-forward-mark)))


;; Functions used inside the skeleton descriptions.
(defun erlang-skel-skip-blank ()
  (skip-chars-backward " \t")
  nil)

(defun erlang-skel-include (&rest args)
  "Include a template inside another template.

Example of use, assuming that `erlang-skel-func' is defined:

 (defvar foo-skeleton '(\"%%% New function:\"
                        (erlang-skel-include erlang-skel-func)))

Technically, this function returns the `tempo' attribute`(l ...)' which
can contain other `tempo' attributes.  Please see the function
`tempo-define-template' for a description of the `(l ...)' attribute."
  (let ((res '())
	entry)
    (while args
      (setq entry (car args))
      (while entry
	(setq res (cons (car entry) res))
	(setq entry (cdr entry)))
      (setq args (cdr args)))
    (cons 'l (nreverse res))))

(defun erlang-skel-separator (&optional percent)
  "Return a comment separator."
  (let ((percent (or percent 3)))
    (concat (make-string percent ?%) 
	    (make-string (- 70 percent) ?-) 
	    "\n")))

(defun erlang-skel-double-separator (&optional percent)
  "Return a comment separator."
  (let ((percent (or percent 3)))
    (concat (make-string percent ?%) 
	    (make-string (- 70 percent) ?=) 
	    "\n")))

(defun erlang-skel-dd-mmm-yyyy ()
  "Return the current date as a string in \"DD Mon YYYY\" form.
The first character of DD is space if the value is less than 10."
  (let ((date (current-time-string)))
    (format "%2d %s %s"
	    (string-to-int (substring date 8 10))
	    (substring date 4 7)
	    (substring date -4))))

;; Local variables:
;; coding: iso-8859-1
;; End:

;;; erlang.el ends here
