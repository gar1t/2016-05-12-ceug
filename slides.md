<style>
.reveal .step {
  text-align: left;
  margin-left: 50px;
  margin-top: 20px;
}
</style>

# Topics

- Sequential Erlang (the language)
- Process management
- Ecosystem (libraries, tools, community)

---

## Notable omissions

- Distribution

---

## Why Erlang?

- Concurrency
- Ecosystem
- Simple, functional

---

## Technical differences

- Message passing, no shared memory
- Efficient garbage collection
- Full process semantics and guarantees (fault tolerance)

---

### Some Erlang

```erlang
-module(sample_cli).

-export([main/1]).

main(Args) ->
    Parser = sample_parser(),
    handle_parsed(cli:parse_args(Args, Parser)).

handle_parsed({{ok, print_help}, P}) ->
    cli:print_help(P);
handle_parsed({{ok, print_version}, P}) ->
    cli:print_version(P);
handle_parsed({{ok, Parsed}, _P}) ->
    handle_args(Parsed);
handle_parsed({{error, Err}, P}) ->
    cli:print_error_and_halt(Err, P).
```

---

### Important to know!

- Lower case names are *atoms*<br>`red, cat, ok`
- Upper case names are *variables*<br>`Color, Pet, Status`
- Square brackets are *lists*<br>`[Color, 123, "Sam"]`
- Curley brackets are *tuples*<br>`{car, {"Dodge", "Dart"}, 1988}`

---

### Important to know <small>(cont)</small>

- Function calls look like this<br>`math:add(1, 2)`
- Function is uniquely identified in a module by *name* and *arity*
  (number of arguments)

---

### Some Erlang <small>(again)</small>

```erlang
-module(sample_cli).

-export([main/1]).

main(Args) ->
    Parser = sample_parser(),
    handle_parsed(cli:parse_args(Args, Parser)).

handle_parsed({{ok, print_help}, P}) ->
    cli:print_help(P);
handle_parsed({{ok, print_version}, P}) ->
    cli:print_version(P);
handle_parsed({{ok, Parsed}, _P}) ->
    handle_args(Parsed);
handle_parsed({{error, Err}, P}) ->
    cli:print_error_and_halt(Err, P).
```

---

## Building blocks

- **Module**<br>One erl source file, compiles to beam
- **Application**<br>Bundle of beams + app metadata
- **Release**<br>Bundle of applications + boot script

---

## Modules contain function definitions

---

## Module

- Source code in *`MODULE_NAME`*`.erl`
- Compiled code in *`MODULE_NAME`*`.beam`
- Source requires `-module(`*`MODULE_NAME`*`)` attribute
- Source may contain other attributes and functions

---

### Applications contain *modules*<br>AND<br>Instructions for starting and<br>running a *subsystem*

---

## Application

- `ebin` dir with *`APP_NAME`*`.app` file
- `priv` dir (optional) contains app resources -- e.g. templates, translations, images
- `src` dir (optional) contains `*.erl` files

---

### `tensorhub.app`

```erlang
{application, tensorhub,
 [{description, "TensorHub application"},
  {vsn, "0.0.0"},
  {modules, [bin_to_hex,hub_app,hub_cache,hub_crypto,...]},
  {registered, []},
  {applications, [kernel, stdlib, sasl, crypto, inets,
                  runtime_tools, e2, psycho, erlydtl, erlexec,
                  erlport, mysql, jiffy]},
  {mod, {e2_application, [hub_app]}},
  {env, []}
 ]}.
```
---

### `tensorhub_app.erl`

```erlang
-module(tensorhub_app).

-export([init/0]).

init() ->
    psycho_mime:init(),
    hub_page:compile_templates(),
    {ok, children()}.

children() ->
    [{hub_python, [{shutdown, 1000}]},
     {hub_db,     [{shutdown, 1000}]},
     {hub_index,  [{shutdown, 1000}]},
     {hub_cache,  [{shutdown, 1000}]},
     {hub_http,   [{shutdown, 1000}]},
     hub_stat,
     hub_event,
     hub_stat_server,
     hub_db_rotate_notify].
```
---

### `application:start(tensorhub)`

<img src="otp-app.png">

---

### `application:start(inets)`

<img src="otp-app-2.png">

---

### Observer - Apps

<img src="apps.png">

---

### Releases contain *applications*<br>AND<br>Instructions for starting and running a *full system*

---

### tensorhub `relx.config`

```erlang
{release, {tensorhub_release, "1"}, [tensorhub]}.
{sys_config, "rel/sys.config"}.
{vm_args, "rel/vm.args"}.
{generate_start_script, false}.
{output_dir, "./rel"}.
```
---

### `./rel/tensorhub_release`

<img src="rel.png">

---

### Starting a release

<center>
<code style="font-size:150%;letter-spacing:-2px;white-space:nowrap"
  >erts/bin/erl -boot BOOT -config CONFIG</code>
</center>

---

### Starting tensorhub

```shell
$ rel/tensorhub_release/erts-7.3/bin/erl \
>   -boot rel/tensorhub_release/releases/1/tensorhub_release \
>   -config priv/dev
```

---

### `priv/dev.config`

```erlang
[{tensorhub,
  [{home,                 "."},
   {etc_dir,              "priv"},
   {cache_dir,            "priv/var"},
   {data_dir,             "priv/var"},
   {index_repo,           "/home/garrett/SCM/tensorhub-index"},
   {github_client_id,     "..."},
   {github_client_secret, "..."},
   {encrypt_key,          {1, <<"...">>}},
   {indexer_bin,          "/usr/local/bin/indexer"},
   {curl_bin,             "/usr/bin/curl"}]
 },
 {other_app, [{some_attr, "123"}]}
].

```
---

# Back to modules

---

## A classic!

```erlang
-module(aclassic).

-export([fib/1]).

fib(0)            -> 0;
fib(1)            -> 1;
fib(N) when N > 1 -> fib(N-1) + fib(N-2).
```

---

### *Attributes* give the compiler extra information

---

## Common attributes

- `module` Required to declare a module
- `export`<br>Declare functions accessible outside the module
- `include` and `include_lib`<br>Include source from other files
- `define` Define a macro

---

## Common attributes (cont)

- `record` Define a record
- `behavior`<br>Declare module to implement a behavior interface
- `import`<br>Import functions from other modules

---

# Functions *do* stuff

---

### Erlang function is 1..N *clauses*

```erlang
HEADER_1 -> BODY_1;
HEADER_2 -> BODY_2;
HEADER_N -> BODY_N.
```

---

### Clauses matched top to bottom

```erlang
fib(0)            -> 0;
fib(1)            -> 1;
fib(N) when N > 1 -> fib(N-1) + fib(N-2).
```

---

## Pattern matching

```erlang
{ok, N} = {ok, 1}
N = 1
1 = 1
N = N
```

---

## Pattern UN matching #fail

```
N = 2
N = 3
2 = 3
{ok, X} = [1, 2, 3]
```

---

### What happens when<br>`N` is negative?

```erlang
fib(0)            -> 0;
fib(1)            -> 1;
fib(N) when N > 1 -> fib(N-1) + fib(N-2).
```

---

## More on *crashing* later

---

## Erlang does *value* checking

---

#### Erlang

```erlang
{ok, X} = get_value()
```

#### Typical

```python
temp = get_value()
if not is_tuple(temp): crash()
if len(temp) <> 2: crash()
if not is_atom(temp[0]): crash()
if not strcmp_atom(temp[0], "ok"): crash()
x = temp[1]
```

---

## The Internet doesn't have *type checking* but seems to work anyway

---

### Payload we don't control (dynamically typed)

```python
temp = get_json_from_internet()
if not is_dict(temp): crash()
if temp.get('status') <> 'ok': crash()
x = temp.get('value')
if x is None: crash()
```

---

### Payload we don't control (dynamically typed)

```erlang
{ok, X} = get_value()
```

---

# Function

- One of two central abstractions in Erlang (other is *process*)
- Referenceable as a term -- can be bound to variables, included in lists, etc.
- Used to implement iteration (recursion)
- Used to implement conditional branching

---

## Functional programming<br><small>(vs the world)</small>

---

### Class inheritance

```python
class Pet:
    def __init__(self, name):
        self.name = name

    def emote(self):
        print "%s says '%s'" % (self.name, self.sound)

class Dog(Pet):
    sound = "woof"

class Cat(Pet):
    sound = "meow"

class AngryCat(Cat):
    def emote(self):
        print "%s ignores your request" % self.name
```

---

### Objects

``` python
>>> sam = Dog("Sam")
>>> sam.emote()
Sam says 'woof'

>>> felix = Cat("Felix")
>>> felix.emote()
Felix says 'meow'

>>> sprinkles = AngryCat("Sprinkles")
>>> sprinkles.emote()
Sprinkles ignores your request
```

---

<img src="oops.png">

---

### So much goodness from OOP!

- Common state across classes (name and sound)
- Default implementation of a function
- Optional re-definition of a function per class

---

## How can we do this with just functions!

---

### Objects in Erlang

```erlang
1> Sam = dog("Sam"),
1> emote(Sam).
Sam says 'woof'

2> Felix = cat("Felix"),
2> emote(Felix).
Felix says 'meow'

3> Sprinkles = angry_cat("Sprinkles"),
3> emote(Sprinkles).
Sprinkles ignores your request
```

---

# But how?

---

### Higher order functions!

```erlang
emote({Name, Sound}) ->
    print("~s says '~s'", [Name, Sound]);
emote({Name, Sound, Emote}) ->
    Emote(Name, Sound).

dog(Name) -> {Name, "woof"}.

cat(Name) -> {Name, "meow"}.

angry_cat(Name) -> {Name, "", fun angry_emote/2}.

angry_emote(Name, _Sound) ->
    print("~s ignores your request", [Name]).
```

---

## OOP vs Erlang

- No classes, no inheritance
- State is decoupled from the functions that operate on that state
- Related state grouped with tuples
- Related functions may define class-like behavior
- Higher order functions enable polymorphic behavior

---

# A little cleanup

---

### Better "type" representation and separation from functions

```erlang
pet(Name, Sound) -> pet(Name, Sound, fun default_emote/2).

pet(Name, Sound, Emote) -> {Name, Sound, Emote}.

dog(Name) -> pet(Name, "woof").

cat(Name) -> pet(Name, "meow").

angry_cat(Name) -> pet(Name, "", fun angry_emote/2).

emote({Name, Sound, Emote}) -> Emote(Name, Sound).

default_emote(Name, Sound) ->
    print("~s says '~s'", [Name, Sound]).

angry_emote(Name, _Sound) ->
    print("~s ignores your request", [Name]).
```

---

# Records

---

### Even better "type" representation

```erlang
-record(pet, {name, sound, emote}).

pet(Name, Sound) -> pet(Name, Sound, fun default_emote/1).

pet(Name, Sound, Emote) ->
    #pet{name=Name, sound=Sound, emote=Emote}.

dog(Name) -> pet(Name, "woof").

cat(Name) -> pet(Name, "meow").

angry_cat(Name) -> pet(Name, "", fun angry_emote/1).

emote(#pet{emote=Emote}=Pet) -> Emote(Pet).

default_emote(#pet{name=Name, sound=Sound}) ->
    print("~s says '~s'", [Name, Sound]).

angry_emote(#pet{name=Name}) ->
    print("~s ignores your request", [Name]).
```

---

# Sequential Erlang

---

## Silly, illustrative

```erlang
one() -> 3, 2, 1.

a_case(N) ->
    case N of
        1 -> one;
        N when N == 2 -> two;
        N -> error({unknown, N})
    end.

an_if(N) ->
    if
        N == 1 -> one;
        N == 2 -> two;
        true -> error({unknown, N})
    end.

a_function(1) -> one;
a_function(2) -> two;
a_function(N) -> error({unknown, N}).
```

---

### OCDified arrows

```erlang
a_case(N) ->
    case N of
        1             -> one;
        N when N == 2 -> two;
        N             -> error({unknown, N})
    end.

an_if(N) ->
    if
        N == 1 -> one;
        N == 2 -> two;
        true   -> error({unknown, N})
    end.

a_function(1) -> one;
a_function(2) -> two;
a_function(N) -> error({unknown, N}).
```

---

## `case` vs `if` vs `function`

- case expressions should be rare
- if expressions should be extremely rare to nonexistent
- Functions are almost always better -- except for simple result mapping

---

#### `case` is fine here

```erlang
open(File) ->
    case file:open("sample") of
        {ok, F} -> File;
        {error, enoent} -> undefined
    end.
```

#### Doesn't add much

```erlang
open(File) ->
    handle_file_open(file:open(File)).

handle_file_open({ok, File}) -> File;
handle_file_open({error, enoent}) -> undefined.
```

#### Improved name helps

```erlang
open(File) ->
    file_or_undefined(file:open(File)).
```

---

### From `rebar_dir.erl`

```erlang
make_absolute_path(Path) ->
    case filename:pathtype(Path) of
        absolute ->
            Path;
        relative ->
            {ok, Dir} = file:get_cwd(),
            filename:join([Dir, Path]);
        volumerelative ->
            Volume = hd(filename:split(Path)),
            {ok, Dir} = file:get_cwd(Volume),
            filename:join([Dir, Path])
    end.
```

---

### Make it super obvious

```erlang
make_absolute_path(Path) ->
    to_abs(filename:pathtype(Path), Path).

to_abs(absolute, Path) -> Path;
to_abs(...) -> ...
```

---

### Dispatch function

```erlang
to_abs(absolute,       Path) -> Path;
to_abs(relative,       Path) -> rel_to_abs(Path);
to_abs(volumerelative, Path) -> volrel_to_abs(Path).
```

---

### More

```erlang
to_abs(absolute,       Path) -> Path;
to_abs(relative,       Path) -> rel_to_abs(Path);
to_abs(volumerelative, Path) -> volrel_to_abs(Path).

rel_to_abs(Path)    -> filename:join(cwd(), Path).
volrel_to_abs(Path) -> filename:join(vol_cwd(Path), Path).
```

---

### Everything

```erlang
make_absolute_path(Path) ->
    to_abs(filename:pathtype(Path), Path).

to_abs(absolute,       Path) -> Path;
to_abs(relative,       Path) -> rel_to_abs(Path);
to_abs(volumerelative, Path) -> volrel_to_abs(Path).

rel_to_abs(Path)    -> filename:join(cwd(), Path).
volrel_to_abs(Path) -> filename:join(vol_cwd(Path), Path).

cwd() ->
    {ok, Cwd} = file:get_cwd(),
    Cwd.

vol_cwd(Path) ->
    {ok, Cwd} = file:get_cwd(path_vol(Path)),
    Cwd.

path_vol(Path) -> hd(filename:split(Path)).
```

---

### Original still not bad

```erlang
make_absolute_path(Path) ->
    case filename:pathtype(Path) of
        absolute ->
            Path;
        relative ->
            {ok, Dir} = file:get_cwd(),
            filename:join([Dir, Path]);
        volumerelative ->
            Volume = hd(filename:split(Path)),
            {ok, Dir} = file:get_cwd(Volume),
            filename:join([Dir, Path])
    end.
```

---

### Bad code

```erlang
profile_dir(Opts, Profiles) ->
    {BaseDir, ProfilesStrings} = case [ec_cnv:to_list(P) || P <- Profiles] of
        ["global" | _] -> {?MODULE:global_cache_dir(Opts), [""]};
        ["bootstrap", "default"] -> {rebar_opts:get(Opts, base_dir, ?DEFAULT_BASE_DIR), ["default"]};
        ["default"] -> {rebar_opts:get(Opts, base_dir, ?DEFAULT_BASE_DIR), ["default"]};
        %% drop `default` from the profile dir if it's implicit and reverse order
        %%  of profiles to match order passed to `as`
        ["default"|Rest] -> {rebar_opts:get(Opts, base_dir, ?DEFAULT_BASE_DIR), Rest}
    end,
    ProfilesDir = string:join(ProfilesStrings, "+"),
    filename:join(BaseDir, ProfilesDir).
```

---

### Still bad

```erlang
profile_dir(Opts, Profiles) ->
   {BaseDir, ProfilesStrings} =
      case [ec_cnv:to_list(P) || P <- Profiles] of
         ["global" | _] ->
            {?MODULE:global_cache_dir(Opts), [""]};
         ["bootstrap", "default"] ->
            {rebar_opts:get(Opts, base_dir, ?DEFAULT_BASE_DIR),
             ["default"]};
         ["default"] ->
            {rebar_opts:get(Opts, base_dir, ?DEFAULT_BASE_DIR),
             ["default"]};
         %% drop `default` from the profile dir if it's
         %% implicity and reverse order of profiles to
         %% match order passed to `as`
         ["default"|Rest] ->
            {rebar_opts:get(Opts, base_dir, ?DEFAULT_BASE_DIR),
             Rest}
      end,
   ProfilesDir = string:join(ProfilesStrings, "+"),
   filename:join(BaseDir, ProfilesDir).
```

---

### Refactored, turns out simple

```erlang
profile_dir(Opts, Profiles) ->
    pr_dir(ensure_string_list(Profiles), Opts).

pr_dir(["global"|_],             Opts) -> global_cache_dir(Opts);
pr_dir(["bootstrap", "default"], Opts) -> default_pr_dir(Opts);
pr_dir(["default"],              Opts) -> default_pr_dir(Opts);
pr_dir(["default"|Rest],         Opts) -> plus_pr_dir(Opts, Rest).

default_pr_dir(Opts) ->
    filename:join(base_dir(Opts), "default").

plus_pr_dir(Opts, Extra) ->
    filename:join(base_dir(Opts), string:join(Extra, "+")).

base_dir(Opts) ->
    rebar_opts:get(Opts, base_dir, ?DEFAULT_BASE_DIR).

ensure_string_list(L) -> [ec_cnv:to_list(X) || X <- L].
```
---

## Summarizing functions

- Pretty much all you get in Erlang
- Don't really need much else
- Pattern matching in Erlang used for parametric dispatch (polymorphism)
- Higher order functions used for OO like extensibility
- Correy Haines *still* prefers objects, which seems wrong

---

# Process management

---

## Functions do stuff, but who *calls* the functions?

---

## Process

- One of two central abstractions in Erlang (other is *function*)
- Referenceable as a term -- can be bound to variables, included in lists, etc.
- Used to execute a function in an independent thread
- Full process semantics -- i.e. not POSIX style threads

---

## Erlang processes enable<br>system oriented programming

---

# A good system

- Runs predictably when components fail
- Runs predictably when components are added, removed, or modified
- Resiliant to higher levels of experimentation and change

---

## Processes are not POSIX style threads

- Isolated heap
- Preemptively controlled by the Erlang VM -- not user space
- Trappable exits

---

### Like OS processes

<img src="processes.png">

---

### Like an OS

<img src="load.png">

---

<img src="os.png">

---

## Working with a process

---

### A math "thing" - what is it?

```erlang
1> {ok, Math} = math:start_link(),
1> math:add(Math, 1, 2).
3
```
---

## Process <small>(cont)</small>

- Started using `spawn` or `spawn_link`
- Runs a function taking no arguments
- Can be assigned (bound) to a variable
- Can receive messages
- Can be inspected (run state, memory, message queue, etc.)

---

```
1> P = spawn(fun() -> receive _ -> ok end end).
<0.36.0>
2> process_info(P).
[{current_function,{prim_eval,'receive',2}},
 {initial_call,{erlang,apply,2}},
 {status,waiting},
 {message_queue_len,0},
 {messages,[]},
 {links,[]},
 {dictionary,[]},
 {trap_exit,false},
 {error_handler,error_handler},
 {priority,normal},
 {group_leader,<0.27.0>},
 {total_heap_size,233},
 {heap_size,233},
 {stack_size,9},
 {reductions,17},
 {garbage_collection,[{min_bin_vheap_size,46422},
                      {min_heap_size,233},
                      {fullsweep_after,65535},
                      {minor_gcs,0}]},
 {suspending,[]}]
```
---

### Two general categories of<br>Erlang process

- "Fire and forget" -- intended to perform a task and exit
- "Server" -- serve requests via message passing forever

---

### Two general categories of<br>OS process

- "Fire and forget" -- batch job
- "Server" -- daemon

---

## Client, server, messages

<img src="client-server.gif">

---

### That math thing is a "server"

```erlang
1> {ok, Math} = math:start_link(),
1> math:add(Math, 1, 2).
3
```

---

### A `math` module

```erlang
%% === Client code ===============================

start_link() -> spawn_link(fun math_loop/0).

add(Math, X, Y) ->
    Math ! {{add, X, Y}, self()},
    receive
        {math_resp, Z} -> Z
    after
        5000 -> error(timeout)
    end.

%% === Server code ===============================

math_loop() ->
    receive
        {{add, X, Y}, Caller} ->
            Caller ! {math_resp, X + Y}
    end,
    math_loop().
```
---

### A better `math` module

```erlang
%% === Client code ===============================

start_link() ->
    gen_server:start_link(?MODULE).

add(Math, X, Y) ->
    gen_server:call({add, X, Y}).

%% === Server code ===============================

handle_call({add, X, Y}, _From, State) ->
    {reply, X + Y, State}.
```

---

## gen_server

- Fancy interface for a process
- Provides important functionality for clients and servers
- Always use unless you know why you shouldn't
- Prime example of an Erlang *behavior* (module export contract)

---

## Process supervision

- Process exits can be trapped!
- A process cannot recover itself -- that's someone else's job
- Supervisor is a process that monitors and recovers another process
- Erlang isn't fault tolerant otherwise - just faulty

---

## Canonical process<br>management in Erlang

- Use `gen_server` (or derived) for your process
- Start app processes under the app supervisor
- Don't spawn/start a process directly -- use a supervisor, which was
  started under the app supervisor

---

## e2

- Wraps `gen_server`, `supervisor`, and `application` interfaces
- Way easier and saner, readable, and fun!
- Use for your Erlang projects -- unless you feel pain and suffering is
  an Erlang badge of honor (many do)

---

# Exceptions

---

## First rule of exception handling: *never handle exception*

---

### A rich tradition, thank you Java

```java
try {
   return somethingThatForcesYouToHandleException();
} catch (SomeStupidException e) {
   // No idea what to do here, but the compiler won't shut up!
   e.printStackTrace(System.err);
   return null;
}
```

---

### That Java feel, in Erlang

```erlang
try
   something_that_might_crash()
catch
   error:Err ->
       io:format("~p", [Err]),
       undefined
end
```

---

## Exceptions

- Are *exceptions* -- aka surprises, unexpected events
- *Never* handle something you don't expect or don't understand
- Again, if you expect it, it's not an exception
- Let exceptions crash the process!

---

## Error handling workflow

- By default, expect things to work and code assertively
- As you learn to expect errors, handle them
- A handled error isn't really an exception anymore -- it's part your
  program behavior/logic/design

---

<div class="step">Step 1 - assume the best!</div>

```erlang
{ok, File} = file:open("some_file")
```

<div class="step">Step 2 - IF you see a crash ENOUGH to justify a change</div>

```erlang
open_file(Path) ->
    case file:open(Path) of
        {ok,    File}   -> File;
        {error, enoent} -> find_missing_file(Path)
    end.
```

<div class="step">Step 3 - Feel guilty about step 2 and question the<br>wisdom of complicating your program!</div>

---

## This doesn't work without

- Actual process isolation<br>(guarantee of non-corrupting behavior)
- Process supervision + reliable restart semantics

---

## Erlang process isolation plus trappable exits =<br>less code + dramatically higher reliability

---

## Erlang process isolation plus trappable exits =<br>fault tolerance

---

# Erlang ecosystem

---

## Libraries

- Lots from core Erlang - start with `http://erlang.org/doc/man/`
- The rest is on Github - use Google to search
- Actually, use Hex Packages with `rebar3` - see `https://hex.pm`
- Don't forget the roll-your-own option!

---

## Reinvent that wheel!

<img src="wheels.png">

---

# Tools

---

## Compile and package

- **rebar3** Increasingly "feature filled" do-pretty-much-everything
- **erlang.mk** Increasingly large Makefile include do-almost-everything
- **relx** Creating releases

---

## Testing

- **eunit** Inspired from the world's worse test framework, every bit
    as terrible
- **common_test** Part of core, used by Ericsson, another weird framework
- **proper** Quick Check like property based testing
- **QuickCheck** Propery based testing -- limited free license + paid commercial

---

## Testing without the pain

```
-module(my_tests).

-export([run/0]).

run() ->
    test_add(),
    test_subtract().

test_add() ->
    io:format("add: "),
    0 = 0 + 0,
    1 = 0 + 1,
    io:format("OK~n").

test_subtract() ->
    io:format("subtract: "),
    0 = 0 - 0,
    1 = 1 - 0,
    io:format("OK~n").
```

---

## Erlang community

- ***First and foremost focused on solving problems that Erlang is good at solving***
- Notoriously uninterested in library fit-and-finish or coddling learners
- Extremely helpful when engaged
- Entirely unarrogant

---

## Useful links

- http://erlang.org
- http://erlang.org/doc/man/
- http://e2project.org
- http://learnyousomeerlang.com/
- http://github.com
- http://www.amazon.com/s?field-keywords=erlang

---

# Questions

<p>@gar1t on Twitter</p>
