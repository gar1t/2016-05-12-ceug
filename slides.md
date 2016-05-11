# Topics

- Sequential Erlang (the language)
- Process management
- Libraries
- Build, test, deploy
- NOT distribution

---

# Motivation

---

## Why Erlang?

- Concurrency
- Ecosystem
- Simple, functional (immutable)

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

### Import to know!

- Lower case names are *atoms*<br>`red, cat, ok`
- Upper case names are *variables*<br>`Color, Pet, Status`
- Square brackets are *lists*<br>`[Color, 123, "Sam"]`
- Curley brackets are *tuples*<br>`{car, {"Dodge", "Dart"}, 1988}`
- Function calls look like this<br>`math:add(1, 2)`

---

## Building blocks

- **Module**<br>One erl source file, compiles to beam
- **Application**<br>Bundle of beams + app metadata
- **Release**<br>Bundle of applications + boot script
- **escripts** Shell interface

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

### Module *attributes* give the compiler extra information

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
- A term -- i.e. can be bound to variables, included in lists, etc.
- Used to implement iteration (recursion)
- Used to implement conditional branching

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

## So much goodness from OO!

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

# So clean, but how?

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

## OO vs Erlang

- No classes, no inheritance
- State is decoupled from the functions that operate on that state
- Related state grouped with tuples
- Related functions may define class-like behavior
- Higher order functions expose

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
