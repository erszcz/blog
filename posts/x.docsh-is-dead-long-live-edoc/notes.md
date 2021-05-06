# Docsh is dead, long live EDoc!

Radek Szymczyszyn <lavrin@gmail.com>
2021-03-25

If you're looking for a how-to on using EDoc to generate doc chunks,
see [Generating Erlang doc chunks with EDoc][edoc-chunks-howto].
This is longer piece describing the road from an early prototype to the
final implementation landing in Erlang/OTP.

[edoc-chunks-howto]:

In January of 2016 I started work on an idea that had been lingering in my head.

# 2021-03-29

Erlang's REPL is probably the most advanced of its kind across modern programming languages.
It not only allows to execute Erlang code, but also to attach to live systems, and inspect,
tune and modify them - even production ones, which sometimes makes devops cringe.
It's more like a shell of an operating system than a simple read-eval-print loop.

However, in spite of its maturity and capabilities,
up until recently Erlang lacked a feature present in the REPLs of other,
sometimes much younger, languages - access to documentation, or even function specifications,
directly from the shell.
This seemed like an area which could significantly improve the developer and operator experience.
This talk will describe docsh - a take at prototyping such a feature.
Then, it will proceed with how José Valim helped make it into EEP-48 to ensure interop with other BEAM languages,
and finally how the idea landed in Erlang versions 23 and the upcoming 24, thanks to mine and Lukas Larsson's work.

<!--
vim: spell
-->

<!--

# The Outline

## Let's start with a quiz

- ets:lookup(Tab, Key) or ets:lookup(Key, Tab)  ## Tab, Key
- gb_trees:lookup(Tab, Key) or gb_trees:lookup(Key, Tab)  ## Key, Tab

## Motivation

Having already worked for approx. 5 years at Erlang Solutions,
I realised that some issues we face in commercial projects are pretty common.

Some of these issues are the nature of the job - when jumping from project to project,
it's obvious that we, as individual engineers, don't know a customer's code base from the get go.
That's something we have to pick up as a project progresses.

Some of them, though, are symptomatic:
- projects seldom have documentation
- tooling fragmentation (erlang.mk, rebar2, and rebar3 used in the same project!)
- missing tests for critical functionality or a complete lack of tests

These could be improved by specific language idioms, better and more uniform tooling,
or better community conventions of using specs and typechecking.

And I'm not the first person realising that.
In 2014 I had a chance to participate in Erlang User Conference in Stockholm and
listen to Garrett Smith's talk "Why the Cool Kids don't use Erlang?"
(https://www.youtube.com/watch?v=3MvKLOecT1I)

Garrett has conducted a survey in the Erlang community and pinpointed some
difficulties on the path to adopting Erlang as an individual and as an organization:

> HardToLearn
> FindingDevelopers
> Adoption
> ManagerBuyIn
> Documentation
> LibraryConsistency

Fred Hebert, the author of "Learn You Some Erlang" and the indispensable Recon troubleshooting toolbox,
commented in his EUC 2016 talk "How Things Are, and How They Could Be"
(https://codesync.global/media/how-things-are-and-how-they-could-be-fred-hebert-euc16/)
on the Erlang developer and operator experience being far from just sunshine and roses.

In 2020 on Code BEAM V, Anton Lavrik from WhatsApp confirmed Fred's findings in
"From 10s to 1000s of engineers: Scaling Erlang Developer Experience at WhatsApp"
(https://codesync.global/uploads/media/activity_slides/0001/03/f2292f201aa6b04db8c4e0b9cfa191dd07c9ee14.pdf):

> Developer productivity for larger teams becomes critical, not merely important.
> lack of well integrated tooling: IDE integration, formatter, build system, ...

Yes, Erlang has features which still make it seem like a tech from the future:

- a world-class runtime capable of utilising all the power of modern multicore CPUs
- network transparency and built-in distribution
- fault-tolerance and resiliency baked into core of the language and runtime
- opt-in typechecking thanks to Dialyzer (and recently also Gradualizer)
- three distinct property based testing libraries,
- a operating system like shell capable of inspecting and tuning almost
  everything in the system, live, even in production!

But at the same time there are things which make it look like a relic of
the past and resemble C more than competitive
very high level languages like Python, Ruby, Go or Elixir:

- an obsolete preprocessor, macro and header file system
- no batteries included in the standard library
  (command line parsing? JSON parsing? exponential backoff for connection management?)
- evolving core, but lagging behind ecosystem (no pretty-printer capable
  of outputting everything the compiler consumes, no modern doc engine
  with support for all language features)

Some of the pain points have been alleviated by community efforts:

- Hex.pm - a package manager for Elixir which also supports Erlang packages
- Rebar3 - a modern build tool capable of resolving dependencies
- ErlangLS - a language server, that is the Erlang brain for editors and IDEs

Back in 2016, though, only the first talk, the one by Garrett Smith, was already published.
Elixir was gaining some traction, though, and solving some of Erlang's shortcomings,
but it was not very popular yet.

Some keywords are repeated in all three of the above talks: documentation,
developer experience, learning curve.
If we add to that the necessity to jump from project to project as a consultant,
to pick up new tools on the way, dive into new code bases all the time,
it becomes obvious that it would just be so much easier and better if we could do it with less friction.

Another thing which lead me to the idea of docsh were the memories of my
baby steps in programming, when I was still a teenager, trying out some
snippets of C and Python - and the simple

```
>>> help(dict)
Help on class dict in module __builtin__:

class dict(object)
 |  dict() -> new empty dictionary
 |  dict(mapping) -> new dictionary initialized from a mapping object's
 |      (key, value) pairs
 |  dict(iterable) -> new dictionary initialized as if via:
 |      d = {}
 |      for k, v in iterable:
 |          d[k] = v
 |  dict(**kwargs) -> new dictionary initialized with the name=value pairs
 |      in the keyword argument list.  For example:  dict(one=1, two=2)
 |
 |  Methods defined here:
```

in the Python console.

I started wondering what it would take to extend the Erlang shell with
on-line documentation support and it seemed to me that all the necessary
parts are there - they just need to be put together.
How hard could it be?


## docsh


### Rationale

- manpages not always present, manpages not available for non-OTP libraries
- EDoc docs look outdated
- a lot of community libraries don't have inline docs
- even if they do, EDoc often crashes on them :(

Why write docs, if they can't be easily accessed?


### Consistency and discoverability

- ets:lookup(Tab, Key) or ets:lookup(Key, Tab)  ## Tab, Key
- gb_trees:lookup(Tab, Key) or gb_trees:lookup(Key, Tab)  ## Key, Tab

Elixir has done a tremendous effort on the front of consistency in their standard library,
but not every line of Erlang can be rewritten in Elixir.

And not every line should: RabbitMQ, Couchbase, MongooseIM, proprietary software (Ericsson?)

But can we help with discoverability?


### Fragmentation

- out of source AsciiDoc (Cowboy - https://github.com/ninenines/cowboy/tree/master/doc/src/manual)
- out of source Markdown with EDown (Exometer - https://github.com/Feuerlabs/exometer/tree/master/doc)
    - EDown extends EDoc by converting from Markdown to EDoc
- some projects adopt in-source EDoc (Recon)
- some projects rely on 3rd party tools or services (MongooseIM and Read the Docs) to document things
  that EDoc should be capable of documenting
- Elixir libs vs Erlang libs on Hex.pm

So how do I document my project properly?


### Elixir libs vs Erlang libs on Hex.pm

Phoenix vs Cowboy - which looks better?

https://hexdocs.pm/phoenix/overview.html
https://hexdocs.pm/cowboy

But there would be no Phoenix without Cowboy!

This is not specific to Cowboy, see also Recon on hexdocs.pm

https://hexdocs.pm/recon/index.html


### Goals

If an Erlang projects has docs - we should to be able to read them in the Erlang shell.

If an Erlang project doesn't have docs - we should at least see function signatures,
argument names and types, to be able to infer their meaning.

We should be able to see a list of types exported by a module and the type definitions.


### docsh control flow - first approach

docsh as a tool in the build process:

- parsing source files with EDoc
- getting specs from abstract code (debug_info): the old and the new chunk
- abstract readers - merging info from source files and debug_info to an
  intermediate format
- converting to the final format - copied from Elixir
- rewriting the .beam files with the new doc chunk
- embedding code necessary to pretty print the docs stored in the new chunk - rewriting the code chunk

### KISS - keep it simple, stupid!

docsh revisited to work only in runtime:

```
> h(recon).
> h(recon_trace, calls).
> t(recon_trace).
```
 
What happens when we type the above?

- check if the .beam file has the doc chunk; if yes, go to "look up"
- if not, find the source file
- look up the `Module` or `{Module, Kind, Arity}` in the chunk
- format the found name, spec / type, and documentation in the shell

### 2016-01-21 First docsh commits

- getting consistent formatting (and testing it!)

### Challenges

- no pretty-printer capable of formatting the Erlang AST to something the
  compiler can then compile again

- Erlang tarballs used to contain precompiled .beam files,
  which had hardcoded paths pointing at locations valid on the build host,
  but not necessarily valid on localhost

- these hardcoded paths pointed at `/net` which lead `filelib:is_regular/1` to take up to 20 seconds
  on accessing the paths;
  this resulted in unacceptable delays when reading docs for OTP modules:
  https://twitter.com/erszcz/status/969241080549072897
  https://github.com/erszcz/docsh/pull/19


### 2017-05-30 Erlang docs in Elixir thanks to an IEx hook

https://github.com/erszcz/docsh/blob/docsh-iex/notes.md#2017-05-30-hooking-into-iex
https://github.com/erszcz/elixir/commit/ea4389ff78789644c62783cda16dff63385e3d3f


### 2017-06-16 Email thread on a Docs chunk for BEAM languages - later to become EEP-48


### 2017-08-10 docsh comes with install.sh

2017-04-15 started with install_SUITE tests (4b9413c)
2017-08-10 last commit


### 2017-11-08 docsh can be installed via Kerl

2017-11-08 kerl PR merged

Per Erlang installation

https://github.com/erszcz/docsh/issues/12


### 2018-01-13 EEP-48 published


### 2018-01-17 Elixir 1.6.0 with spec help for non-Elixir code is released


### 2018-07-20 Erlang/Elixir interop thanks to EEP-48 implementation on both sides

2018-07-20 Rebar3 issue opened

https://github.com/erlang/rebar3/issues/1842


### 2019-01-21 docsh integration with Rebar3 shell


### 2019-05-17 José talks with Kenneth and they agree to extend EDoc with doc chunks


### 2019-10-28 ErlangLS starts using docsh for doc retrieval

Roberto Aloi's Erlang Language Server starts using docsh.

You might be using docsh without knowing it ;)


### 2020-05-13 Erlang 23.0 ships with shell_docs

Lukas Larsson implements shell_docs,
which extends the Erlang shell with features prototyped in docsh:

- help for a module: h(proplists)
- help for a function: h(proplists, get_value)
- all types exported from a module: ht(proplists)
- type definition: h(proplists, property)

Erlang/OTP documentation is now available as EEP-48 doc chunks.

But only OTP documentation - community projects still have no way to
provide docs for shell_docs.


### docsh - unsolved problems

- problems when switching between Erlang versions (stale .beam files) -
  solved by installation with kerl, but...

- kerl is nice, but asdf is nicer - it provides support for multiple languages / environments;
  docsh support was never enabled in asdf (even though it uses kerl internally)

- neither were manpages available with asdf out of the box :(


## EDoc


### Erlang Ecosystem Foundation

The Foundation comes to life on Feb 14th 2019, thanks to
Francesco Cesarini, Kenneth Lundin, Fred Hebert, Jose Valim, and others.

The work on chunk support in EDoc is organized under the Documentation Working Group.


### EDoc before OTP 24.0

- generates docs only as static HTML files
- some Erlang language features are newer than EDoc and therefore not
  supported:
    - no support for callbacks (no callback specs or doc comments)
    - subpar support for -spec / -type attributes
- crashes even on OTP libs
- accepts arbitrary HTML (tables, scripts, maybe even the blink tag)


### EDoc after OTP 24.0

- generates static HTML docs
- generates EEP-48 doc chunks to be consumed by shell_docs and other tools
- rudimentary support for callbacks (specs and comments, but not special
  tags like @since or @deprecated yet)
- some limited subset of HTML is still accepted,
  but other tags are skipped - just raw text is extracted
- known issues - https://github.com/erlef/build-and-packaging-wg/issues/25
    - works cleanly on top 25+ Erlang Hex.pm projects!
    - might still crash on some OTP libs if macros are undefined
      (OTP build system specific issue)


### Doc chunks for your Erlang project - demo (1/2)

```
git clone https://github.com/ferd/recon
cd recon
cat >> rebar.config <<EOF
{edoc_opts, [{doclet, edoc_doclet_chunks},
             {layout, edoc_layout_chunks},
             {preprocess, true},
             {dir, "_build/docs/lib/recon/doc"}]}.
EOF
rebar3 compile
rebar3 edoc
```


### Doc chunks for your Erlang project - demo (2/2)

```
$ rebar3 as docs shell
1> h(recon_trace, calls).

  -spec calls(TSpecs :: tspec() | [tspec(), ...], Max :: max()) ->
                 num_matches().

  Equivalent to calls({Mod, Fun, Args}, Max, []).

  -spec calls(TSpecs :: tspec() | [tspec(), ...],
              Max :: max(),
              Opts :: options()) ->
                 num_matches().

  Allows to set trace patterns and pid specifications to trace function calls.

  ...
```


### WIP: ExDoc + EDoc = pretty Erlang docs 1/3

Thanks to Wojtek Mach for his work on ExDoc Erlang support!

https://hexdocs.pm/recon

vs

https://erszcz.github.io/recon


### WIP: ExDoc + EDoc = pretty Erlang docs 2/3

https://hexdocs.pm/brod

vs

https://erszcz.github.io/brod


### WIP: ExDoc + EDoc = pretty Erlang docs 3/3

https://hexdocs.pm/iex/IEx.html

vs

https://erszcz.github.io/stdlib/shell.html

(only a few modules rendered for the preview)


### WIP: ExDoc + EDoc = pretty Erlang docs - demo 1/2


```
git clone https://github.com/erszcz/ex_doc -b erlang  # still some glitches in main ExDoc
cd ex_doc
mix deps.get
mix compile
mix escript.build  # this produces the ex_doc executable
```


### WIP: ExDoc + EDoc = pretty Erlang docs - demo 2/2


```
cd recon
/path/to/ex_doc Recon "2.5.1" _build/docs/lib/recon/ebin/ --main recon -o docs --proglang erlang
```

Now open recon/docs/index.html in the browser :)


## Challenges

- high coupling of EDoc and erl_docgen - easy to break OTP doc build process,
  fail CI tests, etc

- backwards compatibility with existing projects using EDoc-the-markup: both in OTP
  and in the community

- EDoc means three things:
    - the wiki-like markup we use to document code
    - the app processing this markup and generating HTML / chunks
    - the intermediate XML-based format passed between the core app and the
      doclets / layouts which output documentation

- converting Erlang AST fragments to / from the EDoc XML format is a terrible PITA;
  moreover, some data required by EEP-48 is simply not available in this XML format;
  I had to break the convention that doclets use this format to be able to
  generate chunks

- the EDoc wiki markup used to accept HTML, but it's now limited to a
  certain tag subset - this is not backward compatible with some projects :|


## Next steps

https://github.com/erlef/build-and-packaging-wg/issues/25

- tighter integration with Rebar3 and ExDoc - make pretty HTML docs easy
  to generate

- there are ideas about a new proposal on inline docs via -doc and -moduledoc
  attributes instead of doc comments - not published yet - uncertain if it would be accepted by OTP


## Final notes

EDoc, the Erlang documentation engine, hits version 1.0 with this release, which means a few changes.

First and foremost, EDoc is now capable of emitting EEP-48 doc chunks. This means that, with some configuration, community projects can now provide documentation for shell_docs the same way that OTP libraries did since OTP 23.0. Fancy your favourite library having docs in the shell? Consider contributing - the configuration steps needed to make it happen are minimal. Of course, this opens up a path for other tools to make use of the community project doc chunks, like language servers, editors, static site generators etc.

Second, a further step is taken in deprecating the @spec and @type EDoc tags. These are not supported with the new chunk-generating doclet and layout. Moreover, previously when there was a redundant @spec tag and -spec attribute defined for the same function, the @spec tag would take precedence. Now, the -spec attribute takes precedence and is more important. The same is true for redundant @type and -type attributes. Warnings are now emitted when such redundant entries are found. Certainly, though, the benefit of generating chunks for your project will outweigh the effort of moving from @spec and @type tags to -spec and -type attributes.

Adding doc chunk support to EDoc was carried out by Radek Szymczyszyn, partially sponsored by Erlang Solutions, and coordinated within the Erlang Ecosystem Foundation Documentation Working Group. Special thanks are due to Lukas Larsson for shell_docs and his tireless reviews, Jose Valim for the initial encouragement, Wojtek Mach for a proof of concept implementation, and Kenneth Lundin for supporting the whole effort.

-->
