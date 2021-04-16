# Make illegal states unrepresentable - but how? The Typestate Pattern in Erlang

_Making illegal states unrepresentable_ is one of the practices
[Yaron Minsky from Jane Street preached in his Effective ML lectures][effective-ml]
on programming in OCaml some 10 years ago.
Could this technique be applied in Erlang,
which is known for being notoriously hard to typecheck?
Let's find out!

Minsky's original example shows how to refactor a C-style "bag of things and an enum"
representation of a state machine:

```ocaml
type connection_state =
  | Connecting
  | Connected
  | Disconnected

type connection_info = {
  state: connection_state;
  server: Inet_addr.t;
  last_ping_time: Time.t option;
  last_ping_id: int option;
  session_id: string option;
  when_initiated: Time.t option;
  when_disconnected: Time.t option;
}
```

To a set of algebraic data types, which prevent misuse of fields in states
in which these fields are not defined and should not be accessible:

```ocaml
type connecting = { when_initiated: Time.t; }
type connected = { last_ping : (Time.t * int) option;
                   session_id: string; }
type disconnected = { when_disconnected: Time.t; }

type connection_state =
  | Connecting of connecting
  | Connected of connected
  | Disconnected of disconnected

type connection_info = {
  state : connection_state;
  server: Inet_addr.t;
}
```

I recommend watching the original video by following the link above
to get a better hold of this technique - it's worth it!

Fast forward a few years and it turns out that an improved version of this technique
which uses generic types to encode valid transitions of the state machine
is known as...


## The Typestate Pattern

> The typestate pattern is an approach to designing data types
> and APIs that is quite popular in the Haskell and Rust ecosystem.

That's how [Timo Freiberg introduces the typestate pattern in his blog post][typestate-blog-1]
featured in [the 383rd edition of This Week in Rust][this-week-in-rust-383].
The post starts with an example that's roughly equivalent to the one of Minsky.
Later, though, Freiberg elaborates how structuring the data impacts
the code that operates on this data.
The "bag of things" approach requires multiple conditions in the state machine implementation,
which increase code complexity and cognitive overload.

[The second of Freiberg's posts describes the typestate pattern][typestate-blog-2] and refactoring
of the "bag of things" data representation.
We see on concrete examples not only how the _illegal states are made unrepresentable_,
but also how _the illegal transitions are made impossible_,
thanks to the static type system with generic type support.

The clarity of the state-transitioning functions and the ability to directly access the relevant
state fields without conditional checks make the code so much simpler and clearer!
This declarative description of the data reminds me of a quote from the
introduction to _Algorithms + Data Structures = Programs_, the classic by Niklaus Wirth:

> [...] one has an intuitive feeling that data precede algorithms:
> you must have some objects before you can perform operations on them.

This description of data is something I always missed a bit when working in Erlang,
especially in my early days of digging through ejabberd code
(which later evolved to become [MongooseIM](https://github.com/esl/mongooseim)).
With some experience came the realisation that nothing beats a well placed `io:format/2`,
or a file with [`dbg` snippets][docsh-snippets] at hand if rebuilding is not an option,
to get a hold of the actual structure of data that I'm dealing with.

Due to some interest in static typing,
I set off to experiment with applying the typestate pattern in Erlang
and with tools available in the BEAM ecosystem,
which could reinforce its application:
[Dialyzer](http://erlang.org/doc/apps/dialyzer/dialyzer_chapter.html)
and [Gradualizer](https://github.com/josefs/Gradualizer).
Is it even possible?

[effective-ml]: https://blog.janestreet.com/effective-ml-video/
[typestate-blog-1]: https://www.novatec-gmbh.de/en/blog/the-case-for-the-typestate-pattern-introducing-algebraic-data-types/
[typestate-blog-2]: https://www.novatec-gmbh.de/en/blog/the-case-for-the-typestate-pattern-the-typestate-pattern-itself/
[this-week-in-rust-383]: https://this-week-in-rust.org/blog/2021/03/24/this-week-in-rust-383/
[docsh-snippets]: https://github.com/erszcz/docsh/blob/b6d917e75270cd3220300fc00583e4d3cfa8b460/snippets.erl


## The Typestate Pattern in Erlang

In order to assess Dialyzer and Gradualizer feasibility for the task,
I won't consider the "bag of things" representation outlined by Timo and
jump straight to translating his typestate aware Rust code:

```rust
#[derive(Debug)]
pub struct RepairOrder<State> {
    pub order_number: u64,
    pub damage_description: Option<String>,
    pub vehicle: String,
    pub customer: Customer,
    pub state: State
}
struct New;
struct Valid;
struct Invalid { validation_errors: Vec<String> }
struct InProgress {
    assigned_technician: Employee,
    steps_left: Vec<String>,
}
struct WorkDone;
struct WaitingForPayment { invoice: String }
struct Paid { invoice: String }
```

For the sake of brevity, I omit or adjust some type annotations in the
Erlang version below:

```erlang
-module(repair_order).

-record(?MODULE, {order_number,
                  damage_description,
                  vehicle,
                  customer,
                  state}).
-type t(State) :: #?MODULE{state :: State}.

-type new() :: new.
-type valid() :: valid.
-type invalid() :: {invalid, [string()]}.
-record(in_progress, {assigned_technician,
                      steps_left = []}).
-type in_progress() :: #in_progress{}.
-type work_done() :: work_done.
-type waiting_for_payment() :: waiting_for_payment.
-type paid() :: {paid, non_neg_integer()}.
```

Some might argue it's a lot of typing for no gain, but is it really?
Let's draw the conclusions once we run the typecheckers later.
At this point, though, it's worth taking note that unlike in Rust,
we only have to provide essential type annotations - the rest of the code
can stay untyped or typed in a very permissive fashion.

Timo's Rust `validate`:

```rust
impl RepairOrder<New> {
    fn validate(self) -> Result<RepairOrder<Valid>, RepairOrder<Invalid>> {
        let is_valid = is_valid();
        if is_valid {
            Ok(self.with_state(Valid))
        } else {
            let validation_errors = get_validation_errors();
            Err(self.with_state(Invalid { validation_errors }))
        }
    }
}
```

Gets translated to Erlang (in order to typecheck the code I also stub `is_valid/1`):

```erlang
-spec validate(t(new())) -> {ok, t(valid())} |  {error, t(invalid())}.
validate(Order) ->
    case is_valid(Order) of
        true ->
            {ok, #?MODULE{state = valid}};
        false ->
            {error, #?MODULE{state = {invalid, ["order is not valid"]}}}
    end.

is_valid(_) -> false.
```

With this Erlang code in place, let's consider the following misuse of the
`validate/1` function:

```erlang
-spec invalid_use() -> {ok, t(valid())} |  {error, t(invalid())}.
invalid_use() ->
    validate(#?MODULE{state = {invalid, []}}).
```

What does Dialyzer tell us about the code above?

```
$ r3 dialyzer
===> Verifying dependencies...
===> Analyzing applications...
===> Compiling demos
===> Dialyzer starting, this may take a while...
===> Updating plt...
===> Resolving files...
===> Checking 204 files in _build/default/rebar3_24.0-rc2_plt...
===> Doing success typing analysis...
===> Resolving files...
===> Analyzing 3 files with _build/default/rebar3_24.0-rc2_plt...

src/repair_order.erl
Line 28 Column 9: The pattern 'true' can never match the type 'false'
===> Warnings written to _build/default/24.0-rc2.dialyzer_warnings
===> Warnings occurred running dialyzer: 1
```

Hmmm... not much. The only warning it returns points out that our
`is_valid/1` stub won't ever return anything else than `false`.

Let's try Gradualizer:

```
$ gradualizer src/repair_order.erl
src/repair_order.erl: The tuple on line 39 at column 31 is expected to have type 'new' but it has type '{invalid, []}'

-spec invalid_use() -> {ok, t(valid())} |  {error, t(invalid())}.
invalid_use() ->
    validate(#?MODULE{state = {invalid, []}}).
                              ^^^^^^^^^^^^^
```

What do we have there?
Although, `#?MODULE{state = {invalid, []}}` in general is a valid instance of `repair_order:t()`,
it's not allowed as an argument to `validate/1`!
Its spec tells us that only new repair orders can be validated:

```erlang
-spec validate(t(new())) -> ...
```

Let's consider another example - what if we make a mistake in `validate/1` itself?

```erlang
-spec validate(t(new())) -> {ok, t(valid())} | {error, t(invalid())}.
validate(Order) ->
    case is_valid(Order) of
        true ->
            {ok, #?MODULE{state = valid}};
        false ->
            {error, #?MODULE{state = invalid}}
    end.
```

Dialyzer isn't helpful in this situation either, so I'll skip its output.
Let's see what Gradualizer gives us:

```
$ gradualizer src/repair_order.erl
src/repair_order.erl: The tuple on line 31 at column 13 is expected to have type '{ok, t(valid())} | {error, t(invalid())}' but it has type '{error, #repair_order{}}'

            {ok, #?MODULE{state = valid}};
        false ->
            {error, #?MODULE{state = invalid}}
            ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
```

In this situation the message is not as clear as the previous time, but
[it's not that bad either](TODO: camomilla_release_gcc_before.png).
The `error` tuple tag tells us to look closer at `#?MODULE{state = invalid}` - isn't
it an instance of `t(invalid())`?
Well, no, as upon checking again we realise that `invalid()` is defined as:

```erlang
-type invalid() :: {invalid, [string()]}.
```

All in all, in exchange for defining the states of the state machine as
distinct types we get the benefits of:

1. Being able to encode valid transitions in function specifications
1. Compile time verification of the state record validity

Moreover, we don't really have to type everything!
We can pick and choose which fields are important to be typed,
since the transitions depend on them, and which are not.
The more type annotations we provide in the code, though,
the higher level of confidence in its correctness we get at compile time,
and the more information we pass on to people reading it later.
Ultimately, we can just run the code as usual, since it's still ordinary Erlang,
and typechecking it is completely separate from compiling it.

Some of you might ask, though, "what if I make a mistake in my types or specs?"
Well, obviously, the typechecker won't be able to catch errors based on
invalid specifications.
This situation, however, is similar to designing models for property-based
testing - how likely is it that we make the same mistake both in the model and
the implementation?
Similarly, how likely is it that we make the same mistake in the
type specification and the code it describes?
In the latter case, though, we don't have to run the tests to get
correctness test results.
And we don't even have to write the (property-based) tests!

The BEAM community might grow more accustomed to techniques such as the
typestate pattern as new languages such as [Caramel](https://caramel.run/)
and [Gleam](https://gleam.run/) rise in popularity.
Not every project can be rewritten from scratch, though,
so it's worth keeping in mind that thanks to projects like Gradualizer
it's possible to apply some techniques known in the broader functional
programming community to improve Erlang or Elixir code in an evolutionary,
or _gradual_, rather than revolutionary fashion.

Don't take my word for it - clone https://github.com/erszcz/demos and run
the typecheckers on `src/repair_order.erl` yourself.

PS 1. If you think I'm biased towards Gradualizer or dislike Dialyzer,
then you'd better check out this
[blog post by Rafał Studnicki and Simon Żelazny on typing Elixir](https://well-ironed.com/articles/a-more-type-driven-elixir-workflow-maybe/)
which contains some code examples Gradualizer cannot handle at all, but Dialyzer does very well.

PS 2. Gradualizer is still an experimental project and when trying out the snippets for this blog post,
I've run into [a small bug which crashed the typechecker](https://github.com/josefs/Gradualizer/pull/327).
Hopefully, the fix is merged before this post gets published :)
