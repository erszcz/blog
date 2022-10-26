The two basic mechanisms of set theoretic types are type unions and type intersections.
We’re quite used to unions from working with type specs - for example,
they allow us to spec functions like the following:

```
def area({:circle, radius}), do: :math.pi * radius * radius
def area({:square, side}), do: side * side
```

One way to spec such a function would be:

```
@type shape() :: {:circle, number()}
               | {:square, number()}

@spec area(shape()) :: number()
```

`shape()` is an example of a union type.
We can see this spec is accurate, because of both function clauses returning the same result type - also a `number()`.
What about this, though?

```
def double(l) when is_list(l), do: Enum.map(l, & (2*&1))
def double(n), do: 2 * n
```

One way to spec `double/1` is:

```
@spec double(number() | [number()]) :: number() | [number()]
```

but we see that this spec is imprecise - it’s not true that we can get a `[number()]` for a `number()` argument.
Here’s where intersection types come into play:

```
@spec double([number()]) :: [number()]
@spec double(number()) :: number()
```

The spec has two clauses, resembling clauses of the function - if we pass in a list, we’ll get a list,
if we pass in a number, we’ll get a number.
In general, an intersection type means that something (here a function) might have more than one type at the same time.
Intersection types are the perfect mechanism to model ad-hoc polymorhpism aka function overloading,
which is ubiquitous in Elixir and Erlang.

Back to the question:

> how set theoretic types would differ from more common forms of static typing

Since Elixir and Erlang don’t have the notion of top-level type
constructors - the “variant names” ADTs are built of in OCaml,
Haskell, but also Gleam - nothing like that exists in the Elixir/Erlang syntax - unions
and intersections are the best (only?) mechanism to achieve similar end results.

Of course, it’s not just functions that can have intersection types,
but type checking overloaded functions is the first and most reasonable candidate to use them for.
