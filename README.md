# BuildPipeline

A development tool for running commands with maximum possible concurrency,
designed to speed up CI / CD build piplines by running mulitple independent build steps at once

## Up and Running

First, add build_pipeline as a dependency to your project
Then (from the root of your projects directory, where your `mix.exs` file is) run:
```
mix build_pipeline.init
```

This will generate some directories and a default `config.json` file, like so:
```
build_pipeline
├── config.json
└── scripts
```



## Installation

If [available in Hex](https://hex.pm/docs/publish), the package can be installed
by adding `build_pipeline` to your list of dependencies in `mix.exs`:

```elixir
def deps do
  [
    {:build_pipeline, "~> 0.1.0"}
  ]
end
```

Documentation can be generated with [ExDoc](https://github.com/elixir-lang/ex_doc)
and published on [HexDocs](https://hexdocs.pm). Once published, the docs can
be found at [https://hexdocs.pm/build_pipeline](https://hexdocs.pm/build_pipeline).

