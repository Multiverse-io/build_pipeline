# Build Pipeline

An elixir development tool for running commands with maximum possible concurrency,
designed to speed up CI / CD build piplines by running mulitple independent build steps at once.

Commands will _only_ be executed if the commands that it `dependsOn` have run successfully first.

If commands don't depend on anything, or if all of their dependent `command`s have already run successfully, then they'll be run together concurrently.

## Installation and Getting Up and Running

### Dependencies
- `tput` must be runnable on your system. This is used to work out the width of your terminal to enable fancy command line output

### Installation

This package can be installed by adding `build_pipeline` to your list of dependencies in `mix.exs`:

```elixir
def deps do
  [
    {:build_pipeline, "~> 0.2.0"}
  ]
end
```

Then, From the root of your projects' directory (where your `mix.exs` file is) run:
```
mix build_pipeline.init
```

Run this as first-time setup -

This will generate some directories and a default `config.json` file, like so:
```
build_pipeline
├── config.json
└── scripts
```
Next, edit your `config.json` file, adding the desired build steps.<br>
`config.json` must be only a list, containing `buildSteps`.

Build steps are defined as in the example below.
- `buildStepName` (mandatory) - a name for this build step
- `commandType` (mandatory) - `script` or `shellCommand`
- `command` (mandatory) - either a file name of a script in the `build_pipline/scripts` folder, or a shell command to run
- `dependsOn` (mandatory) - a list of other `buildStepName`s, which must run first before this step is run
- `envVars` (optional) - a list of extra environment variables to be set when the `command` is run

```
[
  {
    "buildStepName": "find_todos",
    "commandType": "script",
    "command": "find_todos",
    "dependsOn": []
  },
  {
    "buildStepName": "deps.get",
    "commandType": "shellCommand",
    "command": "mix deps.get",
    "dependsOn": []
  },
  {
    "buildStepName": "compile",
    "commandType": "shellCommand",
    "command": "mix compile --force --warnings-as-errors",
    "dependsOn": [
      "deps.get"
    ],
    "envVars": [
      {
        "name": "MIX_ENV",
        "value": "test"
      }
    ]
  },
  {
    "buildStepName": "loadconfig",
    "commandType": "shellCommand",
    "command": "mix loadconfig config/prod.exs",
    "dependsOn": []
  },
  {
    "buildStepName": "test",
    "commandType": "shellCommand",
    "command": "mix test --color",
    "dependsOn": [
      "compile"
    ]
  },
  {
    "buildStepName": "esciptBuild",
    "commandType": "shellCommand",
    "command": "mix escript.build",
    "dependsOn": [
      "test"
    ],
    "envVars": [
      {
        "name": "MIX_ENV",
        "value": "prod"
      }
    ]
  }
]


```
The above example is what is used to build build_pipeline itself.
Note that there is a bash script in the  `scripts` folder which returns a non-zero exit code if "TODO" is found anywhere in the codebase (except for in the README of course :) because that wouldn't work).

Also note:
If A depends on B which depends on C, then you only need to define A with the `dependsOn` of [B], and B with the `dependsOn` of [C].
Saying that A `dependsOn` [B, C] is redundant. Just define A with `dependsOn` = [B].

Once your `config.json` and any supporting scripts in `scripts` are in place, you're good to go, and you can run

```
mix build_pipeline.run
```

And you're away!

By default, _output from successful commands are silenced_, and `command` output is only displayed by the first command that fails (returns a non 0 exit code). In the event of a command failing, subsequent dependent commands and commands in progress are gracefully not started or terminated respectively.

## mix build_pipeline.run - Options
- `--cwd [path/to/a/place]` (Optional) - the path in which to search for the build_pipeline directory. Defaults to "."
- `--verbose` (Optional) - Show the output of successful as well as unsuccessul commands
- `--debug` (Optional) - Removes build step concurrency: run commands one at a time & shows command output in realtime. Useful for er.. debugging & checking if any commands are sneakily asking for CLI input.
Only --verbose or --debug can be set

