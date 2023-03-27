# Build Pipeline

A development tool for running commands with maximum possible concurrency,
designed to speed up CI / CD build piplines by running mulitple independent build steps at once.

Commands will _only_ be executed if the commands that it `dependsOn` have run successfully first.

If commands don't depend on anything, or if all of their dependent `command`s have already run successfully, then they'll be run together concurrently.

## Installation and Getting Up and Running

### Dependencies
- `erlang` https://www.erlang.org/
- `tput` must be runnable on your system. This is used to work out the width of your terminal to enable fancy command line output

### Installation

- wget https://raw.githubusercontent.com/mbernerslee/build_pipeline/master/bp
- chmod +x bp

OR

- git clone this project
- [additionally have elixir installed]
- copy (or better would be to symlink) ./bp to the directory of your choice

Then, From the root of your projects' directory run:
```
./bp init
```

Run this as first-time setup -

This will generate some directories and a default `config.json` file, like so:
```
build_pipeline
├── config.json
└── scripts
```
Next, edit your `config.json` file, adding the desired build steps.
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
    "buildStepName": "fetch_dependencies",
    "commandType": "shellCommand",
    "command": "echo 'a real command to fetch project dependencies'",
    "dependsOn": []
  },
  {
    "buildStepName": "compile",
    "commandType": "shellCommand",
    "command": "echo 'a real compile code command!'",
    "dependsOn": ["fetch_dependencies"],
    "envVars": [
      {
        "name": "COMPILE_ENV_VAR_EXAMPLE_KEY",
        "value": "COMPILE_ENV_VAR_EXAMPLE_VALUE"
      }
    ]
  },
  {
    "buildStepName": "test",
    "commandType": "shellCommand",
    "command": "echo 'the tests for my project ran really fast. wow!'",
    "dependsOn": ["compile"]
  }
]


```
In the above example, I have a file: `build_pipline/scripts/find_todos`.
It's a bash script which returns a non-zero exit code if "TODO" is found anywhere in the code.

- `find_todos` will run concurrently with `fetch_dependencies`, since they both depend on nothing
- as soon as `fetch_dependencies` succeeds, `compile` will run
- as soon as `compile` succeeds, `test` will run
- if all commands exit with an exit code of 0, then this run was successful!

If A `dependsOn` B which depends on C, then you only need to define A with the `dependsOn` of [B], and B with the `dependsOn` of [C].
Saying that A `dependsOn` [B, C] is redundant. Just define A with `dependsOn` = [B]. If A fails, B will not run. If B fails, C will not run.

Once your `config.json` and any supporting scripts in `scripts` are in place, you're good to go, and you can run

```
./bp run
```

And you're away!

By default, _output from successful commands are silenced_, and `command` output is only displayed by the first command that fails (returns a non 0 exit code). In the event of a command failing, subsequent dependent commands and commands in progress are gracefully not started or terminated respectively.


## Reccomendations on how to run build_pipeline on CI vs localy

### On CI
On CI it is reccommended to run either

`./bp run`
or
`./bp run --verbose`

Without `--verbose` (as mentioned earlier) _output from successful commands are silenced_. If you don't like that then...

with `--verbose`, output of successful commands _are_ printed to the terminal, but be advised that output from successful commands are only printed when they are finished, not in real-time.

### Locally
Locally it is reccommended to put
`export BUILD_PIPELINE_FROM_FAILED=true`
in your ~/.bashrc, ~/.zshrc, or whatever you use

such that `./bp run` will always run in run-from-failed mode (as if you're always running `./bp run --ff`)

Note that if the env var `BUILD_PIPELINE_FROM_FAILED=true` is set, it can easily be overriden as a one-off by running it with run all `--ra` set: `./bp run --ra`.


## ./bp run - Options
### Command Line Arguments
`--verbose`  - prints output from successful as well as failed build steps to the terminal. Cannot be set with --debug

`--debug`    - build steps run one at a time and their output is printed to the terminal in real time. Cannot be set with --verbose. If you're scratching your head wondering what's going wrong, this flag is reccommended.

`--cwd path` - the path in which to look for the build_pipeline config.json and build scripts. Defaults to "."

`--ff`       - from-failed: saves the results of this run to "<cwd>/previous_run_result.json", and if sed file already exists, then only build steps that were either failed or not started from the previous build will run. Previously successful build steps will not be run. Cannot be set with --ra. from-failed is smart enough to know that if all the build steps we were about to run were going to be skipped - to instead run all the steps.

`--ra`       - run-all: in the event that from-failed mode is set by an environment variable, this can be used to override it and force all build steps to run (as is the default behaviour). Cannot be set with --ff

`--stats`    - puts some additional output at the end of the run - showing the ranking of each dependency "branch" by speed, showing the speed of each build step within it too. Cannot be set with --debug

### Enviroment Variables
Some `./bp run` options be set by enviroment variables.

In the case of an option being set by both by a command line argument and an environment variable - the command line argument takes precdent.

In other words `BUILD_PIPELINE_FROM_FAILED=false ./bp run --ff` _will_ set run-from-failed mode

`BUILD_PIPELINE_FROM_FAILED=true|false` - the same as setting the command line argument `--ff` (see above)
