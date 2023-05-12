# Build Pipeline

A development tool for running commands with maximum possible concurrency,
designed to speed up CI / CD build piplines by running mulitple independent build steps at once.

Commands will _only_ be executed if the commands that it `dependsOn` have run successfully first.

If commands don't depend on anything, or if all of their dependent `command`s have already run successfully, then they'll be run together concurrently.

## Installation and Getting Up and Running

### Dependencies

- `erlang/OTP 25` https://www.erlang.org/
- `tput` must be runnable on your system. This is used to work out the width of your terminal to enable fancy command line output

## Creating releases

1. Checkout the lastest main branch on your machine
2. Update the `version` function in `lib/build_pipeline.ex` to return `1.2.3` (without a preceeding v!)
3. Create a git tag with the new version number `git tag v1.2.3`
4. Push the tag `git push origin v1.2.3`
5. Wait for CircleCI to create a draft release
6. Review the draft release and publish

### Installation


- We recommend using asdf to install and manage build_pipeline versions using https://github.com/Multiverse-io/asdf-build_pipeline
- once you've installed the build_pipeline asdf plugin you can run  `asdf install build_pipeline <version-number e.g. 0.0.11>`, and you're good to go

OR

- browse to https://github.com/Multiverse-io/build_pipeline/releases/latest
- use the link to download `bp`
- copy or symlink `bp` to the root of your project

Then, from the root of your projects' directory run:

```
bp init
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
Saying that A `dependsOn` [B, C] is redundant. Just define A with `dependsOn` = [B]. If A fails, B will not run and therefore C will not run either. If B runs and fails, C will not run.

Once your `config.json` and any supporting scripts in `scripts` are in place, you're good to go, and you can run

```
bp run
```

If installed via asdf, or

```
./bp run
```

And you're away!

By default, _output from successful commands are silenced_, and `command` output is only displayed by the first command that fails (returns a non 0 exit code). In the event of a command failing, subsequent dependent commands and commands in progress are gracefully not started or terminated respectively.

Once you're confident your build is running successfully as it should, I reccommend adding `bp` to your PATH and running `bp run --analyse-self-worth`, which will tell you how much faster the build is with `bp` vs just running each build step one at a time. See the section about this below for more details.

## Reccomendations on how to run build_pipeline on CI vs localy

### On CI

To set up build_pipeline with CI you'll need to do something like this

```
  build_pipeline_version="$(cat .tool-versions | awk '/build_pipeline/{print $2}')"
  build_pipeline_release_url="https://github.com/Multiverse-io/build_pipeline/releases/download/v$build_pipeline_version/bp"
  echo "Downloading from $build_pipeline_release_url"
  curl -fLo bp $build_pipeline_release_url
  chmod +x bp
  ./bp --version
  mv bp /usr/local/bin/
```

The above assumes you're using asdf in your project to define the version of build_pipeline that you're using, and therefore have a `.tool-versions` file in your project that contains the line `build_pipeline <version-number>`.

If you're not then replace the first line with a different command specifying which version you're after.

Obviously it's up to your judgement as to whether the additional time spent on setting up build_pipeline justifies it's use to save time on CI overall! (You could cache bp between CI runs if you wanted to of course)

Then once bp is ready to go it's reccommended to run either

`bp run`
or
`bp run --verbose`

Without `--verbose` (as mentioned earlier) _output from successful commands are silenced_. If you don't like that then...

with `--verbose`, output of successful commands _are_ printed to the terminal, but be advised that output from successful commands are only printed when they are finished, not in real-time.

### Locally

Locally it is reccommended to put
`export BUILD_PIPELINE_FROM_FAILED=true`
in your ~/.bashrc, ~/.zshrc, or whatever you use

such that `bp run` will always run in run-from-failed mode (as if you're always running `bp run --ff`)

Note that if the env var `BUILD_PIPELINE_FROM_FAILED=true` is set, it can easily be overriden as a one-off by running it with run all `--ra` set: `bp run --ra`.

## bp run - Options

### Command Line Arguments

<!-- keep this in sync with the usage instrucitons in lib/run/command_line_arguments.ex -->

`--verbose` - prints output from successful as well as failed build steps to the terminal. Cannot be set with --debug

`--debug` - build steps run one at a time and their output is printed to the terminal in real time. Cannot be set with --verbose. If you're scratching your head wondering what's going wrong, this flag is reccommended.

`--cwd path` - the path in which to look for the `build_pipeline` directory which must contain `config.json` and build `scripts` folder. Defaults to "."

`--ff` - from-failed: saves the results of this run to "{your cwd}/build_pipeline/previous_run_result.json", and if sed file already exists, then only build steps that were either failed or not started from the previous build will run. Previously successful build steps will not be run. Cannot be set with --ra. from-failed is smart enough to know that if all the build steps we were about to run were going to be skipped - to instead run all the steps.

`--ra` - run-all: in the event that from-failed mode is set by an environment variable, this can be used to override it and force all build steps to run (as is the default behaviour). Cannot be set with --ff

`--stats` - puts some additional output at the end of the run - showing the ranking of each dependency "branch" by speed, showing the speed of each build step within it too. See below for more info. Cannot be set with --debug

`--analyse-self-worth` - Runs the full build pipeline twice. Once with full parallelism including build_pipeine overhead, and once serially without build_pipeline overhead. Reports the timings of both. Useful for finding out how much time (if any) is saved by running your build with build_pipeline. Doesn't work unless `bp` is in your PATH! See below section for more details.

### Analyse Self Worth

If requested via the `--analyse-self-worth` flag, then build_pipeline attempts to answer the question

_how much time does build_pipeline save vs. just running the build steps one at a time?_

It requires `bp` to be in your PATH, and it runs all build steps twice.

1) With build_pipeline parallelism as defined in the config file
2) Without build_pipeline parallelism; running each step one at a time.

It times how long 1) and 2) take, and outputs how long each took.

Note that to time 1), we include the time it takes to startup `bp` itself. This is achieved by the initial `bp` instance actually starting up a 2nd `bp` instance to do a full build_pipeline run (this is why `bp` needs to be in your PATH, so that it knows where to find itself).

For 2) we still run the steps with `bp`, but we run each build step one at a time & report only the sum of the time it took run each build step. We don't include additional time added by `bp` overhead by doing this in an attempt to be as honest as we can!

Hopefully you'll see a message saying how much parallelism sped up your build!

... otherwise erm... that's awkward. I guess uninstall? :-(

At the time of writing this, running `bp run --analyse-self-worth`, for build_pipeline's own build (how meta) outputs the following

```
*********************************************************************
Self Worth Analysis
*********************************************************************

build_pipeline runtime = 5.6 s
serial runtime = 9.9 s

I made things faster to the tune of 4.3 s !
Self worth affirmed!
*********************************************************************
```

So build_pipeline has self justification to exist at least!

### Statistics

If requested via the `--stats` flag, statistics from the run will be put on the screen at the expense of a small amount of extra time being spent at the end.

Imagine build steps such that

```
A ── B ── C ── D ── E ── F
          └─── H ── I ── J
```

In other words:

- A depends on [] (nothing)
- B depends on ["A"]
- C depends on ["B"]
- D depends on ["C"]
- E depends on ["D"]
- F depends on ["E"]
- H depends on ["C"]
- I depends on ["H"]
- J depends on ["I"]

This can be viewed as having two "branches" of execution with a "branch point" at step "C".

The if passing `--stats` with this setup then we'd return statistcs as shown below.
We know that steps [D, E, F] and [H, I, J] will run in parallel, and that each branch waits for [A, B, C] to be complete before it can start.

The slowest branches are shown first, and every branch begins at a "root" - a build step with no dependencies, which is "A" in both our branches in this example. The branch shown at the top of the output will always be the critical path to building our pipeline - the slowest limiting factor, and should be the first place to look if trying to speed things up.

```
******************
*** Statistics ***
******************

Branch 1 - 455 ms
├── A [1 ms]
├── B [2 ms]
├── C [4 ms]
├── H [64 ms]
├── I [128 ms]
└── J [256 ms]

Branch 2 - 63 ms
├── A [1 ms]
├── B [2 ms]
├── C [4 ms]
├── D [8 ms]
├── E [16 ms]
└── F [32 ms]

******************
```

### Enviroment Variables

Some `bp run` options be set by enviroment variables.

In the case of an option being set by both by a command line argument and an environment variable - the command line argument takes precdent.

In other words `BUILD_PIPELINE_FROM_FAILED=false bp run --ff` _will_ set run-from-failed mode

`BUILD_PIPELINE_FROM_FAILED=true|false` - the same as setting the command line argument `--ff` (see above)
