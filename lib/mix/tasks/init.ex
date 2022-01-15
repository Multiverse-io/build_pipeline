defmodule Mix.Tasks.BuildPipeline.Init do
  @moduledoc """
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
  Note that in the above example, I added a bash script to `scripts` which returns a non-zero exit code if todos are found anywhere in my code (except for in the README of course :) because that wouldn't work).

  Also note:
  If A depends on B which depends on C, then you only need to define A with the `dependsOn` of [B], and B with the `dependsOn` of [C].
  Saying that A `dependsOn` [B, C] is redundant. Just define A with `dependsOn` = [B].

  Once your `config.json` and any supporting scripts in `scripts` are in place, you're good to go, and you can run

  ```
  mix build_pipeline.run
  ```
  """
  @shortdoc "Run to perform first-time setup for build_pipeline"

  use Mix.Task
  alias BuildPipeline.Result

  @default_options %{cwd: "."}

  @impl Mix.Task
  def run(args) do
    args
    |> parse_args()
    |> Result.and_then(fn options -> perform_preflight_checks(options) end)
    |> Result.and_then(fn options -> create_build_pipeline_directory(options) end)
    |> Result.and_then(fn {options, directory} -> create_script_directory(options, directory) end)
    |> Result.and_then(fn {options, directory} -> copy_initial_config_over(options, directory) end)
    |> case do
      {:ok, _} ->
        :ok

      {:error, error_msg} ->
        write_error_msg(error_msg)
        :error
    end
  end

  defp create_build_pipeline_directory(options) do
    directory = Path.join(options.cwd, "build_pipeline")
    write_info("Creating #{directory}")
    :ok = File.mkdir(directory)
    {:ok, {options, directory}}
  end

  defp create_script_directory(options, directory) do
    script_directory = Path.join(directory, "scripts")
    write_info("Creating #{script_directory}")
    :ok = File.mkdir(script_directory)
    {:ok, {options, directory}}
  end

  defp copy_initial_config_over(options, directory) do
    priv_dir = :code.priv_dir(:build_pipeline)
    copy_from_path = Path.join(priv_dir, "initial_config.json")
    paste_path = Path.join(directory, "config.json")
    write_info("Creating #{paste_path}")
    File.cp(copy_from_path, paste_path)
    {:ok, {options, directory}}
  end

  defp perform_preflight_checks(options) do
    options
    |> check_dir_exists()
    |> Result.and_then(fn options -> check_not_already_initialised(options) end)
  end

  defp parse_args(args), do: parse_args(@default_options, args)

  defp parse_args(options, []) do
    {:ok, options}
  end

  defp parse_args(options, ["--cwd", cwd | rest]) do
    options
    |> Map.put(:cwd, cwd)
    |> parse_args(rest)
  end

  defp parse_args(_options, [unkown_arg | _rest]) do
    error_msg = """
    I didn't understand this argument that you gave me '#{unkown_arg}', so I'm giving up.

    Arguments I do understand:

    --cwd path/to/dir (optional)
      sets the directory in which to initialise build_pipeline. Defaults to the current directory
    """

    {:error, error_msg}
  end

  defp check_not_already_initialised(%{cwd: cwd} = options) do
    build_pipeline_dir = Path.join(cwd, "build_pipeline")

    if File.exists?(build_pipeline_dir) do
      error_msg = """
        You asked me to initialise in the directory #{cwd} but one of the directories I was about to make:
          #{build_pipeline_dir}
        already exists, so it looks like I've already been initialised, and I'm giving up
      """

      {:error, error_msg}
    else
      {:ok, options}
    end
  end

  defp check_dir_exists(%{cwd: cwd} = options) do
    if File.exists?(cwd) do
      {:ok, options}
    else
      error_msg = """
        You told me to initialise in the directory:
          #{cwd}
        but it doesn't exist so I'm giving up
      """

      {:error, error_msg}
    end
  end

  defp write_info(message) do
    IO.puts("#{IO.ANSI.magenta()}#{message}#{IO.ANSI.reset()}")
  end

  defp write_error_msg(message) do
    IO.puts("#{IO.ANSI.red()}#{message}#{IO.ANSI.reset()}")
  end
end
