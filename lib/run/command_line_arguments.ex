defmodule BuildPipeline.Run.CommandLineArguments do
  @moduledoc false
  @default_setup %{cwd: ".", mode: :normal, save_result: false, run_from_failed: false}
  @cwd "--cwd"
  @verbose "--verbose"
  @debug "--debug"
  @save_result "--sr"
  @from_failed "--ff"
  # keep usage_instructions in sync with the README.md file
  @usage_instructions """
  usage: ./bp run [--cwd ./path/to/directory/to/use] [--verbose or --debug] [--sr] [--ff]

  --verbose  - prints output from successful as well as failed build steps to the terminal. Cannot be set with --debug
  --debug    - build steps run one at a time and their output is printed to the terminal in real time. Cannot be set with --verbose. If you're scratching your head wondering what's going wrong, this flag is reccommended.
  --cwd path - the path in which to look for the build_pipeline config.json and build scripts. Defaults to "."
  --sr       - save-result: saves the results of this run to "<cwd>/previous_run_result.json"
  --ff       - from-failed: sets save-result (--sr) and also if "<cwd>/previous_run_result.json" exists, then only build steps that were either failed or not started from the previous build will run. Previously successful build steps will not be run. If no previous_run_result.json file is found then I exit and tell you I couldn't do as you asked.
  """

  def parse(setup, command_line_args) do
    if @debug in command_line_args and @verbose in command_line_args do
      {:error, {:bad_cmd_args, @usage_instructions}}
    else
      setup
      |> put_default_setup()
      |> acc_setup_from_cli_args(command_line_args)
    end
  end

  def acc_setup_from_cli_args(setup, []) do
    {:ok, setup}
  end

  def acc_setup_from_cli_args(setup, [@cwd, cwd | rest]) do
    acc_setup_from_cli_args(Map.put(setup, :cwd, cwd), rest)
  end

  def acc_setup_from_cli_args(setup, [@verbose | rest]) do
    acc_setup_from_cli_args(Map.put(setup, :mode, :verbose), rest)
  end

  def acc_setup_from_cli_args(setup, [@debug | rest]) do
    acc_setup_from_cli_args(Map.put(setup, :mode, :debug), rest)
  end

  def acc_setup_from_cli_args(setup, [@save_result | rest]) do
    acc_setup_from_cli_args(Map.put(setup, :save_result, true), rest)
  end

  def acc_setup_from_cli_args(setup, [@from_failed | rest]) do
    setup
    |> Map.put(:save_result, true)
    |> Map.put(:run_from_failed, true)
    |> acc_setup_from_cli_args(rest)
  end

  def acc_setup_from_cli_args(_step, nonsense) do
    {:error, {:bad_cmd_args, Enum.join(nonsense, " "), @usage_instructions}}
  end

  defp put_default_setup(setup) do
    Enum.reduce(@default_setup, setup, fn {key, value}, setup ->
      Map.put_new(setup, key, value)
    end)
  end
end
