defmodule BuildPipeline.Run.CommandLineArguments do
  @moduledoc false
  @default_setup %{
    cwd: ".",
    mode: :normal,
    save_result: false,
    run_from_failed: false,
    show_stats: false,
    json_report: false,
    halt_when_done: true
  }
  @cwd "--cwd"
  @verbose "--verbose"
  @debug "--debug"
  @from_failed "--ff"
  @run_all "--ra"
  @stats "--stats"
  @json_report "--json-report"
  @analyse_self_worth "--analyse-self-worth"
  # keep usage_instructions in sync with the README.md file

  def usage_instructions do
    """
    usage: ./bp run [--cwd ./path/to/directory/to/use] [--verbose or --debug] [--ff or --ra] [--stats] [--json-report]

    --verbose  - prints output from successful as well as failed build steps to the terminal. Cannot be set with --debug

    --debug    - build steps run one at a time and their output is printed to the terminal in real time. Cannot be set with --verbose. If you're scratching your head wondering what's going wrong, this flag is reccommended.

    --cwd path - the path in which to look for the `build_pipeline` directory which must contain `config.json` and build `scripts` folder. Defaults to "."

    --ff       - from-failed: saves the results of this run to "{your cwd}/build_pipeline/previous_run_result.json", and if sed file already exists, then only build steps that were either failed or not started from the previous build will run. Previously successful build steps will not be run. Cannot be set with --ra. from-failed is smart enough to know that if all the build steps we were about to run were going to be skipped - to instead run all the steps.

    --ra       - run-all: in the event that from-failed mode is set by an environment variable, this can be used to override it and force all build steps to run (as is the default behaviour). Cannot be set with --ff

    --stats    - puts some additional output at the end of the run - showing the ranking of each dependency "branch" by speed, showing the speed of each build step within it too. Cannot be set with --debug

    --json-report - creates a json report including all the steps run, their timings and whether they succeeded or not. Cannot be set with --debug

    --analyse-self-worth    - Runs the full build pipeline twice. Once with full parallism including build_pipeine overhead, and once serially without build_pipeline overhead. Reports the timings of both. Useful for finding out how much time (if any) is saved by running your build with build_pipeline. Doesn't work unless `bp` is in your PATH! Basically every other command line argument except --cwd is incompatible with this one.
    """
  end

  def incompatible_args_error do
    """
    I was given some incompatible arguments.

    --ff cannot be set with --ra
    --debug cannot be set with --verbose
    --debug cannot be set with --stats

    --analyse-self-worth cannot be set with any of the following
       --verbose
       --debug
       --ff
       --ra
       --stats
       --json-report
    """
  end

  def bad_args_error do
    """
     I was given some arguments I don't understand.
     See below for the arguments I accept

    """
  end

  def parse(setup, command_line_args) do
    if incompatible_args?(command_line_args) do
      {:error, {:bad_arguments, incompatible_args_error() <> usage_instructions()}}
    else
      setup_from_cli_args(setup, command_line_args)
    end
  end

  defp setup_from_cli_args(setup, command_line_args) do
    case setup |> put_default_setup() |> acc_setup_from_cli_args(command_line_args) do
      {:ok, setup} -> {:ok, setup}
      :error -> {:error, {:bad_arguments, bad_args_error() <> usage_instructions()}}
    end
  end

  defp acc_setup_from_cli_args(setup, command_line_args) do
    case analyse_self_worth(command_line_args) do
      {true, command_line_args} ->
        setup
        |> Map.put(:mode, {:analyse_self_worth, command_line_args})
        |> Map.put(:save_result, false)
        |> Map.put(:run_from_failed, false)
        |> do_acc_setup_from_cli_args(command_line_args)

      {false, command_line_args} ->
        do_acc_setup_from_cli_args(setup, command_line_args)
    end
  end

  defp analyse_self_worth(command_line_args) do
    if Enum.member?(command_line_args, @analyse_self_worth) do
      {true, command_line_args -- [@analyse_self_worth]}
    else
      {false, command_line_args}
    end
  end

  defp do_acc_setup_from_cli_args(setup, []) do
    {:ok, setup}
  end

  defp do_acc_setup_from_cli_args(setup, [@cwd, cwd | rest]) do
    do_acc_setup_from_cli_args(Map.put(setup, :cwd, cwd), rest)
  end

  defp do_acc_setup_from_cli_args(setup, [cli_arg | rest]) do
    case put_setup_from_singular_cli_arg(setup, cli_arg) do
      {:ok, setup} -> do_acc_setup_from_cli_args(setup, rest)
      error -> error
    end
  end

  defp put_setup_from_singular_cli_arg(setup, @verbose) do
    {:ok, Map.put(setup, :mode, :verbose)}
  end

  defp put_setup_from_singular_cli_arg(setup, @debug) do
    {:ok, Map.put(setup, :mode, :debug)}
  end

  defp put_setup_from_singular_cli_arg(setup, @run_all) do
    {:ok, Map.put(setup, :run_from_failed, false)}
  end

  defp put_setup_from_singular_cli_arg(setup, @stats) do
    {:ok, Map.put(setup, :show_stats, true)}
  end

  defp put_setup_from_singular_cli_arg(setup, @json_report) do
    {:ok, Map.put(setup, :json_report, true)}
  end

  defp put_setup_from_singular_cli_arg(setup, @from_failed) do
    setup =
      setup
      |> Map.put(:save_result, true)
      |> Map.put(:run_from_failed, true)

    {:ok, setup}
  end

  defp put_setup_from_singular_cli_arg(_setup, _bad) do
    :error
  end

  @incompatible_args %{
    @debug => [@verbose, @stats, @json_report],
    @run_all => [@from_failed],
    @analyse_self_worth => [@verbose, @debug, @from_failed, @run_all, @stats, @json_report]
  }

  defp incompatible_args?(command_line_args) do
    @incompatible_args
    |> Enum.filter(fn {arg, _incompatible} ->
      arg in command_line_args
    end)
    |> Enum.flat_map(fn {_arg, incompatible} -> incompatible end)
    |> Enum.any?(fn arg ->
      arg in command_line_args
    end)
  end

  defp put_default_setup(setup) do
    Enum.reduce(@default_setup, setup, fn {key, value}, setup ->
      Map.put_new(setup, key, value)
    end)
  end
end
