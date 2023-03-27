defmodule BuildPipeline.Run.CommandLineArgumentsTest do
  use ExUnit.Case, async: true
  alias BuildPipeline.Run.CommandLineArguments

  @usage_instructions """
  usage: ./bp run [--cwd ./path/to/directory/to/use] [--verbose or --debug] [--ff or --ra] [--stats]

  --verbose  - prints output from successful as well as failed build steps to the terminal. Cannot be set with --debug

  --debug    - build steps run one at a time and their output is printed to the terminal in real time. Cannot be set with --verbose. If you're scratching your head wondering what's going wrong, this flag is reccommended.

  --cwd path - the path in which to look for the build_pipeline config.json and build scripts. Defaults to "."

  --ff       - from-failed: saves the results of this run to "<cwd>/previous_run_result.json", and if sed file already exists, then only build steps that were either failed or not started from the previous build will run. Previously successful build steps will not be run. Cannot be set with --ra. from-failed is smart enough to know that if all the build steps we were about to run were going to be skipped - to instead run all the steps.

  --ra       - run-all: in the event that from-failed mode is set by an environment variable, this can be used to override it and force all build steps to run (as is the default behaviour). Cannot be set with --ff

  --stats    - puts some additional output at the end of the run - showing the ranking of each dependency "branch" by speed, showing the speed of each build step within it too. Cannot be set with --debug
  """

  @bad_args_error """
                  I was given some arguments I don't understand.
                  See below for the arguments I accept

                  """ <> @usage_instructions

  @incompatible_args_error """
                           I was given some incompatible arguments.

                           --ff cannot be set with --ra
                           --debug cannot be set with --verbose
                           --debug cannot be set with --stats

                           """ <> @usage_instructions

  describe "parse/1" do
    test "given no overlapping existing setup & no command line args, puts the default setup" do
      assert {:ok,
              %{
                cwd: ".",
                mode: :normal,
                save_result: false,
                run_from_failed: false,
                a: 1,
                show_stats: false
              }} ==
               CommandLineArguments.parse(%{a: 1}, [])
    end

    test "given overlapping existing setup, the CLI args setup overrides the existing setup" do
      assert {:ok,
              %{
                cwd: ".",
                mode: :normal,
                save_result: true,
                run_from_failed: true,
                show_stats: false
              }} ==
               CommandLineArguments.parse(%{save_result: false, run_from_failed: false}, [
                 "--ff"
               ])
    end

    test "with no args returns the default setup" do
      assert {:ok,
              %{
                cwd: ".",
                mode: :normal,
                save_result: false,
                run_from_failed: false,
                show_stats: false
              }} ==
               CommandLineArguments.parse(%{}, [])
    end

    test "with --cwd set, returns it" do
      path = "./some/path"
      assert {:ok, %{cwd: ^path}} = CommandLineArguments.parse(%{}, ["--cwd", path])
    end

    test "with some unrecognised nonsense returns error with the usage instructions" do
      assert {:error, {:bad_arguments, @bad_args_error}} ==
               CommandLineArguments.parse(%{}, ["total", "nonsense"])
    end

    test "with --verbose set, sets mode to verbose" do
      assert {:ok, %{mode: :verbose}} = CommandLineArguments.parse(%{}, ["--verbose"])
    end

    test "with --debug set, sets mode to debug" do
      assert {:ok, %{mode: :debug}} = CommandLineArguments.parse(%{}, ["--debug"])
    end

    test "with --ra set, sets run_from_failed = false, even if it was true already" do
      assert {:ok, %{save_result: true, run_from_failed: false}} =
               CommandLineArguments.parse(%{save_result: true, run_from_failed: true}, ["--ra"])
    end

    test "with --debug and --verbose set, returns error & usage instructions" do
      assert {:error, {:bad_arguments, @incompatible_args_error}} =
               CommandLineArguments.parse(%{}, ["--debug", "--verbose"])

      assert {:error, {:bad_arguments, @incompatible_args_error}} =
               CommandLineArguments.parse(%{}, ["--verbose", "--debug"])

      assert {:error, {:bad_arguments, @incompatible_args_error}} =
               CommandLineArguments.parse(%{}, ["--verbose", "--debug", "--cwd", "cool/path"])
    end

    test "with --ra and --ff set, returns error & usage instructions" do
      assert {:error, {:bad_arguments, @incompatible_args_error}} =
               CommandLineArguments.parse(%{}, ["--ff", "--ra"])

      assert {:error, {:bad_arguments, @incompatible_args_error}} =
               CommandLineArguments.parse(%{}, ["--ra", "--ff"])

      assert {:error, {:bad_arguments, @incompatible_args_error}} =
               CommandLineArguments.parse(%{}, ["--ra", "--ff", "--cwd", "cool/path"])
    end

    test "with --ff from failed, returns save_result: true" do
      assert {:ok, %{save_result: true}} = CommandLineArguments.parse(%{}, ["--ff"])
    end

    test "with --stats, returns show_stats: true" do
      assert {:ok, %{show_stats: true}} = CommandLineArguments.parse(%{}, ["--stats"])
    end

    test "with --stats and --debug, returns an error because these args are incompatible" do
      assert {:error, {:bad_arguments, @incompatible_args_error}} =
               CommandLineArguments.parse(%{}, ["--debug", "--stats", "--cwd", "cool/path"])
    end
  end
end
