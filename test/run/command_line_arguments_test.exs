defmodule BuildPipeline.Run.CommandLineArgumentsTest do
  use ExUnit.Case, async: true
  alias BuildPipeline.Run.CommandLineArguments

  @usage_instructions CommandLineArguments.usage_instructions()

  @bad_args_error CommandLineArguments.bad_args_error() <> @usage_instructions

  @incompatible_args_error CommandLineArguments.incompatible_args_error() <> @usage_instructions

  describe "parse/1" do
    test "given no overlapping existing setup & no command line args, puts the default setup" do
      assert {:ok,
              %{
                cwd: ".",
                mode: :normal,
                save_result: false,
                run_from_failed: false,
                a: 1,
                show_stats: false,
                json_report: false,
                halt_when_done: true
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
                show_stats: false,
                json_report: false,
                halt_when_done: true
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
                show_stats: false,
                json_report: false,
                halt_when_done: true
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

    test "with --json-report, returns json_report: true" do
      assert {:ok, %{json_report: true}} = CommandLineArguments.parse(%{}, ["--json-report"])
    end

    test "with --stats and --debug, returns an error because these args are incompatible" do
      assert {:error, {:bad_arguments, @incompatible_args_error}} =
               CommandLineArguments.parse(%{}, ["--debug", "--stats", "--cwd", "cool/path"])
    end

    test "with --json-report and --debug, returns an error because these args are incompatible" do
      assert {:error, {:bad_arguments, @incompatible_args_error}} =
               CommandLineArguments.parse(%{}, ["--debug", "--json-report", "--cwd", "cool/path"])
    end

    test "with --analyse-self-worth, returns it as the mode" do
      assert {:ok,
              %{
                mode: {:analyse_self_worth, ["--cwd", "some/dir"]}
              }} = CommandLineArguments.parse(%{}, ["--cwd", "some/dir", "--analyse-self-worth"])
    end

    test "with --analyse-self-worth, some other args are not compatible" do
      assert {:error, {:bad_arguments, @incompatible_args_error}} ==
               CommandLineArguments.parse(%{}, ["--analyse-self-worth", "--verbose"])

      assert {:error, {:bad_arguments, @incompatible_args_error}} ==
               CommandLineArguments.parse(%{}, ["--analyse-self-worth", "--debug"])

      assert {:error, {:bad_arguments, @incompatible_args_error}} ==
               CommandLineArguments.parse(%{}, ["--analyse-self-worth", "--ff"])

      assert {:error, {:bad_arguments, @incompatible_args_error}} ==
               CommandLineArguments.parse(%{}, ["--analyse-self-worth", "--ra"])

      assert {:error, {:bad_arguments, @incompatible_args_error}} ==
               CommandLineArguments.parse(%{}, ["--analyse-self-worth", "--stats"])

      assert {:error, {:bad_arguments, @incompatible_args_error}} ==
               CommandLineArguments.parse(%{}, ["--analyse-self-worth", "--json-report"])
    end

    test "with --analyse-self-worth set, save_result & run_from_failed get set to faluse even if previously true" do
      assert {:ok, %{save_result: false, run_from_failed: false}} =
               CommandLineArguments.parse(%{save_result: true, run_from_failed: true}, [
                 "--analyse-self-worth"
               ])
    end
  end
end
