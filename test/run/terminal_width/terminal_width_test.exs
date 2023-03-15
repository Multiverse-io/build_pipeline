defmodule BuildPipeline.Run.TerminalWidthTest do
  use ExUnit.Case, async: true
  use Mimic
  alias BuildPipeline.Run.TerminalWidth
  alias BuildPipeline.Run.TerminalWidth.TputCols

  alias BuildPipeline.Run.Mocks.TputCols.{
    NotOnSystem,
    GoodResult,
    ResultUnexpectedError,
    DecimalResult,
    NonsenseResult
  }

  @setup %{setup: {}}

  describe "append_to_setup/0" do
    test "returns an error if the tput command isn't on the system" do
      Mimic.copy(TputCols)
      Mimic.stub(TputCols, :run, &NotOnSystem.run/0)

      assert {:error, {:terminal_width, :tput_not_on_system}} ==
               TerminalWidth.append_to_setup(@setup)
    end

    test "when tput runs and returns a result with an exit code of 0, we return OK and the number of columns on the terminal" do
      Mimic.copy(TputCols)
      Mimic.stub(TputCols, :run, &GoodResult.run/0)

      terminal_width = GoodResult.cols()

      assert {:ok, %{setup: %{terminal_width: ^terminal_width}}} =
               TerminalWidth.append_to_setup(%{setup: @setup})
    end

    test "when tput runs and returns an unexpected failure, we return error" do
      Mimic.copy(TputCols)
      Mimic.stub(TputCols, :run, &ResultUnexpectedError.run/0)

      assert {:error, {:terminal_width, :unexpected_tput_result, ResultUnexpectedError.output()}} ==
               TerminalWidth.append_to_setup(@setup)
    end

    test "when tput gives a unparseable decimal result" do
      Mimic.copy(TputCols)
      Mimic.stub(TputCols, :run, &DecimalResult.run/0)

      assert {:error, {:terminal_width, :unexpected_tput_result, DecimalResult.output()}} ==
               TerminalWidth.append_to_setup(@setup)
    end

    test "when tput gives a unparseable nonesense result" do
      Mimic.copy(TputCols)
      Mimic.stub(TputCols, :run, &NonsenseResult.run/0)

      assert {:error, {:terminal_width, :unexpected_tput_result, NonsenseResult.output()}} ==
               TerminalWidth.append_to_setup(@setup)
    end
  end
end
