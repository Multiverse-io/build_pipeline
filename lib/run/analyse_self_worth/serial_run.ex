defmodule BuildPipeline.Run.AnalyseSelfWorth.SerialRun do
  alias BuildPipeline.Run

  def run(command_line_args) do
    Run.main(command_line_args, fn setup ->
      setup
      |> put_in([:setup, :halt_when_done], false)
      |> put_in([:setup, :mode], :debug)
    end)
  end
end
