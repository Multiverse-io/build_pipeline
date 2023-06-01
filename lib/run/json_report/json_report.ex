defmodule BuildPipeline.Run.JsonReport do
  alias BuildPipeline.Run.JsonReport.{FileWriter, Generator}

  def generate(%{json_report: true} = state) do
    state.runners
    |> Generator.generate()
    |> Jason.encode!(pretty: true)
    |> FileWriter.write()
  end

  def generate(_), do: nil
end
