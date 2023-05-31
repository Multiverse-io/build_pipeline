defmodule BuildPipeline.Run.JsonReport do
  alias BuildPipeline.Run.JsonReport.Generator

  @output_filename "build_report.json"

  def generate(%{json_report: true} = state) do
    state.runners
    |> Generator.generate()
    |> write_file()
  end

  def generate(_), do: nil

  defp write_file(report) do
    File.write!(@output_filename, Jason.encode!(report, pretty: true))
  end
end
