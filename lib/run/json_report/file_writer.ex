defmodule BuildPipeline.Run.JsonReport.FileWriter do
  @output_filename "build_report.json"

  def write(report) do
    File.write!(@output_filename, report)
  end
end
