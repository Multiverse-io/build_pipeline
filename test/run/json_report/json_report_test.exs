defmodule BuildPipeline.Run.JsonReportTest do
  use ExUnit.Case, async: true
  use Mimic
  alias BuildPipeline.Run.Builders.ServerStateBuilder
  alias BuildPipeline.Run.JsonReport
  alias BuildPipeline.Run.JsonReport.FileWriter

  describe "generate/1" do
    test "with json report enabled, generates a json file" do
      Mimic.copy(FileWriter)
      Mimic.expect(FileWriter, :write, 1, fn report ->
        assert Jason.decode!(report)
      end)

      state =
        ServerStateBuilder.build()
        |> ServerStateBuilder.with_json_report(true)

      JsonReport.generate(state)

      Mimic.verify!()
    end

    test "with json report disabled, doesn't generate a json file" do
      Mimic.copy(FileWriter)
      Mimic.reject(&FileWriter.write/1)

      state =
        ServerStateBuilder.build()
        |> ServerStateBuilder.with_json_report(false)

      JsonReport.generate(state)

      Mimic.verify!()
    end
  end
end
