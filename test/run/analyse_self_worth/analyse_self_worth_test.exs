defmodule BuildPipeline.Run.AnalyseSelfWorthTest do
  use ExUnit.Case, async: true
  import ExUnit.CaptureIO
  alias Mimic
  alias BuildPipeline.Run.Builders.RunnersBuilder
  alias BuildPipeline.Run.AnalyseSelfWorth
  alias BuildPipeline.Run.AnalyseSelfWorth.{BuildPipelineRun, SerialRun}

  describe "run/1" do
    test "when the serial & bp runs succeed, and build_pipeline made it faster, print a happy result" do
      Mimic.copy(BuildPipelineRun)
      Mimic.stub(BuildPipelineRun, :run, fn _ -> {:ok, 1} end)

      Mimic.copy(SerialRun)

      Mimic.stub(SerialRun, :run, fn _ ->
        {:ok,
         %{
           build_pipeline: [
             RunnersBuilder.build_complete() |> RunnersBuilder.with_duration_in_microseconds(1),
             RunnersBuilder.build_complete() |> RunnersBuilder.with_duration_in_microseconds(2),
             RunnersBuilder.build_complete() |> RunnersBuilder.with_duration_in_microseconds(4)
           ],
           result: :success
         }}
      end)

      output =
        capture_io(fn ->
          assert {:ok,
                  %{
                    serially_in_microseconds: 7,
                    build_pipeline_in_microseconds: 1
                  }} == AnalyseSelfWorth.run([])
        end)

      assert output =~ "I made things faster to the tune of 6 μs"
    end

    test "when the serial & bp runs succeed, and build_pipeline made it slower, print a sad result" do
      Mimic.copy(BuildPipelineRun)
      Mimic.stub(BuildPipelineRun, :run, fn _ -> {:ok, 999_999} end)

      Mimic.copy(SerialRun)

      Mimic.stub(SerialRun, :run, fn _ ->
        {:ok,
         %{
           build_pipeline: [
             RunnersBuilder.build_complete() |> RunnersBuilder.with_duration_in_microseconds(1),
             RunnersBuilder.build_complete() |> RunnersBuilder.with_duration_in_microseconds(2),
             RunnersBuilder.build_complete() |> RunnersBuilder.with_duration_in_microseconds(4)
           ],
           result: :success
         }}
      end)

      output =
        capture_io(fn ->
          assert {:ok,
                  %{
                    serially_in_microseconds: 7,
                    build_pipeline_in_microseconds: 999_999
                  }} == AnalyseSelfWorth.run([])
        end)

      assert output =~ "I made things slower by 1000 ms"
    end

    test "when the serial & bp runs succeed, and build_pipeline took just as long as running it serially, print a nuanced result" do
      Mimic.copy(BuildPipelineRun)
      Mimic.stub(BuildPipelineRun, :run, fn _ -> {:ok, 7} end)

      Mimic.copy(SerialRun)

      Mimic.stub(SerialRun, :run, fn _ ->
        {:ok,
         %{
           build_pipeline: [
             RunnersBuilder.build_complete() |> RunnersBuilder.with_duration_in_microseconds(1),
             RunnersBuilder.build_complete() |> RunnersBuilder.with_duration_in_microseconds(2),
             RunnersBuilder.build_complete() |> RunnersBuilder.with_duration_in_microseconds(4)
           ],
           result: :success
         }}
      end)

      output =
        capture_io(fn ->
          assert {:ok,
                  %{
                    serially_in_microseconds: 7,
                    build_pipeline_in_microseconds: 7
                  }} == AnalyseSelfWorth.run([])
        end)

      assert output =~ "I made no difference? \nHow unsatisfying"
    end

    test "if the build_pipeline run fails, return error" do
      Mimic.copy(BuildPipelineRun)

      Mimic.stub(BuildPipelineRun, :run, fn _ -> {:error, "bad output"} end)

      output =
        capture_io(fn ->
          assert :error == AnalyseSelfWorth.run([])
        end)

      assert output =~ "Failed to analyse self worth because the run failed:\nbad output"
    end

    test "if the serial run fails, return error" do
      Mimic.copy(BuildPipelineRun)
      Mimic.stub(BuildPipelineRun, :run, fn _ -> {:ok, 123_456} end)

      Mimic.copy(SerialRun)

      Mimic.stub(SerialRun, :run, fn _ ->
        {:ok,
         %{
           build_pipeline: [
             RunnersBuilder.build_complete() |> RunnersBuilder.with_duration_in_microseconds(1),
             RunnersBuilder.build_complete() |> RunnersBuilder.with_duration_in_microseconds(2),
             RunnersBuilder.build_complete() |> RunnersBuilder.with_duration_in_microseconds(4)
           ],
           result: :failure
         }}
      end)

      output = capture_io(fn -> assert :error == AnalyseSelfWorth.run([]) end)

      assert output =~ "Failed to analyse self worth because the run failed"
    end
  end
end
