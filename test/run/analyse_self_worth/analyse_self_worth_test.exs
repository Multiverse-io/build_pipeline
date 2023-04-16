defmodule BuildPipeline.Run.AnalyseSelfWorthTest do
  use ExUnit.Case, async: true
  alias Mimic
  alias BuildPipeline.Run.Builders.RunnersBuilder
  alias BuildPipeline.Run.AnalyseSelfWorth
  alias BuildPipeline.Run.AnalyseSelfWorth.{BuildPipelineRun, SerialRun}

  describe "run/1" do
    test "when the serial & bp runs succeed, add up the serial step runs & return the build_pipeline runtime" do
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
           result: :success
         }}
      end)

      assert {:ok,
              %{
                serially_in_microseconds: 7,
                build_pipeline_in_microseconds: 123_456
              }} == AnalyseSelfWorth.run([])
    end

    # TODO put an error msg to the user if this happens
    test "if the build_pipeline run fails, return error" do
      Mimic.copy(BuildPipelineRun)
      Mimic.stub(BuildPipelineRun, :run, fn _ -> :error end)
      Mimic.copy(SerialRun)

      assert :error == AnalyseSelfWorth.run([])
    end

    # TODO put an error msg to the user if this happens
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

      assert :error == AnalyseSelfWorth.run([])
    end
  end
end
