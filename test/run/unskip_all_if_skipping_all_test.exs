defmodule BuildPipeline.Run.UnskipAllIfSkippingAllTest do
  use ExUnit.Case, async: true
  alias BuildPipeline.Run.UnskipAllIfSkippingAll
  alias BuildPipeline.Run.Builders.{RunnersBuilder, ServerSetupBuilder}

  describe "parse/1" do
    test "doesn't change anything if there's a build step which isn't being skipped" do
      runner_1 =
        RunnersBuilder.build_incomplete()
        |> RunnersBuilder.with_skip(false)

      runner_2 =
        RunnersBuilder.build_incomplete()
        |> RunnersBuilder.with_skip(true)

      setup =
        ServerSetupBuilder.build()
        |> ServerSetupBuilder.with_build_pipeline([runner_1, runner_2])
        |> ServerSetupBuilder.with_run_from_failed(true)

      assert {:ok, setup} == UnskipAllIfSkippingAll.parse(setup)
    end

    test "if all steps are being skipped and we're in run_from_failed mode, then unskip them all!" do
      runner_1 =
        RunnersBuilder.build_incomplete()
        |> RunnersBuilder.with_skip(true)

      runner_2 =
        RunnersBuilder.build_incomplete()
        |> RunnersBuilder.with_skip(true)

      setup =
        ServerSetupBuilder.build()
        |> ServerSetupBuilder.with_build_pipeline([runner_1, runner_2])
        |> ServerSetupBuilder.with_run_from_failed(true)

      assert {:ok,
              %{
                build_pipeline: [%{skip: false}, %{skip: false}]
              }} = UnskipAllIfSkippingAll.parse(setup)
    end

    test "if all steps are being skipped and we're NOT in run_from_failed mode, then don't change anything" do
      runner_1 =
        RunnersBuilder.build_incomplete()
        |> RunnersBuilder.with_skip(true)

      runner_2 =
        RunnersBuilder.build_incomplete()
        |> RunnersBuilder.with_skip(true)

      setup =
        ServerSetupBuilder.build()
        |> ServerSetupBuilder.with_build_pipeline([runner_1, runner_2])
        |> ServerSetupBuilder.with_run_from_failed(false)

      assert {:ok, setup} == UnskipAllIfSkippingAll.parse(setup)
    end
  end
end
