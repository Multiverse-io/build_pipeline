defmodule BuildPipeline.FinalResult.BuilderTest do
  use ExUnit.Case, async: true
  alias BuildPipeline.Builders.{RunnersBuilder, ServerStateBuilder}
  alias BuildPipeline.Const
  alias BuildPipeline.FinalResult.Builder

  describe "build/3" do
    test "given server_state, a runner PID and it's exit code, builds the final result JSON" do
      runner_pid_1 = RunnersBuilder.pid()

      runner_1 =
        RunnersBuilder.build_complete()
        |> RunnersBuilder.with_exit_code(0)
        |> RunnersBuilder.with_order(0)
        |> RunnersBuilder.with_build_step_name("hi_mom")

      runner_pid_2 = RunnersBuilder.pid()

      runner_2 =
        RunnersBuilder.build_incomplete()
        |> RunnersBuilder.with_exit_code(0)
        |> RunnersBuilder.with_order(1)
        |> RunnersBuilder.with_build_step_name("hi_someone_else")
        |> RunnersBuilder.with_skip(true)

      runner_pid_3 = RunnersBuilder.pid()

      runner_3 =
        RunnersBuilder.build_complete()
        |> RunnersBuilder.with_exit_code(0)
        |> RunnersBuilder.with_order(2)
        |> RunnersBuilder.with_build_step_name("hi_dad")

      runners = %{
        runner_pid_1 => runner_1,
        runner_pid_2 => runner_2,
        runner_pid_3 => runner_3
      }

      server_state =
        ServerStateBuilder.build()
        |> ServerStateBuilder.with_runners(runners)

      json = Builder.build(server_state, runner_pid_3, 0)

      assert Jason.decode!(json) == [
               %{
                 "buildStepName" => "hi_mom",
                 "result" => Const.successful()
               },
               %{
                 "buildStepName" => "hi_someone_else",
                 "result" => Const.skipped()
               },
               %{
                 "buildStepName" => "hi_dad",
                 "result" => Const.successful()
               }
             ]
    end
  end
end
