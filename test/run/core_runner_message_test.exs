defmodule BuildPipeline.Run.CoreRunnerMessageTest do
  use ExUnit.Case, async: true
  alias BuildPipeline.Run.CoreRunnerMessage
  alias BuildPipeline.Run.Builders.RunnersBuilder

  describe "create/1" do
    test "puts the env vars in front of the command" do
      runner_1 =
        RunnersBuilder.build_incomplete()
        |> RunnersBuilder.with_command("echo walk over")
        |> RunnersBuilder.with_order(0)

      runner_2 =
        RunnersBuilder.build_incomplete()
        |> RunnersBuilder.with_command("echo hello")
        |> RunnersBuilder.with_command_env_vars([{"MIX_ENV", "test"}, {"COOL", "ENV"}])
        |> RunnersBuilder.with_order(1)

      assert CoreRunnerMessage.create(runner_1) == "echo walk over"
      assert CoreRunnerMessage.create(runner_2) == "MIX_ENV=test COOL=ENV echo hello"
    end
  end
end
