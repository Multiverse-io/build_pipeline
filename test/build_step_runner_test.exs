defmodule BuildPipeline.BuildStepRunnerTest do
  use ExUnit.Case, async: true
  alias BuildPipeline.{BuildStepBuilder, BuildStepRunner}

  @build_step BuildStepBuilder.build("noop")

  describe "start_link/2" do
    test "starts up a build step runner process with the status 'waiting'" do
      assert {:ok, pid} = BuildStepRunner.start_link(@build_step, self())
      assert is_pid(pid)

      assert %{build_step: @build_step, server_pid: self(), status: :waiting, opts: []} ==
               :sys.get_state(pid)
    end

    test "stores optional options in the genserver state" do
      assert {:ok, pid} =
               BuildStepRunner.start_link(@build_step, self(), print_cmd_output_as_it_comes: false)

      assert is_pid(pid)

      assert %{opts: [print_cmd_output_as_it_comes: false]} = :sys.get_state(pid)
    end
  end

  describe "handle_cast/2 - run_if_able" do
    test "runs if it depends_on nothing" do
      assert_step_ran_to_completion([], [], true)
    end

    test "runs if it depends_on one thing that's done" do
      assert_step_ran_to_completion(["dependentStep"], ["dependentStep"], true)
    end

    test "does not run if it depends_on one thing that is not done" do
      assert_step_ran_to_completion(["dependentStep"], ["OtherStep"], false)
      assert_step_ran_to_completion(["dependentStep"], [], false)
    end
  end

  describe "handle_cast/2 - run" do
    test "can run shell commands" do
      build_step =
        BuildStepBuilder.build()
        |> BuildStepBuilder.with_shell_command("true")

      {:ok, pid} = BuildStepRunner.start_link(build_step, self())

      GenServer.cast(pid, :run)

      assert_receive {:"$gen_cast", {:runner_finished, ^pid, %{exit_code: 0, output: ""}}}
    end
  end

  defp assert_step_ran_to_completion(depends_on, completed_steps, ran?) do
    build_step =
      BuildStepBuilder.build()
      |> BuildStepBuilder.with_shell_command("true")
      |> BuildStepBuilder.with_depends_on(MapSet.new(depends_on))

    {:ok, pid} = BuildStepRunner.start_link(build_step, self())

    GenServer.cast(pid, {:run_if_able, MapSet.new(completed_steps)})

    if ran? do
      assert_receive {:"$gen_cast", {:runner_finished, ^pid, %{exit_code: 0, output: ""}}}
    else
      refute_receive {:"$gen_cast", {:runner_finished, ^pid, %{exit_code: 0, output: ""}}}
    end
  end
end
