defmodule BuildPipeline.Run.WhichBuildStepsCanRunTest do
  use ExUnit.Case, async: true
  alias BuildPipeline.Run.WhichBuildStepsCanRun
  alias BuildPipeline.Run.Builders.{ServerStateBuilder, RunnersBuilder}

  describe "determine_pids/1 - mode = normal / verbose" do
    test "incomplete runners with no dependencies are returned" do
      for mode <- [:verbose, :normal] do
        runner_pid_1 = RunnersBuilder.pid()
        runner_pid_2 = RunnersBuilder.pid()

        runners = %{
          runner_pid_1 =>
            RunnersBuilder.build_incomplete()
            |> RunnersBuilder.with_order(0)
            |> RunnersBuilder.with_depends_on(MapSet.new([])),
          runner_pid_2 =>
            RunnersBuilder.build_incomplete()
            |> RunnersBuilder.with_order(1)
            |> RunnersBuilder.with_depends_on(MapSet.new([]))
        }

        server_state =
          ServerStateBuilder.build()
          |> ServerStateBuilder.with_mode(mode)
          |> ServerStateBuilder.with_runners(runners)

        assert WhichBuildStepsCanRun.determine(server_state) ==
                 MapSet.new([
                   runner_pid_1,
                   runner_pid_2
                 ])
      end
    end

    test "with no runners, returns []" do
      for mode <- [:verbose, :normal] do
        runners = %{}

        server_state =
          ServerStateBuilder.build()
          |> ServerStateBuilder.with_mode(mode)
          |> ServerStateBuilder.with_runners(runners)

        assert WhichBuildStepsCanRun.determine(server_state) == MapSet.new([])
      end
    end

    test "incomplete runners with some dependcies incomplete are not returned" do
      for mode <- [:verbose, :normal] do
        runner_pid_1 = RunnersBuilder.pid()
        runner_pid_2 = RunnersBuilder.pid()
        runner_pid_3 = RunnersBuilder.pid()
        runner_pid_4 = RunnersBuilder.pid()
        runner_pid_5 = RunnersBuilder.pid()

        runners = %{
          runner_pid_1 =>
            RunnersBuilder.build_incomplete()
            |> RunnersBuilder.with_order(0)
            |> RunnersBuilder.with_build_step_name("0")
            |> RunnersBuilder.with_depends_on(MapSet.new([])),
          runner_pid_2 =>
            RunnersBuilder.build_incomplete()
            |> RunnersBuilder.with_order(1)
            |> RunnersBuilder.with_build_step_name("1")
            |> RunnersBuilder.with_depends_on(MapSet.new([])),
          runner_pid_3 =>
            RunnersBuilder.build_incomplete()
            |> RunnersBuilder.with_order(2)
            |> RunnersBuilder.with_build_step_name("2")
            |> RunnersBuilder.with_depends_on(MapSet.new(["1"])),
          runner_pid_4 =>
            RunnersBuilder.build_incomplete()
            |> RunnersBuilder.with_order(3)
            |> RunnersBuilder.with_build_step_name("3")
            |> RunnersBuilder.with_depends_on(MapSet.new(["2"])),
          runner_pid_5 =>
            RunnersBuilder.build_incomplete()
            |> RunnersBuilder.with_order(4)
            |> RunnersBuilder.with_build_step_name("4")
            |> RunnersBuilder.with_depends_on(MapSet.new([]))
        }

        server_state =
          ServerStateBuilder.build()
          |> ServerStateBuilder.with_mode(mode)
          |> ServerStateBuilder.with_runners(runners)

        assert WhichBuildStepsCanRun.determine(server_state) ==
                 MapSet.new([
                   runner_pid_1,
                   runner_pid_2,
                   runner_pid_5
                 ])
      end
    end

    test "incomplete runners with all dependcies complete are not returned" do
      runners = %{
        "fake_pid_A" =>
          RunnersBuilder.build_incomplete()
          |> RunnersBuilder.with_order(0)
          |> RunnersBuilder.with_build_step_name("0")
          |> RunnersBuilder.complete()
          |> RunnersBuilder.with_depends_on(MapSet.new([])),
        "fake_pid_B" =>
          RunnersBuilder.build_incomplete()
          |> RunnersBuilder.with_order(1)
          |> RunnersBuilder.with_build_step_name("1")
          |> RunnersBuilder.complete()
          |> RunnersBuilder.with_depends_on(MapSet.new([])),
        "fake_pid_C" =>
          RunnersBuilder.build_incomplete()
          |> RunnersBuilder.with_order(2)
          |> RunnersBuilder.with_build_step_name("2")
          |> RunnersBuilder.with_depends_on(MapSet.new(["1"])),
        "fake_pid_D" =>
          RunnersBuilder.build_incomplete()
          |> RunnersBuilder.with_order(3)
          |> RunnersBuilder.with_build_step_name("3")
          |> RunnersBuilder.with_depends_on(MapSet.new(["0"])),
        "fake_pid_E" =>
          RunnersBuilder.build_incomplete()
          |> RunnersBuilder.with_order(4)
          |> RunnersBuilder.with_build_step_name("4")
          |> RunnersBuilder.with_depends_on(MapSet.new(["0", "1"])),
        "fake_pid_F" =>
          RunnersBuilder.build_incomplete()
          |> RunnersBuilder.with_order(5)
          |> RunnersBuilder.with_build_step_name("5")
          |> RunnersBuilder.with_depends_on(MapSet.new(["0", "1", "2"]))
      }

      server_state =
        ServerStateBuilder.build()
        |> ServerStateBuilder.with_runners(runners)

      assert WhichBuildStepsCanRun.determine(server_state) ==
               MapSet.new([
                 "fake_pid_C",
                 "fake_pid_D",
                 "fake_pid_E"
               ])
    end
  end

  describe "determine_pids/1 - mode = debug" do
    test "given multiple incomplete runners with no dependencies, returns the one with the lowest order" do
      runners = %{
        "fake_pid_A" =>
          RunnersBuilder.build_incomplete()
          |> RunnersBuilder.with_order(0)
          |> RunnersBuilder.with_depends_on(MapSet.new([])),
        "fake_pid_B" =>
          RunnersBuilder.build_incomplete()
          |> RunnersBuilder.with_order(1)
          |> RunnersBuilder.with_depends_on(MapSet.new([])),
        "fake_pid_C" =>
          RunnersBuilder.build_incomplete()
          |> RunnersBuilder.with_order(2)
          |> RunnersBuilder.with_depends_on(MapSet.new([])),
        "fake_pid_D" =>
          RunnersBuilder.build_incomplete()
          |> RunnersBuilder.with_order(3)
          |> RunnersBuilder.with_depends_on(MapSet.new([])),
        "fake_pid_E" =>
          RunnersBuilder.build_incomplete()
          |> RunnersBuilder.with_order(4)
          |> RunnersBuilder.with_depends_on(MapSet.new([])),
        "fake_pid_F" =>
          RunnersBuilder.build_incomplete()
          |> RunnersBuilder.with_order(5)
          |> RunnersBuilder.with_depends_on(MapSet.new([]))
      }

      server_state =
        ServerStateBuilder.build()
        |> ServerStateBuilder.with_mode(:debug)
        |> ServerStateBuilder.with_runners(runners)

      assert WhichBuildStepsCanRun.determine(server_state) == MapSet.new(["fake_pid_A"])
    end

    test "with no runners, returns []" do
      runners = %{}

      server_state =
        ServerStateBuilder.build()
        |> ServerStateBuilder.with_mode(:debug)
        |> ServerStateBuilder.with_runners(runners)

      assert WhichBuildStepsCanRun.determine(server_state) == MapSet.new([])
    end

    test "incomplete runners with some dependcies incomplete are not returned" do
      runners = %{
        "fake_pid_A" =>
          RunnersBuilder.build_incomplete()
          |> RunnersBuilder.with_order(0)
          |> RunnersBuilder.with_build_step_name("0")
          |> RunnersBuilder.with_depends_on(MapSet.new([])),
        "fake_pid_B" =>
          RunnersBuilder.build_incomplete()
          |> RunnersBuilder.with_order(1)
          |> RunnersBuilder.with_build_step_name("1")
          |> RunnersBuilder.with_depends_on(MapSet.new([])),
        "fake_pid_C" =>
          RunnersBuilder.build_incomplete()
          |> RunnersBuilder.with_order(2)
          |> RunnersBuilder.with_build_step_name("2")
          |> RunnersBuilder.with_depends_on(MapSet.new(["1"])),
        "fake_pid_D" =>
          RunnersBuilder.build_incomplete()
          |> RunnersBuilder.with_order(4)
          |> RunnersBuilder.with_build_step_name("4")
          |> RunnersBuilder.with_depends_on(MapSet.new(["2"])),
        "fake_pid_E" =>
          RunnersBuilder.build_incomplete()
          |> RunnersBuilder.with_order(5)
          |> RunnersBuilder.with_build_step_name("5")
          |> RunnersBuilder.with_depends_on(MapSet.new([]))
      }

      server_state =
        ServerStateBuilder.build()
        |> ServerStateBuilder.with_mode(:debug)
        |> ServerStateBuilder.with_runners(runners)

      assert WhichBuildStepsCanRun.determine(server_state) == MapSet.new(["fake_pid_A"])
    end

    test "given many incomplete runners with all dependencies complete, returns the first by order" do
      runners = %{
        "fake_pid_A" =>
          RunnersBuilder.build_incomplete()
          |> RunnersBuilder.with_order(0)
          |> RunnersBuilder.with_build_step_name("0")
          |> RunnersBuilder.complete()
          |> RunnersBuilder.with_depends_on(MapSet.new([])),
        "fake_pid_B" =>
          RunnersBuilder.build_incomplete()
          |> RunnersBuilder.with_order(1)
          |> RunnersBuilder.with_build_step_name("1")
          |> RunnersBuilder.complete()
          |> RunnersBuilder.with_depends_on(MapSet.new([])),
        "fake_pid_C" =>
          RunnersBuilder.build_incomplete()
          |> RunnersBuilder.with_order(2)
          |> RunnersBuilder.with_build_step_name("2")
          |> RunnersBuilder.with_depends_on(MapSet.new(["1"])),
        "fake_pid_D" =>
          RunnersBuilder.build_incomplete()
          |> RunnersBuilder.with_order(3)
          |> RunnersBuilder.with_build_step_name("3")
          |> RunnersBuilder.with_depends_on(MapSet.new(["0"])),
        "fake_pid_E" =>
          RunnersBuilder.build_incomplete()
          |> RunnersBuilder.with_order(4)
          |> RunnersBuilder.with_build_step_name("4")
          |> RunnersBuilder.with_depends_on(MapSet.new(["1", "2"])),
        "fake_pid_F" =>
          RunnersBuilder.build_incomplete()
          |> RunnersBuilder.with_order(5)
          |> RunnersBuilder.with_build_step_name("5")
          |> RunnersBuilder.with_depends_on(MapSet.new(["1", "2", "3"]))
      }

      server_state =
        ServerStateBuilder.build()
        |> ServerStateBuilder.with_mode(:debug)
        |> ServerStateBuilder.with_runners(runners)

      assert WhichBuildStepsCanRun.determine(server_state) == MapSet.new(["fake_pid_C"])
    end

    test "runners with skip: true are not reurned" do
      for mode <- [:verbose, :normal, :debug] do
        runner_pid_1 = RunnersBuilder.pid()
        runner_pid_2 = RunnersBuilder.pid()
        runner_pid_3 = RunnersBuilder.pid()
        runner_pid_4 = RunnersBuilder.pid()

        runners = %{
          runner_pid_1 =>
            RunnersBuilder.build_incomplete()
            |> RunnersBuilder.with_order(0)
            |> RunnersBuilder.with_status_skip()
            |> RunnersBuilder.with_depends_on(MapSet.new([])),
          runner_pid_2 =>
            RunnersBuilder.build_incomplete()
            |> RunnersBuilder.with_order(1)
            |> RunnersBuilder.with_depends_on(MapSet.new([])),
          runner_pid_3 =>
            RunnersBuilder.build_incomplete()
            |> RunnersBuilder.with_order(3)
            |> RunnersBuilder.with_status_skip()
            |> RunnersBuilder.with_depends_on(MapSet.new([])),
          runner_pid_4 =>
            RunnersBuilder.build_incomplete()
            |> RunnersBuilder.with_order(4)
            |> RunnersBuilder.with_depends_on(MapSet.new([]))
        }

        server_state =
          ServerStateBuilder.build()
          |> ServerStateBuilder.with_mode(mode)
          |> ServerStateBuilder.with_runners(runners)

        runners = WhichBuildStepsCanRun.determine(server_state)
        skipped_runners = MapSet.new([runner_pid_1, runner_pid_3])

        assert MapSet.disjoint?(runners, skipped_runners)
      end
    end

    test "runners which depend on a skipped step can run right away" do
      for mode <- [:verbose, :normal, :debug] do
        runner_pid_1 = RunnersBuilder.pid()
        runner_pid_2 = RunnersBuilder.pid()

        runners = %{
          runner_pid_1 =>
            RunnersBuilder.build_incomplete()
            |> RunnersBuilder.with_order(0)
            |> RunnersBuilder.with_build_step_name("0")
            |> RunnersBuilder.with_status_skip()
            |> RunnersBuilder.with_depends_on(MapSet.new([])),
          runner_pid_2 =>
            RunnersBuilder.build_incomplete()
            |> RunnersBuilder.with_order(1)
            |> RunnersBuilder.with_build_step_name("1")
            |> RunnersBuilder.with_depends_on(MapSet.new(["0"]))
        }

        server_state =
          ServerStateBuilder.build()
          |> ServerStateBuilder.with_mode(mode)
          |> ServerStateBuilder.with_runners(runners)

        runners = WhichBuildStepsCanRun.determine(server_state)

        assert runners == MapSet.new([runner_pid_2])
      end
    end

    test "skipped runners are not returned even if they're incomplete" do
      for mode <- [:verbose, :normal, :debug] do
        runner_pid_1 = RunnersBuilder.pid()
        runner_pid_2 = RunnersBuilder.pid()

        runners = %{
          runner_pid_1 =>
            RunnersBuilder.build_incomplete()
            |> RunnersBuilder.with_order(0)
            |> RunnersBuilder.with_build_step_name("0")
            |> RunnersBuilder.with_status_skip()
            |> RunnersBuilder.with_depends_on(MapSet.new([])),
          runner_pid_2 =>
            RunnersBuilder.build_incomplete()
            |> RunnersBuilder.with_order(1)
            |> RunnersBuilder.with_build_step_name("1")
            |> RunnersBuilder.with_status_skip()
            |> RunnersBuilder.with_depends_on(MapSet.new([]))
        }

        server_state =
          ServerStateBuilder.build()
          |> ServerStateBuilder.with_mode(mode)
          |> ServerStateBuilder.with_runners(runners)

        runners = WhichBuildStepsCanRun.determine(server_state)

        assert runners == MapSet.new([])
      end
    end
  end
end
