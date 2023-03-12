defmodule BuildPipeline.WhichBuildStepsCanRunTest do
  use ExUnit.Case, async: true
  alias BuildPipeline.WhichBuildStepsCanRun
  alias BuildPipeline.Builders.ServerStateBuilder

  describe "determine_pids/1 - mode = normal / verbose" do
    test "incomplete runners with no dependencies are returned" do
      for mode <- [:verbose, :normal] do
        runners = %{
          "fake_pid_1" => %{
            build_step_name: "1",
            command: "echo tires",
            command_env_vars: [],
            command_type: :shell_command,
            depends_on: MapSet.new([]),
            order: 0,
            status: :incomplete,
            terminal_line_number: 1
          },
          "fake_pid_2" => %{
            build_step_name: "2",
            command: "echo tires",
            command_env_vars: [],
            command_type: :shell_command,
            depends_on: MapSet.new([]),
            order: 1,
            status: :incomplete,
            terminal_line_number: 2
          }
        }

        server_state =
          ServerStateBuilder.build()
          |> ServerStateBuilder.with_mode(mode)
          |> ServerStateBuilder.with_runners(runners)

        assert WhichBuildStepsCanRun.determine(server_state) == [
                 "fake_pid_1",
                 "fake_pid_2"
               ]
      end
    end

    test "with no runners, returns []" do
      for mode <- [:verbose, :normal] do
        runners = %{}

        server_state =
          ServerStateBuilder.build()
          |> ServerStateBuilder.with_mode(mode)
          |> ServerStateBuilder.with_runners(runners)

        assert WhichBuildStepsCanRun.determine(server_state) == []
      end
    end

    test "incomplete runners with some dependcies incomplete are not returned" do
      for mode <- [:verbose, :normal] do
        runners = %{
          "fake_pid_A" => %{
            build_step_name: "A",
            command: "echo A",
            command_env_vars: [],
            command_type: :shell_command,
            depends_on: MapSet.new([]),
            order: 0,
            status: :incomplete,
            terminal_line_number: 1
          },
          "fake_pid_B" => %{
            build_step_name: "B",
            command: "echo B",
            command_env_vars: [],
            command_type: :shell_command,
            depends_on: MapSet.new([]),
            order: 1,
            status: :incomplete,
            terminal_line_number: 2
          },
          "fake_pid_C" => %{
            build_step_name: "C",
            command: "echo C",
            command_env_vars: [],
            command_type: :shell_command,
            depends_on: MapSet.new(["B"]),
            order: 2,
            status: :incomplete,
            terminal_line_number: 3
          },
          "fake_pid_D" => %{
            build_step_name: "D",
            command: "echo D",
            command_env_vars: [],
            command_type: :shell_command,
            depends_on: MapSet.new(["C"]),
            order: 3,
            status: :incomplete,
            terminal_line_number: 4
          },
          "fake_pid_E" => %{
            build_step_name: "E",
            command: "echo E",
            command_env_vars: [],
            command_type: :shell_command,
            depends_on: MapSet.new([]),
            order: 4,
            status: :incomplete,
            terminal_line_number: 5
          }
        }

        server_state =
          ServerStateBuilder.build()
          |> ServerStateBuilder.with_mode(mode)
          |> ServerStateBuilder.with_runners(runners)

        assert WhichBuildStepsCanRun.determine(server_state) == [
                 "fake_pid_A",
                 "fake_pid_B",
                 "fake_pid_E"
               ]
      end
    end

    test "incomplete runners with all dependcies complete are not returned" do
      runners = %{
        "fake_pid_A" => %{
          build_step_name: "A",
          command: "echo A",
          command_env_vars: [],
          command_type: :shell_command,
          depends_on: MapSet.new([]),
          order: 0,
          status: :complete,
          terminal_line_number: 1
        },
        "fake_pid_B" => %{
          build_step_name: "B",
          command: "echo B",
          command_env_vars: [],
          command_type: :shell_command,
          depends_on: MapSet.new([]),
          order: 1,
          status: :complete,
          terminal_line_number: 2
        },
        "fake_pid_C" => %{
          build_step_name: "C",
          command: "echo C",
          command_env_vars: [],
          command_type: :shell_command,
          depends_on: MapSet.new(["B"]),
          order: 2,
          status: :incomplete,
          terminal_line_number: 3
        },
        "fake_pid_D" => %{
          build_step_name: "D",
          command: "echo D",
          command_env_vars: [],
          command_type: :shell_command,
          depends_on: MapSet.new(["A"]),
          order: 3,
          status: :incomplete,
          terminal_line_number: 4
        },
        "fake_pid_E" => %{
          build_step_name: "E",
          command: "echo E",
          command_env_vars: [],
          command_type: :shell_command,
          depends_on: MapSet.new(["A", "B"]),
          order: 4,
          status: :incomplete,
          terminal_line_number: 5
        },
        "fake_pid_F" => %{
          build_step_name: "F",
          command: "echo F",
          command_env_vars: [],
          command_type: :shell_command,
          depends_on: MapSet.new(["A", "B", "C"]),
          order: 5,
          status: :incomplete,
          terminal_line_number: 6
        }
      }

      server_state =
        ServerStateBuilder.build()
        |> ServerStateBuilder.with_runners(runners)

      assert WhichBuildStepsCanRun.determine(server_state) == [
               "fake_pid_C",
               "fake_pid_D",
               "fake_pid_E"
             ]
    end
  end

  describe "determine_pids/1 - mode = debug" do
    test "given multiple incomplete runners with no dependencies, returns the one with the highest order" do
      runners = %{
        "fake_pid_A" => %{
          build_step_name: "A",
          command: "echo A",
          command_env_vars: [],
          command_type: :shell_command,
          depends_on: MapSet.new([]),
          order: 5,
          status: :incomplete,
          terminal_line_number: 6
        },
        "fake_pid_B" => %{
          build_step_name: "B",
          command: "echo B",
          command_env_vars: [],
          command_type: :shell_command,
          depends_on: MapSet.new([]),
          order: 1,
          status: :incomplete,
          terminal_line_number: 2
        },
        "fake_pid_C" => %{
          build_step_name: "C",
          command: "echo C",
          command_env_vars: [],
          command_type: :shell_command,
          depends_on: MapSet.new([]),
          order: 2,
          status: :incomplete,
          terminal_line_number: 3
        },
        "fake_pid_D" => %{
          build_step_name: "D",
          command: "echo D",
          command_env_vars: [],
          command_type: :shell_command,
          depends_on: MapSet.new([]),
          order: 3,
          status: :incomplete,
          terminal_line_number: 4
        },
        "fake_pid_E" => %{
          build_step_name: "E",
          command: "echo E",
          command_env_vars: [],
          command_type: :shell_command,
          depends_on: MapSet.new([]),
          order: 4,
          status: :incomplete,
          terminal_line_number: 5
        },
        "fake_pid_F" => %{
          build_step_name: "F",
          command: "echo F",
          command_env_vars: [],
          command_type: :shell_command,
          depends_on: MapSet.new([]),
          order: 0,
          status: :incomplete,
          terminal_line_number: 1
        }
      }

      server_state =
        ServerStateBuilder.build()
        |> ServerStateBuilder.with_mode(:debug)
        |> ServerStateBuilder.with_runners(runners)

      assert WhichBuildStepsCanRun.determine(server_state) == [
               "fake_pid_F"
             ]
    end

    test "with no runners, returns []" do
      runners = %{}

      server_state =
        ServerStateBuilder.build()
        |> ServerStateBuilder.with_mode(:debug)
        |> ServerStateBuilder.with_runners(runners)

      assert WhichBuildStepsCanRun.determine(server_state) == []
    end

    test "incomplete runners with some dependcies incomplete are not returned" do
      runners = %{
        "fake_pid_A" => %{
          build_step_name: "A",
          command: "echo A",
          command_env_vars: [],
          command_type: :shell_command,
          depends_on: MapSet.new([]),
          order: 0,
          status: :incomplete,
          terminal_line_number: 1
        },
        "fake_pid_B" => %{
          build_step_name: "B",
          command: "echo B",
          command_env_vars: [],
          command_type: :shell_command,
          depends_on: MapSet.new([]),
          order: 1,
          status: :incomplete,
          terminal_line_number: 2
        },
        "fake_pid_C" => %{
          build_step_name: "C",
          command: "echo C",
          command_env_vars: [],
          command_type: :shell_command,
          depends_on: MapSet.new(["B"]),
          order: 2,
          status: :incomplete,
          terminal_line_number: 3
        },
        "fake_pid_D" => %{
          build_step_name: "D",
          command: "echo D",
          command_env_vars: [],
          command_type: :shell_command,
          depends_on: MapSet.new(["C"]),
          order: 3,
          status: :incomplete,
          terminal_line_number: 4
        },
        "fake_pid_E" => %{
          build_step_name: "E",
          command: "echo E",
          command_env_vars: [],
          command_type: :shell_command,
          depends_on: MapSet.new([]),
          order: 4,
          status: :incomplete,
          terminal_line_number: 5
        }
      }

      server_state =
        ServerStateBuilder.build()
        |> ServerStateBuilder.with_mode(:debug)
        |> ServerStateBuilder.with_runners(runners)

      assert WhichBuildStepsCanRun.determine(server_state) == [
               "fake_pid_A"
             ]
    end

    test "given many incomplete runners with all dependencies complete, returns the first by order" do
      runners = %{
        "fake_pid_A" => %{
          build_step_name: "A",
          command: "echo A",
          command_env_vars: [],
          command_type: :shell_command,
          depends_on: MapSet.new([]),
          order: 0,
          status: :complete,
          terminal_line_number: 1
        },
        "fake_pid_B" => %{
          build_step_name: "B",
          command: "echo B",
          command_env_vars: [],
          command_type: :shell_command,
          depends_on: MapSet.new([]),
          order: 1,
          status: :complete,
          terminal_line_number: 2
        },
        "fake_pid_C" => %{
          build_step_name: "C",
          command: "echo C",
          command_env_vars: [],
          command_type: :shell_command,
          depends_on: MapSet.new(["B"]),
          order: 2,
          status: :incomplete,
          terminal_line_number: 3
        },
        "fake_pid_D" => %{
          build_step_name: "D",
          command: "echo D",
          command_env_vars: [],
          command_type: :shell_command,
          depends_on: MapSet.new(["A"]),
          order: 3,
          status: :incomplete,
          terminal_line_number: 4
        },
        "fake_pid_E" => %{
          build_step_name: "E",
          command: "echo E",
          command_env_vars: [],
          command_type: :shell_command,
          depends_on: MapSet.new(["A", "B"]),
          order: 4,
          status: :incomplete,
          terminal_line_number: 5
        },
        "fake_pid_F" => %{
          build_step_name: "F",
          command: "echo F",
          command_env_vars: [],
          command_type: :shell_command,
          depends_on: MapSet.new(["A", "B", "C"]),
          order: 5,
          status: :incomplete,
          terminal_line_number: 6
        }
      }

      server_state =
        ServerStateBuilder.build()
        |> ServerStateBuilder.with_mode(:debug)
        |> ServerStateBuilder.with_runners(runners)

      assert WhichBuildStepsCanRun.determine(server_state) == [
               "fake_pid_C"
             ]
    end
  end
end
