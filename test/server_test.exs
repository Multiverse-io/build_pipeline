defmodule BuildPipeline.ServerTest do
  use ExUnit.Case, async: true
  alias BuildPipeline.Server

  describe "start_link/2" do
    test "runs build steps that work, then terminates gracefully sending the result" do
      assert {:ok, server_pid} = Server.start_link({working_setup(), self()})

      result = working_setup_result()

      assert_receive {:server_done, ^result}, 1_000

      refute Process.alive?(server_pid)
    end

    test "runs build steps, returning the result of the first that fails along with those that succeeded" do
      assert {:ok, server_pid} = Server.start_link({failing_setup(), self()})

      result = failed_setup_result()

      assert_receive {:server_done, ^result}, 1_000

      refute Process.alive?(server_pid)
    end
  end

  defp working_setup do
    %{
      build_pipeline: [
        %{
          build_step_name: "tiresNotSlashed",
          command: "echo tires",
          command_type: :shell_command,
          depends_on: MapSet.new(),
          order: 0
        },
        %{
          build_step_name: "enoughFuel",
          command: "echo fuel",
          command_type: :shell_command,
          depends_on: MapSet.new(),
          order: 1
        },
        %{
          build_step_name: "carWorks",
          command: "echo car works",
          command_type: :shell_command,
          depends_on: MapSet.new(["enoughFuel", "tiresNotSlashed"]),
          order: 2
        },
        %{
          build_step_name: "driveToOffice",
          command: "echo drive",
          command_type: :shell_command,
          depends_on: MapSet.new(["carWorks"]),
          order: 3
        },
        %{
          build_step_name: "approachHuman",
          command: "echo walk over",
          command_type: :shell_command,
          depends_on: MapSet.new(["driveToOffice"]),
          order: 4
        },
        %{
          build_step_name: "sayHello",
          command: "echo hello",
          command_type: :shell_command,
          depends_on: MapSet.new(["approachHuman"]),
          order: 5
        }
      ],
      setup: %{
        cwd: "./test/example_projects/complex_yet_functioning",
        print_cmd_output: false
      }
    }
  end

  defp failed_setup_result do
    %{
      result: :failure,
      build_pipeline: [
        %{
          build_step_name: "tiresNotSlashed",
          command: "echo tires",
          command_type: :shell_command,
          depends_on: MapSet.new([]),
          exit_code: 0,
          order: 0,
          output: "tires\n",
          status: :complete
        },
        %{
          build_step_name: "enoughFuel",
          command: "echo fuel",
          command_type: :shell_command,
          depends_on: MapSet.new([]),
          exit_code: 0,
          order: 1,
          output: "fuel\n",
          status: :complete
        },
        %{
          build_step_name: "carWorks",
          command: "echo car works",
          command_type: :shell_command,
          depends_on: MapSet.new(["enoughFuel", "tiresNotSlashed"]),
          exit_code: 0,
          order: 2,
          output: "car works\n",
          status: :complete
        },
        %{
          build_step_name: "driveToOffice",
          command: "notARealCommand",
          command_type: :shell_command,
          depends_on: MapSet.new(["carWorks"]),
          exit_code: 127,
          order: 3,
          output: "sh: 1: exec: notARealCommand: not found\n",
          status: :complete
        },
        %{
          build_step_name: "approachHuman",
          command: "echo walk over",
          command_type: :shell_command,
          depends_on: MapSet.new(["driveToOffice"]),
          order: 4,
          status: :incomplete
        },
        %{
          build_step_name: "sayHello",
          command: "echo hello",
          command_type: :shell_command,
          depends_on: MapSet.new(["approachHuman"]),
          order: 5,
          status: :incomplete
        }
      ]
    }
  end

  defp working_setup_result do
    %{
      result: :success,
      build_pipeline: [
        %{
          build_step_name: "tiresNotSlashed",
          command: "echo tires",
          command_type: :shell_command,
          depends_on: MapSet.new([]),
          exit_code: 0,
          order: 0,
          output: "tires\n",
          status: :complete
        },
        %{
          build_step_name: "enoughFuel",
          command: "echo fuel",
          command_type: :shell_command,
          depends_on: MapSet.new([]),
          exit_code: 0,
          order: 1,
          output: "fuel\n",
          status: :complete
        },
        %{
          build_step_name: "carWorks",
          command: "echo car works",
          command_type: :shell_command,
          depends_on: MapSet.new(["enoughFuel", "tiresNotSlashed"]),
          exit_code: 0,
          order: 2,
          output: "car works\n",
          status: :complete
        },
        %{
          build_step_name: "driveToOffice",
          command: "echo drive",
          command_type: :shell_command,
          depends_on: MapSet.new(["carWorks"]),
          exit_code: 0,
          order: 3,
          output: "drive\n",
          status: :complete
        },
        %{
          build_step_name: "approachHuman",
          command: "echo walk over",
          command_type: :shell_command,
          depends_on: MapSet.new(["driveToOffice"]),
          exit_code: 0,
          order: 4,
          output: "walk over\n",
          status: :complete
        },
        %{
          build_step_name: "sayHello",
          command: "echo hello",
          command_type: :shell_command,
          depends_on: MapSet.new(["approachHuman"]),
          exit_code: 0,
          order: 5,
          output: "hello\n",
          status: :complete
        }
      ]
    }
  end

  defp failing_setup do
    %{
      build_pipeline: [
        %{
          build_step_name: "tiresNotSlashed",
          command: "echo tires",
          command_type: :shell_command,
          depends_on: MapSet.new(),
          order: 0
        },
        %{
          build_step_name: "enoughFuel",
          command: "echo fuel",
          command_type: :shell_command,
          depends_on: MapSet.new(),
          order: 1
        },
        %{
          build_step_name: "carWorks",
          command: "echo car works",
          command_type: :shell_command,
          depends_on: MapSet.new(["enoughFuel", "tiresNotSlashed"]),
          order: 2
        },
        %{
          build_step_name: "driveToOffice",
          command: ~s|notARealCommand|,
          command_type: :shell_command,
          depends_on: MapSet.new(["carWorks"]),
          order: 3
        },
        %{
          build_step_name: "approachHuman",
          command: "echo walk over",
          command_type: :shell_command,
          depends_on: MapSet.new(["driveToOffice"]),
          order: 4
        },
        %{
          build_step_name: "sayHello",
          command: "echo hello",
          command_type: :shell_command,
          depends_on: MapSet.new(["approachHuman"]),
          order: 5
        }
      ],
      setup: %{
        cwd: "./test/example_projects/complex_yet_functioning",
        print_cmd_output: false
      }
    }
  end
end