defmodule BuildPipeline.ServerTest do
  use ExUnit.Case, async: false
  import ExUnit.CaptureIO
  alias BuildPipeline.Server
  alias BuildPipeline.Builders.ServerSetupBuilder

  describe "start_link/2" do
    test "runs build steps that work, then terminates gracefully sending the result" do
      assert {:ok, server_pid} = Server.start_link({working_setup(), self()})

      assert_receive {:server_done,
                      %{
                        result: :success,
                        build_pipeline: [
                          %{
                            build_step_name: "tiresNotSlashed",
                            command: "echo tires",
                            command_env_vars: [],
                            command_type: :shell_command,
                            depends_on: _,
                            exit_code: 0,
                            order: 0,
                            output: "tires\n",
                            status: :complete,
                            duration_in_microseconds: _
                          },
                          %{
                            build_step_name: "enoughFuel",
                            command: "echo fuel",
                            command_env_vars: [],
                            command_type: :shell_command,
                            depends_on: _,
                            exit_code: 0,
                            order: 1,
                            output: "fuel\n",
                            status: :complete,
                            duration_in_microseconds: _
                          },
                          %{
                            build_step_name: "carWorks",
                            command: "echo car works",
                            command_env_vars: [],
                            command_type: :shell_command,
                            depends_on: _,
                            exit_code: 0,
                            order: 2,
                            output: "car works\n",
                            status: :complete,
                            duration_in_microseconds: _
                          },
                          %{
                            build_step_name: "driveToOffice",
                            command: "echo drive",
                            command_env_vars: [],
                            command_type: :shell_command,
                            depends_on: _,
                            exit_code: 0,
                            order: 3,
                            output: "drive\n",
                            status: :complete,
                            duration_in_microseconds: _
                          },
                          %{
                            build_step_name: "approachHuman",
                            command: "echo walk over",
                            command_env_vars: [],
                            command_type: :shell_command,
                            depends_on: _,
                            exit_code: 0,
                            order: 4,
                            output: "walk over\n",
                            status: :complete,
                            duration_in_microseconds: _
                          },
                          %{
                            build_step_name: "sayHello",
                            command: "echo hello",
                            command_env_vars: [],
                            command_type: :shell_command,
                            depends_on: _,
                            exit_code: 0,
                            order: 5,
                            output: "hello\n",
                            status: :complete,
                            duration_in_microseconds: _
                          }
                        ]
                      }},
                     1_000

      refute Process.alive?(server_pid)
    end

    test "runs build steps, returning the result of the first that fails along with those that succeeded" do
      capture_io(fn ->
        assert {:ok, _server_pid} = Server.start_link({failing_setup(), self()})

        assert_receive {:server_done,
                        %{
                          result: :failure,
                          build_pipeline: [
                            %{
                              build_step_name: "tiresNotSlashed",
                              command: "echo tires",
                              command_env_vars: [],
                              command_type: :shell_command,
                              depends_on: _,
                              exit_code: 0,
                              order: 0,
                              output: "tires\n",
                              status: :complete,
                              duration_in_microseconds: _
                            },
                            %{
                              build_step_name: "enoughFuel",
                              command: "echo fuel",
                              command_env_vars: [],
                              command_type: :shell_command,
                              depends_on: _,
                              exit_code: 0,
                              order: 1,
                              output: "fuel\n",
                              status: :complete,
                              duration_in_microseconds: _
                            },
                            %{
                              build_step_name: "carWorks",
                              command: "echo car works",
                              command_env_vars: [],
                              command_type: :shell_command,
                              depends_on: _,
                              exit_code: 0,
                              order: 2,
                              output: "car works\n",
                              status: :complete,
                              duration_in_microseconds: _
                            },
                            %{
                              build_step_name: "driveToOffice",
                              command: "notARealCommand",
                              command_env_vars: [],
                              command_type: :shell_command,
                              depends_on: _,
                              exit_code: 127,
                              order: 3,
                              output: _,
                              status: :complete,
                              duration_in_microseconds: _
                            },
                            %{
                              build_step_name: "approachHuman",
                              command: "echo walk over",
                              command_env_vars: [],
                              command_type: :shell_command,
                              depends_on: _,
                              order: 4,
                              status: :incomplete
                            },
                            %{
                              build_step_name: "sayHello",
                              command: "echo hello",
                              command_env_vars: [],
                              command_type: :shell_command,
                              depends_on: _,
                              order: 5,
                              status: :incomplete
                            }
                          ]
                        }},
                       1_000
      end)
    end

    test "with mode = verbose, returns the output of the commands given" do
      original_env = Application.get_env(:build_pipeline, :print_runner_output)

      Application.put_env(:build_pipeline, :print_runner_output, true)

      build_step = %{
        build_step_name: "echoStuff",
        command: "echo stuff",
        command_env_vars: [],
        command_type: :shell_command,
        depends_on: MapSet.new(),
        order: 0
      }

      server_setup =
        ServerSetupBuilder.build()
        |> ServerSetupBuilder.with_build_pipeline([build_step])
        |> ServerSetupBuilder.with_mode(:verbose)

      output =
        capture_io(fn ->
          assert {:ok, _server_pid} = Server.start_link({server_setup, self()})
          assert_receive {:server_done, _}, 1_000
        end)

      assert output =~ "stuff"
      Application.put_env(:build_pipeline, :print_runner_output, original_env)
    end

    test "with mode = normal, and a step fails, we output its output" do
      original_env = Application.get_env(:build_pipeline, :print_runner_output)

      Application.put_env(:build_pipeline, :print_runner_output, true)

      build_step = %{
        build_step_name: "fail",
        command: "notARealShellCommand",
        command_env_vars: [],
        command_type: :shell_command,
        depends_on: MapSet.new(),
        order: 0
      }

      server_setup =
        ServerSetupBuilder.build()
        |> ServerSetupBuilder.with_build_pipeline([build_step])

      output =
        capture_io(fn ->
          assert {:ok, _server_pid} = Server.start_link({server_setup, self()})
          assert_receive {:server_done, _}, 1_000
        end)

      assert output =~ " notARealShellCommand: not found"
      Application.put_env(:build_pipeline, :print_runner_output, original_env)
    end

    test "with mode = verbose, and a step fails, we output its output" do
      original_env = Application.get_env(:build_pipeline, :print_runner_output)

      Application.put_env(:build_pipeline, :print_runner_output, true)

      build_step = %{
        build_step_name: "fail",
        command: "notARealShellCommand",
        command_env_vars: [],
        command_type: :shell_command,
        depends_on: MapSet.new(),
        order: 0
      }

      server_setup =
        ServerSetupBuilder.build()
        |> ServerSetupBuilder.with_build_pipeline([build_step])
        |> ServerSetupBuilder.with_mode(:verbose)

      output =
        capture_io(fn ->
          assert {:ok, _server_pid} = Server.start_link({server_setup, self()})
          assert_receive {:server_done, _}, 1_000
        end)

      assert output =~ " notARealShellCommand: not found"

      Application.put_env(:build_pipeline, :print_runner_output, original_env)
    end
  end

  defp working_setup do
    build_pipeline = [
      %{
        build_step_name: "tiresNotSlashed",
        command: "echo tires",
        command_env_vars: [],
        command_type: :shell_command,
        depends_on: MapSet.new(),
        order: 0
      },
      %{
        build_step_name: "enoughFuel",
        command: "echo fuel",
        command_env_vars: [],
        command_type: :shell_command,
        depends_on: MapSet.new(),
        order: 1
      },
      %{
        build_step_name: "carWorks",
        command: "echo car works",
        command_env_vars: [],
        command_type: :shell_command,
        depends_on: MapSet.new(["enoughFuel", "tiresNotSlashed"]),
        order: 2
      },
      %{
        build_step_name: "driveToOffice",
        command: "echo drive",
        command_env_vars: [],
        command_type: :shell_command,
        depends_on: MapSet.new(["carWorks"]),
        order: 3
      },
      %{
        build_step_name: "approachHuman",
        command: "echo walk over",
        command_env_vars: [],
        command_type: :shell_command,
        depends_on: MapSet.new(["driveToOffice"]),
        order: 4
      },
      %{
        build_step_name: "sayHello",
        command: "echo hello",
        command_env_vars: [],
        command_type: :shell_command,
        depends_on: MapSet.new(["approachHuman"]),
        order: 5
      }
    ]

    ServerSetupBuilder.build()
    |> ServerSetupBuilder.with_cwd("./example_projects/complex_yet_functioning")
    |> ServerSetupBuilder.with_mode(:normal)
    |> ServerSetupBuilder.with_build_pipeline(build_pipeline)
  end

  defp failing_setup do
    build_pipeline = [
      %{
        build_step_name: "tiresNotSlashed",
        command: "echo tires",
        command_env_vars: [],
        command_type: :shell_command,
        depends_on: MapSet.new(),
        order: 0
      },
      %{
        build_step_name: "enoughFuel",
        command: "echo fuel",
        command_env_vars: [],
        command_type: :shell_command,
        depends_on: MapSet.new(),
        order: 1
      },
      %{
        build_step_name: "carWorks",
        command: "echo car works",
        command_env_vars: [],
        command_type: :shell_command,
        depends_on: MapSet.new(["enoughFuel", "tiresNotSlashed"]),
        order: 2
      },
      %{
        build_step_name: "driveToOffice",
        command: ~s|notARealCommand|,
        command_env_vars: [],
        command_type: :shell_command,
        depends_on: MapSet.new(["carWorks"]),
        order: 3
      },
      %{
        build_step_name: "approachHuman",
        command: "echo walk over",
        command_env_vars: [],
        command_type: :shell_command,
        depends_on: MapSet.new(["driveToOffice"]),
        order: 4
      },
      %{
        build_step_name: "sayHello",
        command: "echo hello",
        command_env_vars: [],
        command_type: :shell_command,
        depends_on: MapSet.new(["approachHuman"]),
        order: 5
      }
    ]

    ServerSetupBuilder.build()
    |> ServerSetupBuilder.with_cwd("./example_projects/complex_yet_functioning")
    |> ServerSetupBuilder.with_mode(:normal)
    |> ServerSetupBuilder.with_build_pipeline(build_pipeline)
  end
end
