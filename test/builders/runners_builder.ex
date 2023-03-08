defmodule BuildPipeline.Builders.RunnersBuilder do
  def build do
    %{
      "fake_pid_1" => %{
        build_step_name: "tiresNotSlashed",
        command: "echo tires",
        command_env_vars: [],
        command_type: :shell_command,
        depends_on: MapSet.new([]),
        order: 0,
        status: :incomplete,
        terminal_line_number: 1
      },
      "fake_pid_2" => %{
        build_step_name: "enoughFuel",
        command: "echo fuel",
        command_env_vars: [],
        command_type: :shell_command,
        depends_on: MapSet.new([]),
        order: 1,
        status: :incomplete,
        terminal_line_number: 2
      },
      "fake_pid_3" => %{
        build_step_name: "carWorks",
        command: "echo car works",
        command_env_vars: [],
        command_type: :shell_command,
        depends_on: MapSet.new(["enoughFuel", "tiresNotSlashed"]),
        order: 2,
        status: :incomplete,
        terminal_line_number: 3
      },
      "fake_pid_4" => %{
        build_step_name: "driveToOffice",
        command: "echo drive",
        command_env_vars: [],
        command_type: :shell_command,
        depends_on: MapSet.new(["carWorks"]),
        order: 3,
        status: :incomplete,
        terminal_line_number: 4
      },
      "fake_pid_5" => %{
        build_step_name: "approachHuman",
        command: "echo walk over",
        command_env_vars: [],
        command_type: :shell_command,
        depends_on: MapSet.new(["driveToOffice"]),
        order: 4,
        status: :incomplete,
        terminal_line_number: 5
      },
      "fake_pid_6" => %{
        build_step_name: "sayHello",
        command: "echo hello",
        command_env_vars: [],
        command_type: :shell_command,
        depends_on: MapSet.new(["approachHuman"]),
        order: 5,
        status: :incomplete,
        terminal_line_number: 6
      }
    }
  end
end
