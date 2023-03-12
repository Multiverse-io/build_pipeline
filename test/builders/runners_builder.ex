defmodule BuildPipeline.Builders.RunnersBuilder do
  alias Faker.Pokemon

  def build_many do
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

  def build_complete do
    incomplete = build_incomplete()
    %{build_step_name: build_step_name} = incomplete

    incomplete
    |> with_exit_code(0)
    |> with_output("#{build_step_name}\n")
    |> with_duration_in_microseconds(1557)
  end

  def build_incomplete do
    build_step_name = random_pokemon()

    %{
      build_step_name: build_step_name,
      command: "echo #{build_step_name}",
      command_env_vars: [],
      command_type: :shell_command,
      depends_on: MapSet.new([]),
      order: 0,
      output: "#{build_step_name}\n",
      status: :complete,
      terminal_line_number: 1
    }
  end

  def pid, do: "#FAKE_PID<0.#{positive_number()}.0>"

  def with_order(runner, order) do
    runner
    |> Map.put(:order, order)
    |> Map.put(:terminal_line_number, order + 1)
  end

  def with_exit_code(runner, exit_code) do
    Map.put(runner, :exit_code, exit_code)
  end

  def with_output(runner, output) do
    Map.put(runner, :output, output)
  end

  def with_command(runner, command) do
    Map.put(runner, :command, command)
  end

  def with_build_step_name(runner, build_step_name) do
    Map.put(runner, :build_step_name, build_step_name)
  end

  def with_duration_in_microseconds(runner, duration_in_microseconds) do
    Map.put(runner, :duration_in_microseconds, duration_in_microseconds)
  end

  defp random_pokemon do
    Pokemon.name() <> to_string(positive_number())
  end

  defp positive_number, do: System.unique_integer([:positive])
end
