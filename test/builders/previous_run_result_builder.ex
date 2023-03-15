defmodule BuildPipeline.Builders.PreviousRunResultBuilder do
  alias Faker.Pokemon
  alias BuildPipeline.Const

  @all_results Const.all_results()

  def build do
    %{
      "buildStepName" => build_step_name(),
      "result" => Enum.random(@all_results)
    }
  end

  def with_build_step_name(build_step, build_step_name) do
    Map.put(build_step, "buildStepName", build_step_name)
  end

  def with_result(build_step, result) do
    Map.put(build_step, "result", result)
  end

  def build_step_name do
    Pokemon.name() <> to_string(positive_number())
  end

  defp positive_number, do: System.unique_integer([:positive])
end
