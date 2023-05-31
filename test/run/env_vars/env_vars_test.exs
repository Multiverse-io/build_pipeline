defmodule BuildPipeline.Run.EnvVarsTest do
  use ExUnit.Case, async: true
  use Mimic
  alias BuildPipeline.Run.{Const, EnvVars}
  alias BuildPipeline.Run.Support.FetchEnvVarsMock

  @from_failed Const.from_failed_env_var_name()

  describe "read/1" do
    test "when from file is a nonsense value, returns error" do
      FetchEnvVarsMock.setup(from_failed: "something nonsenseical")

      error = """
      The loaded environment variable was
        #{@from_failed}=something nonsenseical
      but I only accept the values
        true
        false
      """

      assert {:error, {:load_env_vars, error}} == EnvVars.read()
    end

    test "when from_failed = false" do
      FetchEnvVarsMock.setup(from_failed: "false")
      assert {:ok, %{run_from_failed: false, save_result: false}} == EnvVars.read()
    end

    test "when from_failed = true" do
      FetchEnvVarsMock.setup(from_failed: "true")
      assert {:ok, %{run_from_failed: true, save_result: true}} == EnvVars.read()
    end

    test "when from_failed = unset" do
      FetchEnvVarsMock.setup(from_failed: nil)
      assert {:ok, %{}} == EnvVars.read()
    end
  end
end
