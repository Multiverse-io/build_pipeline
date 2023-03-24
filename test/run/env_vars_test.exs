defmodule BuildPipeline.Run.EnvVarsTest do
  use ExUnit.Case, async: true
  use Mimic
  alias BuildPipeline.Run.{Const, EnvVars}
  alias BuildPipeline.Run.Support.EnvVarsSystemMock

  @save_result Const.save_result_env_var_name()
  @from_failed Const.from_failed_env_var_name()

  describe "read/1" do
    test "when save result is a nonsense value, returns error" do
      EnvVarsSystemMock.setup(save_result: "something nonsenseical")

      error = """
      The loaded environment variable was
        #{@save_result}=something nonsenseical
      but I only accept the values
        true
        false
      """

      assert {:error, {:load_env_vars, error}} == EnvVars.read()
    end

    test "when from file is a nonsense value, returns error" do
      EnvVarsSystemMock.setup(from_failed: "something nonsenseical")

      error = """
      The loaded environment variable was
        #{@from_failed}=something nonsenseical
      but I only accept the values
        true
        false
      """

      assert {:error, {:load_env_vars, error}} == EnvVars.read()
    end
  end

  describe "read/1 - with from failed = true" do
    test "when save_result = unset" do
      EnvVarsSystemMock.setup(from_failed: "true", save_result: nil)
      assert {:ok, %{run_from_failed: true, save_result: true}} == EnvVars.read()
    end

    test "when save_result = false" do
      EnvVarsSystemMock.setup(from_failed: "true", save_result: "false")
      assert {:ok, %{run_from_failed: true, save_result: true}} == EnvVars.read()
    end

    test "when save_result = true" do
      EnvVarsSystemMock.setup(from_failed: "true", save_result: "true")
      assert {:ok, %{run_from_failed: true, save_result: true}} == EnvVars.read()
    end
  end

  describe "read/1 - with from failed = false" do
    test "when from_failed = false and save_result = unset" do
      EnvVarsSystemMock.setup(from_failed: "false", save_result: nil)
      assert {:ok, %{run_from_failed: false, save_result: false}} == EnvVars.read()
    end

    test "when from_failed = false and save_result = false" do
      EnvVarsSystemMock.setup(from_failed: "false", save_result: "false")
      assert {:ok, %{run_from_failed: false, save_result: false}} == EnvVars.read()
    end

    test "when from_failed = false and save_result = true" do
      EnvVarsSystemMock.setup(from_failed: "false", save_result: "true")
      assert {:ok, %{run_from_failed: false, save_result: true}} == EnvVars.read()
    end
  end

  describe "read/1 - with from failed = unset" do
    test "when from_failed = unset and save_result = unset" do
      EnvVarsSystemMock.setup(from_failed: nil, save_result: nil)
      assert {:ok, %{}} == EnvVars.read()
    end

    test "when from_failed = unset and save_result = false" do
      EnvVarsSystemMock.setup(from_failed: nil, save_result: "false")
      assert {:ok, %{save_result: false}} == EnvVars.read()
    end

    test "when from_failed = unset and save_result = true" do
      EnvVarsSystemMock.setup(from_failed: nil, save_result: "true")
      assert {:ok, %{save_result: true}} == EnvVars.read()
    end
  end
end
