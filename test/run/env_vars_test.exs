defmodule BuildPipeline.Run.EnvVarsTest do
  use ExUnit.Case, async: true
  use Mimic
  alias BuildPipeline.Run.{Const, EnvVars}

  @save_result_env_var_name Const.save_result_env_var_name()

  describe "put_config/1" do
    test "when the env var is unset, return the same setup given" do
      Mimic.copy(System)
      Mimic.stub(System, :get_env, fn @save_result_env_var_name -> nil end)

      setup = %{}
      assert {:ok, setup} == EnvVars.put_config(setup)
    end

    test "when the env var is set as true, return the setup with save_result: true in it" do
      Mimic.copy(System)
      Mimic.stub(System, :get_env, fn @save_result_env_var_name -> "true" end)

      assert {:ok, %{save_result: true, a: 1}} == EnvVars.put_config(%{a: 1})
    end

    test "when the env var is set as false, return the setup with save_result: false in it" do
      Mimic.copy(System)
      Mimic.stub(System, :get_env, fn @save_result_env_var_name -> "false" end)

      assert {:ok, %{save_result: false}} = EnvVars.put_config(%{})
    end

    test "when the given setup already has save_result set and the env var is set as <anything valid>, ignore the env var and leave the setup as it was" do
      Mimic.copy(System)
      Mimic.stub(System, :get_env, fn @save_result_env_var_name -> "false" end)

      setup = %{save_result: true, a: 1}
      assert {:ok, setup} == EnvVars.put_config(setup)
    end

    test "when the given setup already has save_result set to false and the env var is set as true, then set it to true" do
      Mimic.copy(System)
      Mimic.stub(System, :get_env, fn @save_result_env_var_name -> "true" end)

      assert {:ok, %{save_result: true, a: 1}} == EnvVars.put_config(%{save_result: false, a: 1})
    end

    test "when the env var is set to something nonsenseical, return error" do
      Mimic.copy(System)
      Mimic.stub(System, :get_env, fn @save_result_env_var_name -> "something nonsenseical" end)

      error = """
      The loaded environment variable was
        #{@save_result_env_var_name}=something nonsenseical
      but I only accept the values
        true
        false
      """

      assert {:error, {:load_env_vars, error}} == EnvVars.put_config(%{})
    end
  end
end
