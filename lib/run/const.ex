defmodule BuildPipeline.Run.Const do
  def successful, do: "successful"
  def failed, do: "failed"
  def not_started, do: "not started"
  def skipped, do: "skipped"

  def save_result_env_var_name, do: "BUILD_PIPELINE_SAVE_RESULT"

  def all_results, do: [successful(), failed(), not_started(), skipped()]
  def skipable_results, do: [successful(), skipped()]
  def unskipable_results, do: [failed(), not_started()]
end
